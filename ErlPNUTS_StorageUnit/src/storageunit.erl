-module(storageunit).

-author('Noah.Shen87@gmail.com').

-include("storageunit.hrl").

-record(state,
        {mongodb,
		 dboptions=#dbOptions{}}).

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get/2, get/3]).

%% @spec start_link() -> ServerRet
start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).


init(State) ->
	{ok, Options} = storageunit_options:parse_dboptions(State),
%%     application:start(emongo),
	application:start(erlmongo),
	
%% 	emongo:add_pool(Options#dbOptions.poolName, 
%% 					Options#dbOptions.host, 
%% 					Options#dbOptions.port, 
%% 					Options#dbOptions.database,
%% 					Options#dbOptions.poolSize),
	
	mongodb:singleServer(Options#dbOptions.poolName, string:concat(string:concat(Options#dbOptions.host, ":"), integer_to_list(Options#dbOptions.port))),
	mongodb:connect(Options#dbOptions.poolName),
	
	Database = Options#dbOptions.database,
	Mong = mongoapi:new(Options#dbOptions.poolName, list_to_binary(Database)),
	
	process_flag(trap_exit, true),
    {ok, #state{mongodb = Mong, dboptions = Options}}.

handle_call(_Request, _From, State) ->
	Reply = case _Request of
		{get, Collection, Key, Opts} ->
			do_Get(Collection, Key, Opts, State);
		_ ->
			{error, "Bad request"}
	end,

	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
%% 	emongo:del_pool(State#dbOptions.poolName),
	DbOpts = State#state.dboptions,
	mongodb:deleteConnection(DbOpts#dbOptions.poolName),
	ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get(Collection, Key) -> 
	get(Collection, Key, []).

get(Collection, Key, Opts) ->
	gen_server:call(?MODULE, {get, Collection, Key, Opts}).

do_Get(Collection, Key, Opts, State) ->
	Mong = State#state.mongodb,
	case Opts of
		last_revision ->
			%%TODO
			ok;
		{revision, Rev} ->
			getByRev(Mong, Collection, Key, Rev);
		_ ->
			Mong:findOne(Collection, [{"key", Key}])
%% 			emongo:find_all(PoolName, Collection, [{"key", Key}])
		
	end.

getByRev(Mong, Collection, Key, Rev) ->
%% 	Result = emongo:find_all(PoolName, Collection, [{"key", Key}, {"rev", Rev}]),
	Result = Mong:findOne(Collection, [{"key", Key}, {"rev", Rev}]),
	case Result of
		{ok, []} ->
%% 			emongo:find_all(PoolName, "Revision_Collection", [{"key", Key}, {"rev", Rev}]),
			Func1 = "function(obj,out) { if (obj.fieldversion >= out.rev && obj.fieldversion <= ",
			Func2 = ") {out.fieldversion = obj.fieldversion; out.fieldvalue = obj.fieldvalue;} }",
			ReduceFunc = string:concat(string:concat(Func1, integer_to_list(Rev)), Func2),
			GroupRes = Mong:group("rev", 
								[{"key", Key}],
								{code, 
									ReduceFunc, 
									[]},
								[{"rev", 0}],
					   		[]);
			
		{ok, [_]} ->
			Result
	end.
	