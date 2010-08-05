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
			{ok, []};
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
			Func0 = "function(obj,out) { if (obj.fieldid == \"",
			Func1 = string:concat(Func0, Key),
			Func2 = string:concat(Func1, "\" && obj.fieldversion >= out.fieldversion && obj.fieldversion <= "),
			Func3 = string:concat(Func2, integer_to_list(Rev)),
			Func4 = string:concat(Func3, ") {out.fieldversion = obj.fieldversion; out.fieldvalue = obj.fieldvalue; out.tablename = obj.tablename; out.fieldid = obj.fieldid} }"),
%% 			io:format("~p~n", [Func4]),
			GroupRes = Mong:group("rev", 
								[{"fieldname", 1}],
								{code, 
									Func4, 
									[]},
								[{"fieldversion", 0}],
					   		[]),
			case GroupRes of
				[{<<"retval">>, {array, ResultArr}}, _, _, _] ->
					case ResultArr of 
						[] ->
							{ok, []};
						_ ->
							T = parseRecord(ResultArr),
							
							case T of
								[] ->
									{ok, []};
								_ ->
									{ok, [{<<"key">>, list_to_binary(Key)} | T]}
							end
					end;
				_ ->
					{ok, []}
			end;
		{ok, _} ->
			Result
	end.

parseRecord([]) ->
	[];

parseRecord([R | T]) ->
	FieldName = proplists:get_value(<<"fieldname">>, R),
	FieldValue = proplists:get_value(<<"fieldvalue">>, R),
	
	case FieldValue of
		undefined ->
			parseRecord(T);
		_ ->
			[{FieldName, FieldValue} | parseRecord(T)]
	end.
	