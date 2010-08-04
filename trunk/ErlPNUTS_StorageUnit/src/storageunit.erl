-module(storageunit).

-author('Noah.Shen87@gmail.com').

-include("storageunit.hrl").

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
    application:start(emongo),
	
	emongo:add_pool(Options#dbOptions.poolName, 
					Options#dbOptions.host, 
					Options#dbOptions.port, 
					Options#dbOptions.database,
					Options#dbOptions.poolSize),
	process_flag(trap_exit, true),
    {ok, Options}.

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
	emongo:del_pool(State#dbOptions.poolName),
	ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get(Collection, Key) -> 
	get(Collection, Key, []).

get(Collection, Key, Opts) ->
	gen_server:call(?MODULE, {get, Collection, Key, Opts}).

do_Get(Collection, Key, Opts, State) ->
	PoolName = State#dbOptions.poolName,
	case Opts of
		last_revision ->
			%%TODO
			ok;
		{revision, Rev} ->
			getByRev(PoolName, Collection, Key, Rev);
		_ ->
			emongo:find_all(PoolName, Collection, [{"key", Key}])
		
	end.

getByRev(PoolName, Collection, Key, Rev) ->
	Data = emongo:find_all(PoolName, Collection, [{"key", Key}, {"rev", Rev}]),
	case Data of
		[] ->
			Data2 = emongo:find_all(PoolName, "Revision_Collection", [{"key", Key}, {"rev", Rev}]),
			
			ok;
		_ ->
			Data
	end.
	