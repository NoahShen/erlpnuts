-module(storageunit).

-author('Noah.Shen87@gmail.com').

-include("storageunit.hrl").

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

	
%% @spec start_link() -> ServerRet
start_link(State) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, State).


init(State) ->
	Options = storageunit_options:parse_dboptions(State),
	io:format("ParseResult ~p~n",[Options]),
    application:start(emongo),
	
	emongo:add_pool(Options#dbOptions.poolName, 
					Options#dbOptions.host, 
					Options#dbOptions.port, 
					Options#dbOptions.collection, 
					Options#dbOptions.poolSize),
	
    {ok, State}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.