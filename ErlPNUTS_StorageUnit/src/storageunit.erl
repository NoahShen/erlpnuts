-module(storageunit).

-author('Noah.Shen87@gmail.com').

-include("storageunit.hrl").

-record(state,
        {mongodb,
		 dboptions=#dbOptions{}}).

-record(result,
        {revCorrect = false,
		fields = []}).

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
	
	application:start(erlmongo),
	
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
		{put, Collection, Key, Json, Opts} ->
			do_Put(Collection, Key, Json, Opts, State);
		_ ->
			{error, "Bad request"}
	end,

	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
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
			Mong:findOne(Collection, [{"_key", Key}])		
	end.

getByRev(Mong, Collection, Key, Rev) ->
	Result = Mong:findOne(Collection, [{"_key", Key}, {"rev", Rev}]),
	case Result of
		{ok, []} ->

			Func4 = io_lib:format("function(obj,out) { if (obj.fieldrevision >= out.fieldrevision && obj.fieldrevision <= ~s) {out.fieldrevision = obj.fieldrevision; out.fieldvalue = obj.fieldvalue; out.collection = obj.collection; out.rev_id = obj.rev_id;} }", 
								  [integer_to_list(Rev)]),
			GroupRes = Mong:group("rev", 
								[{"fieldname", 1}],
								{code, 
									Func4, 
									[]},
								[{"fieldrevision", 0}],
					   			[{"cond", [{"collection", Collection}, {"rev_id", Key}]}]
							),
			case GroupRes of
				[{<<"retval">>, {array, ResultArr}}, _, _, _] ->
					case ResultArr of 
						[] ->
							{ok, []};
						_ ->
							case parseRecord(Rev, ResultArr) of
								{true, Fields} ->
									{ok, [{<<"_key">>, list_to_binary(Key)}, {<<"_rev">>, Rev} | Fields]};
								{false, _} ->
									{ok, []}
							end
					end;
				_ ->
					{ok, []}
			end;
		{ok, _} ->
			Result
	end.

parseRecord(Rev, ResultArr) ->
	parseRecord(Rev, ResultArr, #result{}).

parseRecord(_Rev, [], Result) ->
	{Result#result.revCorrect, Result#result.fields};

parseRecord(Rev, [R | T], Result) ->
	FieldName = proplists:get_value(<<"fieldname">>, R),
	FieldValue = proplists:get_value(<<"fieldvalue">>, R),
	Fieldrevision = proplists:get_value(<<"fieldrevision">>, R),
	
	case FieldValue of
		undefined ->
			parseRecord(Rev, T);
		_ ->
			if 
				Fieldrevision == Rev ->
					parseRecord(Rev, T, 
								#result{revCorrect = true, 
										fields = [{FieldName, FieldValue} | Result#result.fields]});
				true ->
					parseRecord(Rev, T, 
								#result{revCorrect = Result#result.revCorrect, 
										fields = [{FieldName, FieldValue} | Result#result.fields]})
			end
	end.


do_Put(Collection, Key, Json, Opts, State) ->
%% 	Mong = State#state.mongodb,
%% 	case Opts of
%% 		{revision, Rev} ->
%% 			getByRev(Mong, Collection, Key, Rev);
%% 		_ ->
%% 			Mong:findOne(Collection, [{"_key", Key}])		
%% 	end.
	ok.