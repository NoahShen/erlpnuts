%% Author: noah
-module(storageunit_options).
-author('Noah.Shen87@gmail.com').
%%
%% Include files
%%
-include("storageunit.hrl").

%%
%% Exported Functions
%%
-export([parse_dboptions/1]).

%%
%% API Functions
%%
parse_dboptions(Options) ->
	parse_dboptions(Options, #dbOptions{}).


parse_dboptions([], State) ->
	{ok, State};

parse_dboptions([{poolName, P} | Rest], State) when is_atom(P) ->
    parse_dboptions(Rest, State#dbOptions{poolName = P});

parse_dboptions([{poolSize, S} | Rest], State) when is_integer(S) ->
    parse_dboptions(Rest, State#dbOptions{poolSize = S});

parse_dboptions([{host, H} | Rest], State) when is_list(H) ->
    parse_dboptions(Rest, State#dbOptions{host = H});

parse_dboptions([{port, Port} | Rest], State) when is_integer(Port) ->
    parse_dboptions(Rest, State#dbOptions{port = Port});

parse_dboptions([{database, D} | Rest], State) when is_list(D) ->
    parse_dboptions(Rest, State#dbOptions{database = D}).


%%
%% Local Functions
%%

