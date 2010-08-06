%% Author: noah
-module(storageunit_test).

-compile(export_all).
%%
%% Include files
%%

%%
%% Exported Functions
%%
%% -export([start/0, stop/0]).

%%
%% API Functions
%%
start() ->
	storageunit:start_link([
		{poolName, erlPNUTS},
		{host, "localhost"},
		{port, 27017},
		{database, "testdb"}]).

get_test() ->
	storageunit:get("user", "Noah", {revision, 4}).

stop() ->
    void.


%%
%% Local Functions
%%

