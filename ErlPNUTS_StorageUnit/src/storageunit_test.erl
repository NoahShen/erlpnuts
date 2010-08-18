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
	storageunit:get("user", "Noah", {revision, 3}).

put_test() ->
	storageunit:put("user", "Noah6", [{"name", "Noah6"},
									  {"password", "123456_6"}]).

stop() ->
    void.


%%
%% Local Functions
%%

