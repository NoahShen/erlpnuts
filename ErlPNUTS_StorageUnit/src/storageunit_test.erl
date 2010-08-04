%% Author: noah
-module(storageunit_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, stop/0]).

%%
%% API Functions
%%
start() ->
	storageunit:start_link([
		{poolName, pool1},
		{poolSize, 1},
		{host, "localhost"},
		{port, 27017},
		{collection, "testdb"}]).

stop() ->
    void.


%%
%% Local Functions
%%

