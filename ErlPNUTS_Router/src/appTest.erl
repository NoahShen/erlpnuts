%% Author: noah
%% Created: 2010-8-1
%% Description: TODO: Add description to appTest
-module(appTest).

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
    mochiweb_http:start([
        {name, ?MODULE},
        {ip, any},
        {port, 6500},
        {loop, fun(Req) ->
            error_logger:info_report([helloweb, {req, Req}]),
            Req:ok({"text/plain", "hello world"})
        end}
    ]).
stop() ->
    mochiweb:stop(?MODULE).


%%
%% Local Functions
%%

