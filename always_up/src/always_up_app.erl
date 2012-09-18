%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the always_up application.

-module(always_up_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for always_up.
start(_Type, _StartArgs) ->
    always_up_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for always_up.
stop(_State) ->
    ok.
