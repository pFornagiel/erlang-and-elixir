%%%-------------------------------------------------------------------
%% @doc lab public API
%% @end
%%%-------------------------------------------------------------------

-module(lab_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lab_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
