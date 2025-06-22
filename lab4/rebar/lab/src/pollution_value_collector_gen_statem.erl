-module(pollution_value_collector_gen_statem).
-behaviour(gen_statem).
% -define(NAME, pollution_value_collector_gen_statem).

-export([start_link/0]).
-export([init/1,callback_mode/0,terminate/3]).
-export([station/3, value/3]).
-export([set_station/1, add_value/3,store_data/0,stop/0]).

start_link() ->
  gen_statem:start_link({local,?MODULE}, ?MODULE, {}, []).

init({}) ->
  {ok, station, {}}.

station(_Event, {set_station, NameOrCoordinates}, _State) ->
  Values = [],
  {next_state, value, {NameOrCoordinates, Values}}.
  
value(_Event, {add_value, Time, Type, Value}, {NameOrCoordinates, Values}) -> 
  NewValues = Values ++ [{Time,Type,Value}],
  {next_state, value, {NameOrCoordinates, NewValues}};

value(_Event, store_data, {NameOrCoordinates, Values}) -> 
  [pollution_gen_server:add_value(NameOrCoordinates, Time, Type, Value) || {Time,Type,Value} <- Values],
  {next_state, station, {}}.

% API %

set_station(NameOrCoordinates) -> gen_statem:cast(?MODULE, {set_station, NameOrCoordinates}).
add_value(Time, Type, Value) -> gen_statem:cast(?MODULE, {add_value, Time, Type, Value}).
store_data() -> gen_statem:cast(?MODULE, store_data).

stop() -> gen_statem:stop(?MODULE).

terminate(_Reason, _StateName, _StateData) -> ok.

callback_mode() ->
  state_functions.