%%%-------------------------------------------------------------------
%%% @author olek-laptop
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, 
         get_station_min/2, get_daily_mean/2, crash/0]).

-define(SERVER, ?MODULE). 

% -record(pollution_gen_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, pollution:create_monitor(), []).

init(Monitor) ->
  {ok, Monitor}.

handle_call({add_station, Name, Coordinates}, _From, Monitor) ->
  NewMonitor = pollution:add_station(Name, Coordinates, Monitor),
  case NewMonitor of
    {error, _} ->   {reply, ok, Monitor};
    _ -> {reply, ok, NewMonitor}
  end;

handle_call({add_value, CoordinatesOrName, Time, Type, Value}, _From, Monitor) ->
  NewMonitor = pollution:add_value(CoordinatesOrName, Time, Type, Value, Monitor),
  case NewMonitor of
    {error, _} ->   {reply, ok, Monitor};
    _ -> {reply, ok, NewMonitor}
  end;

handle_call({remove_value, CoordinatesOrName, Time, Type}, _From, Monitor) ->
  NewMonitor = pollution:remove_value(CoordinatesOrName, Time, Type, Monitor),
  case NewMonitor of
    {error, _} ->   {reply, ok, Monitor};
    _ -> {reply, ok, NewMonitor}
  end;

handle_call({get_one_value, CoordinatesOrName, Time, Type}, _From, Monitor) ->
  Value = pollution:get_one_value(CoordinatesOrName, Time, Type, Monitor),
  {reply, Value, Monitor };

handle_call({get_station_min, CoordinatesOrName, Type}, _From, Monitor) ->
  MinValue = pollution:get_station_min(CoordinatesOrName, Type, Monitor),
  {reply, MinValue, Monitor};

handle_call({get_daily_mean, Type, Day}, _From, Monitor) ->
  MeanValue = pollution:get_daily_mean(Type, Day, Monitor),
  {reply, MeanValue, Monitor};

handle_call({replace_state, NewMonitor}, _From, Monitor) ->
  {reply, NewMonitor, NewMonitor};

handle_call({get_state}, _From, Monitor) ->
  {reply, Monitor, Monitor};

handle_call(_, _From, Monitor) ->
  {reply, no_such_call_error, Monitor}.

% handle_call(_Request, _From, State = #pollution_gen_server_state{}) ->
%     {reply, ok, State}.

handle_cast(_Request, Monitor) ->
    {noreply, Monitor}.

handle_info(_Info, Monitor) ->
    {noreply, Monitor}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, Monitor, _Extra) ->
    {ok, Monitor}.

%%%===================================================================
%%% GenServer API
%%%===================================================================
add_station(Name, Coordinates) ->
  gen_server:call(?MODULE, {add_station, Name, Coordinates}).
  
add_value(CoordinatesOrName, Time, Type, Value) ->
  gen_server:call(?MODULE, {add_value, CoordinatesOrName, Time, Type, Value}).
  
remove_value(CoordinatesOrName, Time, Type) ->
  gen_server:call(?MODULE, {remove_value, CoordinatesOrName, Time, Type}).
  
get_one_value(CoordinatesOrName, Time, Type) ->
  gen_server:call(?MODULE, {get_one_value, CoordinatesOrName, Time, Type}).
  
get_station_min(CoordinatesOrName, Type) ->
  gen_server:call(?MODULE, {get_station_min, CoordinatesOrName, Type}).

get_daily_mean(Type, Day) ->
  gen_server:call(?MODULE, {get_daily_mean, Type, Day}).

replace_state(Monitor) ->
  gen_server:call(?MODULE, {replace_state, Monitor}).

get_state() ->
  gen_server:call(?MODULE, {get_state}).

crash() ->
  gen_server:call(?MODULE, {crashing_serv, 0, 1, 2}).