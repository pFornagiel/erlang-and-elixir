-module(pollution_server).
-export([start/0, stop/0, init/0]).

-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, 
         get_station_min/2, get_daily_mean/2]).

% Main server loop: handles requests for station and measurement management.
loop(Monitor) ->
  receive
    {init, Pid} -> 
      NewMonitor = pollution:sample_info(),
      Pid ! {reply, NewMonitor},
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;
    
    {add_station, Pid, Name, Coordinates} ->
      NewMonitor = pollution:add_station(Name, Coordinates, Monitor),
      Pid ! {reply, NewMonitor},
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;

    {add_value, Pid, CoordinatesOrName, Time, Type, Value} ->
      NewMonitor = pollution:add_value(CoordinatesOrName, Time, Type, Value, Monitor),
      Pid ! {reply, NewMonitor},
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;

    {remove_value, Pid, CoordinatesOrName, Time, Type} -> 
      NewMonitor = pollution:remove_value(CoordinatesOrName, Time, Type, Monitor),
      Pid ! {reply, NewMonitor},
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;

    {get_one_value, Pid ,CoordinatesOrName, Time, Type} -> 
      Value = pollution:get_one_value(CoordinatesOrName, Time, Type, Monitor),
      Pid ! {reply, Value},
      loop(Monitor);

    {get_station_min, Pid ,CoordinatesOrName, Type} -> 
      MinValue = pollution:get_station_min(CoordinatesOrName, Type, Monitor),
      Pid ! {reply, MinValue},
      loop(Monitor);

    {get_daily_mean, Pid ,Type, Day} -> 
      MeanValue = pollution:get_daily_mean(Type, Day, Monitor),
      Pid ! {reply, MeanValue},
      loop(Monitor);

    {stop, Pid} -> 
      Pid ! {reply, terminated};

    {_, Pid} ->
      Pid ! {reply, "Unknown request"},
      loop(Monitor)
  end.

% Starts the pollution server process and registers it.
start() -> 
  register(server_pol, spawn(fun () -> loop(pollution:create_monitor()) end)),
  ok.

% Stops the pollution server process.
stop() ->
  server_pol ! {stop, self()},
  receive
    {_,X} -> X
  end.

% Initializes the server with sample data and returns the monitor.
init() ->
  server_pol ! {init, self()},
  receive
    {_, X} -> X
  end.

% Sends a request to add a station.
add_station(Name, Coordinates) ->
  server_pol ! {add_station, self(), Name, Coordinates},
  receive 
    {_, Monitor} -> Monitor
  end.

% Sends a request to add a measurement value.
add_value(CoordinatesOrName, Time, Type, Value) ->
  server_pol ! {add_value, self(), CoordinatesOrName, Time, Type, Value},
  receive
    {_, Monitor} -> Monitor
end.

% Sends a request to remove a measurement value.
remove_value(CoordinatesOrName, Time, Type) ->
  server_pol ! {remove_value, self(), CoordinatesOrName, Time, Type},
  receive
    {_, Monitor} -> Monitor
end.

% Sends a request to get a single measurement value.
get_one_value(CoordinatesOrName, Time, Type) ->
  server_pol ! {get_one_value, self(), CoordinatesOrName, Time, Type},
  receive
    {_, Monitor} -> Monitor
end.

% Sends a request to get the minimum value of a type for a station.
get_station_min(CoordinatesOrName, Type) ->
  server_pol ! {get_station_min, self(), CoordinatesOrName, Type},
  receive
    {_, Monitor} -> Monitor
end.

% Sends a request to get the daily mean value of a type.
get_daily_mean(Type, Day) ->
  server_pol ! {get_daily_mean, self(), Type, Day},
  receive
    {_, Monitor} -> Monitor
end.

