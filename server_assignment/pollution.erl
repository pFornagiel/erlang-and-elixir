-module(pollution).
-export([sample_info/0, create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, 
         get_station_min/3, get_daily_mean/3]).

-record(measurement, {type, value, datetime}).
-record(station, {name, coordinates, measurements=[]}).

% Returns a list of sample stations with measurements.
sample_info() ->
  MeasurementOne = #measurement{
    type="PM10",
    value=3,
    datetime={{2025,4,5}, {11,4,30}}
  },
  MeasurementTwo = #measurement{
    type="MP25",
    value=4,
    datetime={{2023,4,5}, {12,5,40}}
  },
  MeasurementThree = #measurement{
    type="TEMP",
    value=25,
    datetime={{2025,2,15},{4,50,20}}
  },

  StationOne = #station{
    name="Rogów",
    coordinates={2.0, 3.0},
    measurements=[MeasurementOne]
  },

  StationTwo = #station{
    name="Bełsznica",
    coordinates={2.0, 3.0},
    measurements=[MeasurementTwo, MeasurementThree]
  },

  [StationOne, StationTwo].

% Checks if a station exists by coordinates or name.
checkIfExists(#station{coordinates=CurrentCoordinates}, {X_coord, Y_coord}) -> 
  CurrentCoordinates == {X_coord, Y_coord};
checkIfExists(#station{name=CurrentName}, Name) when is_list(Name) ->
  CurrentName == Name.
checkIfExists(#station{coordinates=CurrentCoordinates, name=CurrentName}, Name, {X_coord, Y_coord}) -> 
  CurrentCoordinates == {X_coord, Y_coord} orelse CurrentName == Name.

% Checks if a measurement exists by type and time.
checkMeasurementExists(#measurement{type=T, datetime=D}, Type, Time) -> 
  T == Type andalso D == Time.

% Creates an empty monitor (list of stations).
create_monitor() -> 
  [].

% Adds a new station to the monitor if it does not already exist.
add_station(Name, Coordinates, Monitor) ->
  case lists:any(fun(S) -> checkIfExists(S, Name, Coordinates) end, Monitor) of
    false -> [#station{name=Name, coordinates=Coordinates} | Monitor];
    true -> {error, station_already_exists}
  end.

% Adds a measurement to a station by coordinates or name.
add_value({X_coord, Y_coord}, Time, Type, Value, Monitor) ->
  NewMeasurement = #measurement{type=Type, value=Value, datetime=Time},
  NonExistingStation = [S || S <- Monitor, S#station.coordinates == {X_coord, Y_coord}],
  ExistingMeasurement = [Ms || S <- Monitor, Ms <- S#station.measurements, Ms#measurement.type == Type, Ms#measurement.datetime == Time, S#station.coordinates == {X_coord, Y_coord}],

  F = fun
    (S = #station{coordinates=CurrentCoordinates, measurements=Ms}) when CurrentCoordinates == {X_coord, Y_coord} ->
      S#station{measurements=[NewMeasurement | Ms]};
    (S) -> S
  end,
    
  case {NonExistingStation, ExistingMeasurement} of 
    {[], _} -> {error, station_does_not_exist};
    {_, []} -> lists:map(F, Monitor);
    _ -> {error, measurement_already_exists}
  end;

add_value(Name, Time, Type, Value, Monitor) when is_list(Name) ->
  NewMeasurement = #measurement{type=Type, value=Value, datetime=Time},
  NonExistingStation = [S || S <- Monitor, S#station.name == Name],
  ExistingMeasurement = [Ms || S <- Monitor, Ms <- S#station.measurements, Ms#measurement.type == Type, Ms#measurement.datetime == Time, S#station.name == Name],

  F = fun
    (S = #station{name=CurrentName, measurements=Ms}) when CurrentName == Name ->
        S#station{measurements=[NewMeasurement | Ms]};
      (S) -> S
  end,
    
  case {NonExistingStation, ExistingMeasurement} of 
    {[], _} -> {error, station_does_not_exist};
    {_, []} -> lists:map(F, Monitor);
    _ -> {error, measurement_already_exists}
  end.

% Removes a measurement from a station by coordinates or name.
remove_value({X_coord, Y_coord}, Time, Type, Monitor) -> 
  ToDelete = [Ms || S <- Monitor, Ms <- S#station.measurements, Ms#measurement.type == Type, Ms#measurement.datetime == Time, S#station.coordinates == {X_coord, Y_coord}],
  
  F = fun
    (S = #station{coordinates=CurrentCoordinates, measurements=Ms}) when CurrentCoordinates == {X_coord, Y_coord} ->
      [Value | _] = ToDelete,
      S#station{measurements=Ms -- [Value]};
    (S) -> S
  end,

    
  case ToDelete of 
    [] -> {error, station_does_not_exist};
    _ -> lists:map(F, Monitor)
  end;

remove_value(Name, Time, Type, Monitor) when is_list(Name) ->
  ToDelete = [Ms || S <- Monitor, Ms <- S#station.measurements, Ms#measurement.type == Type, Ms#measurement.datetime == Time, S#station.name == Name],
  
    F = fun
      (S = #station{name=CurrentName, measurements=Ms}) when CurrentName == Name ->
        [Value | _] = ToDelete,
        S#station{measurements=Ms -- [Value]};
      (S) -> S
    end,
  
      
    case ToDelete of 
      [] -> {error, station_does_not_exist};
      _ -> lists:map(F, Monitor)
    end.

% Gets a single measurement value from a station by coordinates or name.
get_one_value({X_coord, Y_coord}, Time, Type, Monitor) ->
  case lists:search(fun(S) -> checkIfExists(S, {X_coord, Y_coord}) end, Monitor) of 
    false -> {error, station_does_not_exist};
    {value, #station{measurements=Ms}} -> 
      case lists:search(fun(M) -> checkMeasurementExists(M, Type, Time) end, Ms) of
        false -> {error, no_such_measurement};
        {value, #measurement{value=Value}} -> Value
      end
  end;

get_one_value(Name, Time, Type, Monitor) when is_list(Name) ->
  case lists:search(fun(S) -> checkIfExists(S, Name) end, Monitor) of 
    false -> {error, station_does_not_exist};
    {value, #station{measurements=Ms}} -> 
      case lists:search(fun(M) -> checkMeasurementExists(M, Type, Time) end, Ms) of
        false -> {error, no_such_measurement};
        {value, #measurement{value=Value}} -> Value
      end
  end.

% Gets the minimum value of a given type from a station by coordinates or name.
get_station_min({X_coord, Y_coord}, Type, Monitor) ->
  case lists:search(fun(S) -> checkIfExists(S, {X_coord, Y_coord}) end, Monitor) of 
    false -> {error, station_does_not_exist};
    {value, #station{measurements=Ms}} -> 
      Values = [X#measurement.value || X <- Ms, X#measurement.type == Type],
      case Values of
        [] -> {error, no_such_measurement};
        _ -> lists:min(Values)
      end
  end;

get_station_min(Name, Type, Monitor) when is_list(Name) ->
  case lists:search(fun(S) -> checkIfExists(S, Name) end, Monitor) of 
    false -> {error, station_does_not_exist};
    {value, #station{measurements=Ms}} -> 
      Values = [CurrentValue || #measurement{type=CurrentType, value=CurrentValue} <- Ms, CurrentType == Type],
      case Values of
        [] -> {error, no_such_measurement};
        _ -> lists:min(Values)
      end
  end.

% Gets the daily mean value of a given type for a specific day from all stations.
get_daily_mean(Type, Day, Monitor) ->
  AllMs = [Value || S <- Monitor, 
                    #measurement{type=MType, value=Value, datetime={Date,_}} <- S#station.measurements, 
                    MType == Type, Date == Day],
  case AllMs of
    [] -> {error, no_such_measurement};
    _ -> lists:sum(AllMs) / length(AllMs)
  end.