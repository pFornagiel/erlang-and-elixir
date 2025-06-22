-module(pollution).
-export([sample_info/0, create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, 
         get_station_min/3, get_daily_mean/3]).

-record(measurement, {type, value, datetime}).
-record(station, {name, coordinates, measurements=maps:new()}).

%% Now uses maps instead of lists
%% Key structure: {Type, DateTime} -> Value
%% Monitor structure: maps with keys as coordinates and names

sample_info() ->
    MeasurementMap1 = maps:from_list([
        {{"PM10", {{2025,4,5}, {11,4,30}}}, 3}
    ]),
    MeasurementMap2 = maps:from_list([
        {{"MP25", {{2023,4,5}, {12,5,40}}}, 4},
        {{"TEMP", {{2025,2,15},{4,50,20}}}, 25}
    ]),

    StationOne = #station{
        name="Rogów",
        coordinates={2.0, 3.0},
        measurements=MeasurementMap1
    },

    StationTwo = #station{
        name="Bełsznica", 
        coordinates={2.0, 3.0},
        measurements=MeasurementMap2
    },

    % Monitor as a map with both coordinate and name lookups
    Monitor = maps:new(),
    Monitor1 = maps:put({2.0, 3.0}, StationOne, Monitor),
    Monitor2 = maps:put("Rogów", StationOne, Monitor1),
    Monitor3 = maps:put({2.0, 3.0}, StationTwo, Monitor2), % This will overwrite, but it's just sample data
    maps:put("Bełsznica", StationTwo, Monitor3).

create_monitor() -> 
    maps:new().

add_station(Name, Coordinates, Monitor) ->
    % Check if station already exists by either name or coordinates
    case maps:is_key(Name, Monitor) orelse maps:is_key(Coordinates, Monitor) of
        true -> {error, station_already_exists};
        false -> 
            Station = #station{name=Name, coordinates=Coordinates},
            Monitor1 = maps:put(Name, Station, Monitor),
            maps:put(Coordinates, Station, Monitor1)
    end.

add_value(Identifier, Time, Type, Value, Monitor) ->
    case maps:find(Identifier, Monitor) of
        error -> {error, station_does_not_exist};
        {ok, Station} ->
            MeasurementKey = {Type, Time},
            case maps:is_key(MeasurementKey, Station#station.measurements) of
                true -> {error, measurement_already_exists};
                false ->
                    NewMeasurements = maps:put(MeasurementKey, Value, Station#station.measurements),
                    UpdatedStation = Station#station{measurements=NewMeasurements},
                    % Update both name and coordinate keys
                    Monitor1 = maps:put(Station#station.name, UpdatedStation, Monitor),
                    maps:put(Station#station.coordinates, UpdatedStation, Monitor1)
            end
    end.

remove_value(Identifier, Time, Type, Monitor) ->
    case maps:find(Identifier, Monitor) of
        error -> {error, station_does_not_exist};
        {ok, Station} ->
            MeasurementKey = {Type, Time},
            case maps:is_key(MeasurementKey, Station#station.measurements) of
                false -> {error, measurement_does_not_exist};
                true ->
                    NewMeasurements = maps:remove(MeasurementKey, Station#station.measurements),
                    UpdatedStation = Station#station{measurements=NewMeasurements},
                    % Update both name and coordinate keys
                    Monitor1 = maps:put(Station#station.name, UpdatedStation, Monitor),
                    maps:put(Station#station.coordinates, UpdatedStation, Monitor1)
            end
    end.

get_one_value(Identifier, Time, Type, Monitor) ->
    case maps:find(Identifier, Monitor) of
        error -> {error, station_does_not_exist};
        {ok, Station} ->
            MeasurementKey = {Type, Time},
            case maps:find(MeasurementKey, Station#station.measurements) of
                error -> {error, no_such_measurement};
                {ok, Value} -> Value
            end
    end.

get_station_min(Identifier, Type, Monitor) ->
    case maps:find(Identifier, Monitor) of
        error -> {error, station_does_not_exist};
        {ok, Station} ->
            % Filter measurements by type and extract values
            Values = maps:fold(
                fun({MType, _Time}, Value, Acc) when MType =:= Type -> [Value | Acc];
                   (_, _, Acc) -> Acc
                end,
                [],
                Station#station.measurements
            ),
            case Values of
                [] -> {error, no_such_measurement};
                _ -> lists:min(Values)
            end
    end.

get_daily_mean(Type, Day, Monitor) ->
    % Collect all values of given type from given day across all stations
    AllValues = maps:fold(
        fun(Key, Station, Acc) when is_tuple(Key) -> % Skip coordinate keys
                maps:fold(
                    fun({MType, {Date, _Time}}, Value, InnerAcc) 
                        when MType =:= Type, Date =:= Day -> 
                            [Value | InnerAcc];
                       (_, _, InnerAcc) -> InnerAcc
                    end,
                    Acc,
                    Station#station.measurements
                );
           (_, _, Acc) -> Acc % Skip name keys
        end,
        [],
        Monitor
    ),
    case AllValues of
        [] -> {error, no_such_measurement};
        _ -> lists:sum(AllValues) / length(AllValues)
    end.