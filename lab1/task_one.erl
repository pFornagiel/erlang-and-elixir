-module(station).
-import(lists,[max/1, sum/1]). 
-export([number_of_readings/2, readings/0, calculate_max/2, calculate_mean/2]).

% Returns a list of sample readings from two stations.
readings() -> 
  P1 = {
    "Nazwa 1",
    {
      {12,8,34},
      {22,03,2025}
    },
    [{"temp", 32}, {"ciśnienie", 12}]
  },

  P2 = {
    "Nazwa 2",
    {
      {12,41,34},
      {22,03,2025}
    },
    [{"temp", 1}]
  },

  [P1, P2].

% Counts the number of readings for a given date in the list of readings.
number_of_readings([], _) ->
  0;
number_of_readings([{_, {_, OtherDate}, Values } | T], Date) ->
  case Date of 
    OtherDate -> number_of_readings(T, Date) + length(Values);
    _ -> number_of_readings(T, OtherDate)
  end.

% Helper function: extracts values of a given type from all readings.
getValuesOfType([], _) ->
  [];
getValuesOfType([{_, _, Values } | T], Type) ->
  [X || {OtherType, X} <- Values, OtherType == Type ] ++ getValuesOfType(T,Type).

% Returns the maximum value of a given type from the readings.
calculate_max(Values, Type) ->
  Filtered = getValuesOfType(Values, Type),
  case Filtered of 
    [] -> throw("Type not permited");
    _ -> max(Filtered)
  end.
  
% Returns the mean value of a given type from the readings.
calculate_mean(Values, Type) -> 
  Filtered = getValuesOfType(Values, Type),
  case Filtered of 
    [] -> throw("Type not permited");
    _ -> sum(Filtered)/length(Filtered)
  end.