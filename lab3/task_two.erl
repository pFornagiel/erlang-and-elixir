-module(second).
-export([test/0, rand_locs/1, find_closest/2, find_for_person/2, find_closest_parallel/2]).

% There are 2 lists of points on a 2D plane:
% a list of air quality sensor locations, for example 1000 of them
% a list of people locations who want to see any sensor - for example 20000 people

rand_locs(N) -> [{rand:uniform(10000), rand:uniform(10000)}|| _ <- lists:seq(0,N)].

% Calculates the Euclidean distance between two 2D points.
dist({X1, Y1}, {X2, Y2}) -> math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2) * (Y1-Y2)).

% Finds the closest sensor location to a given person location.
find_for_person(PersonLocation, SensorLocations) -> 
  io:format("a"),
  lists:min(
    [{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorLocations]
  ).

% Finds the closest pair (person, sensor) among all people and sensors.
find_closest(PeopleLocations, SensorLocations) ->
  lists:min(
    [find_for_person(PL, SensorLocations) || PL <- PeopleLocations]
  ).

% Test function: generates random locations and finds the closest pair in parallel.
test() ->
  PL = rand_locs(5),
  SL = rand_locs(5),
  A = find_closest_parallel(PL, SL),
  io:format("A: ~w~n", [A]),
  ok.

% Finds the closest pair using parallel processes for each person (may not terminate correctly).
find_closest_parallel(PL, SL) ->
  [spawn(fun () -> self() ! find_for_person(Person, SL) end) || Person <- PL],
  lists:min(
    [receive N -> N end || _ <- PL]
  ).