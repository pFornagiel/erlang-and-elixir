-module(qc).
-export([qs/1, less_than/2, grt_eq_than/2, random_elems/3 ,compare_speeds/3, zadanie/0]).

% Returns a list of elements from List that are less than Arg.
less_than(List, Arg) -> 
  [X || X <- List, X<Arg].

% Returns a list of elements from List that are greater than or equal to Arg.
grt_eq_than(List, Arg) ->
  [X || X <- List, X>=Arg].

% QuickSort implementation using less_than and grt_eq_than helpers.
qs([]) ->
  [];
qs([Pivot|Tail]) -> 
  qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

% Generates a list of N random integers between Min and Max.
random_elems(N,Min,Max)-> 
  [rand:uniform(Max)  + Min || _ <- lists:seq(0,N)].

% Compares the execution time of two functions Fun1 and Fun2 on List.
compare_speeds(List, Fun1, Fun2) -> 
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Fun1: ~w ms,~nFun2: ~w ms~n", [Time1, Time2]).

