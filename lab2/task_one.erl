-module(zadanie).
-export([first/0, second/0, third/0]).

% Replaces 'o' with 'a', 'e' with 'o', and leaves other characters unchanged in the string "alamakota".
first() ->
  A = fun 
    ($o) -> $a;
    ($e) -> $o;
    (X) -> X
  end,
  lists:map(A, "alamakota").

% Counts how many numbers in the list [3,4,5,6,7,8,9,10] are divisible by 3.
second() -> 
  Dividable = fun 
    (X) when X rem 3 == 0 -> true;
    (_) -> false
  end,

  length(lists:filter(Dividable, [3,4,5,6,7,8,9,10])).

% Returns a list of two sample data tuples.
data() -> 
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

% Calculates the mean of all values in the third element of the data tuples.
third() ->
  Dane = dane(),
  Third_argument = fun ({_,_,X}) -> X end,
  Third_arg_list = lists:map(Third_argument, Dane),
  Third_arg_flattened = lists:foldl(fun (X, L) -> X ++ L end, [], Third_arg_list),
  Vals = lists:map(fun ({_, X})-> X end, Third_arg_list),
  lists:sum(Vals) / length(Vals).