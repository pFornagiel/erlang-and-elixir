-module(main).
-export([ping_loop/1,pong_loop/0, stop/0, stop/1,start/0,play/1]).

% Pong process loop: receives numbers, prints them, and sends decremented value to ping.
pong_loop() ->
  receive
    0 -> 
      io:format("pong got 0, it ends here", []),
      pong_loop();

    N when is_number(N) -> 
      io:format("pong got ~w~n", [N]),
      timer:sleep(100),
      ping ! N-1,
      % io:format("pong sent ~w~n", [N-1]),
      timer:sleep(100),
      pong_loop();

    _ -> ok

    after 
      20000 -> ok
  end.

% Ping process loop: receives numbers, prints state and value, and sends decremented value to pong.
ping_loop(State) ->
  receive
    0 -> 
      io:format("Ping state: ~w~n", [State]),
      io:format("ping got 0, it ends here", []),
      ping_loop(State+1);

    N when is_number(N) -> 
      io:format("Ping state: ~w~n", [State]),
      io:format("ping got ~w~n", [N]),
      timer:sleep(100),
      pong ! N-1,
      % io:format("ping sent ~w~n", [N-1]),
      timer:sleep(100),
      ping_loop(State+1);

    _ -> ok

    after 
      20000 -> ok
  end.

% Stops both ping and pong processes.
stop() ->
  pong ! stop,
  ping ! stop.

% Stops a process with a given alias.
stop(Alias) ->
  Alias ! stop.

% Starts and registers ping and pong processes.
start() ->
  register(ping, spawn(fun () -> ping_loop(0) end)),
  register(pong, spawn(fun () -> pong_loop() end)).

% Sends a number N to the ping process to start the ping-pong sequence.
play(N) ->
  ping ! N.

