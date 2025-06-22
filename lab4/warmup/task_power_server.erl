-module(pow).
-behaviour(gen_server).

-export([start_link/0, step/0, read/0, close/0, crash/0, set/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, 2 ,[]).

init(N) -> {ok, N}.

step() -> gen_server:cast(?MODULE, step).
read() -> gen_server:call(?MODULE, read).
crash() -> gen_server:cast(?MODULE, crash).
close() -> gen_server:call(?MODULE, close).
set() -> gen_server:call(?MODULE, set).

handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N}.

handle_call(read, _From, N) -> {reply, N, N};
handle_call(set, _From, N) -> {reply, N, N};
handle_call(close, _From, N) -> {stop, normal, ok, N}.

handle_info(Msg, N) -> 
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, N}.


terminate(normal, N) -> io:format("The number is ~w.~nBye.", [N]).