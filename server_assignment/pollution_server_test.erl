%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% EUnit test module for pollution_server. Contains tests for all server operations.
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").

% Starts the pollution server before each test.
start() -> 
  pollution_server:start().

% Stops the pollution server after each test.
stop(_) ->
  pollution_server:stop().

% Runs all tests in sequence, starting and stopping the server for each.
foreach_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
  [
    fun add_station_test/1, 
    fun add_value_test/1,
    fun add_value_fail_test/1,
    fun add_value_non_existing_station_test/1,
    fun remove_value_test/1,
    fun remove_value_and_add_back_test/1,
    fun remove_value_fail_test/1,
    fun get_one_value_test/1,
    fun get_one_value_fail_test/1
  ]
}.
  

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_server_test() ->
  pollution_server:start(),
  ?assertNotEqual(whereis(server_pol), undefined),
  Res = pollution_server:stop(),
  ?assertEqual(whereis(server_pol), undefined),
  ?assertEqual(Res, terminated).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test(_) ->
  M2 = pollution_server:add_station("Stacja 1", {1,1}),
  [
    ?_assertNotMatch({error, _}, M2),
    ?_assertMatch({error, _}, pollution_server:add_station("Stacja 1", {1,1})),
    ?_assertMatch({error, _}, pollution_server:add_station("Stacja 1", {2,2})),
    ?_assertMatch({error, _}, pollution_server:add_station("Stacja 2", {1,1}))
  ].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),

  M1 = pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  M2 = pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  timer:sleep(1100),
  Time2 = calendar:local_time(),
  
  M4 = pollution_server:add_value({1,1}, Time2, "PM10", 46.3),
  M5 = pollution_server:add_value({1,1}, Time2, "PM1", 46.3),
  M6 = pollution_server:add_value({1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3),
  
  [
    ?_assertNotMatch({error, _}, M1),
    ?_assertNotMatch({error, _}, M2),
    ?_assertNotMatch({error, _}, M3),

    ?_assertNotMatch({error, _}, M4),
    ?_assertNotMatch({error, _}, M5),
    ?_assertNotMatch({error, _}, M6)
  ].


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  M2 = pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),

  [
    ?_assertNotMatch({error, _}, M2),
    ?_assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
    ?_assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 36.3)),
    ?_assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 46.3)),
    ?_assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 36.3))
  ].
  


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  [
    ?_assertMatch({error, _}, pollution_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3)),
    ?_assertMatch({error, _}, pollution_server:add_value({1,2}, calendar:local_time(), "PM10", 46.3))
  ].
 


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  M4 = pollution_server:remove_value("Stacja 1", Time, "PM10"),
  M5 = pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),

  [
    ?_assertNotMatch({error, _}, M4),
    ?_assertNotEqual(M4, M3),
    ?_assertNotMatch({error, _}, M5),
    ?_assertNotEqual(M5, M4)
  ].
  


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  M4 = pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
  M5 = pollution_server:add_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  [
    ?_assertNotEqual(M4, M3),
    ?_assertEqual(M5, M3)
  ].
  


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  [
    ?_assertMatch({error, _}, pollution_server:remove_value("Stacja 1", Time, "PM25")),
    ?_assertMatch({error, _}, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
    ?_assertMatch({error, _}, pollution_server:remove_value({1,2}, Time, "PM10")),
    ?_assertMatch({error, _}, pollution_server:remove_value("Stacja 2", Time, "PM10"))
  ].
 


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  [
    ?_assertMatch(46.3, pollution_server:get_one_value("Stacja 1", Time, "PM10")),
    ?_assertMatch(36.3, pollution_server:get_one_value("Stacja 1", Time, "PM1")),
    ?_assertMatch(46.3, pollution_server:get_one_value({1,1}, Time, "PM10")),
    ?_assertMatch(26.3, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"))
  ].
  


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test(_) ->
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  [
    ?_assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", Time, "PM25")),
    ?_assertMatch({error, _}, pollution_server:get_one_value({1,1}, Time, "PM25")),
    ?_assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
    ?_assertMatch({error, _}, pollution_server:get_one_value("Stacja 2", Time, "PM1")),
    ?_assertMatch({error, _}, pollution_server:get_one_value({1,2}, Time, "PM10"))
  ].
 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_station_min_test() ->
%   M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
%   M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%   M2 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),
%   M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10, M2),
%   M4 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 20, M3),

%   ?assertMatch(10, pollution:get_station_min("Stacja 1", "PM10", M2)),
%   ?assertMatch(10, pollution:get_station_min({1,1}, "PM10", M4)),
%   ?assertMatch(10, pollution:get_station_min("Stacja 1", "PM10", M3)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_station_min_fail_test() ->
%   M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
%   ?assertMatch({error, _}, pollution:get_station_min("Stacja 1", "PM10", M)),
%   M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%   ?assertMatch({error, _}, pollution:get_station_min("Stacja 1", "PM25", M1)),
%   ?assertMatch({error, _}, pollution:get_station_min("Stacja 2", "PM25", M1)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_daily_mean_test() ->
%   M = pollution:add_station("Stacja 3", {3,3}, pollution:add_station("Stacja 2", {2,2}, pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()))),
%   M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%   M2 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),
%   M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10, M2),
%   M4 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20, M3),

%   M5 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100, M4),
%   M6 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220, M5),

%   M7 = pollution:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000, M6),
%   M8 = pollution:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000, M7),

%   M9 = pollution:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234, M8),

%   ?assertMatch(15.0, pollution:get_daily_mean("PM10",{2023,3,27}, M2)),
%   ?assertMatch(15.0, pollution:get_daily_mean("PM10",{2023,3,27}, M6)),
%   ?assertMatch(258.8, pollution:get_daily_mean("PM10",{2023,3,27}, M9)).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_daily_mean_fail_test() ->
%   M = pollution:add_station("Stacja 2", {2,2}, pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor())),
%   ?assertMatch({error, _}, pollution:get_daily_mean("PM10",{2023,3,27}, M)),
%   M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%   M2 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),

%   ?assertMatch({error, _}, pollution:get_daily_mean("PM25",{2023,3,27}, M2)),
%   ?assertMatch({error, _}, pollution:get_daily_mean("PM10",{2023,3,29}, M2)).

