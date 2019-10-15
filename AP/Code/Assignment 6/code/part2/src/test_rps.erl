-module(test_rps).
-export([test_all/0]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       start_broker(),
       start_coordinator(),
       broker_handle_queue_up_game_found(),
       broker_handle_statistics(),
       broker_handle_drain()
      ], [verbose]).


start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.

start_coordinator() ->
    {"Start a coordinator, and nothing else",
     fun() ->
     	PidA = list_to_pid("<0.10.0>"),
     	PidB = list_to_pid("<0.20.0>"),
        ?assertMatch({ok, _}, rps_coordinator:start({make_ref(), make_ref(), 10, PidA, PidB}))
     end}.

broker_handle_queue_up_game_found() ->
    {"Handle queue up message in RPS broker",
     fun() ->
     	PidA = list_to_pid("<0.10.0>"),
     	PidB = list_to_pid("<0.20.0>"),
     	Resp = rps_broker:handle_call({q_up, "Bob", 4},
     								  {PidA, "bob"},
     								  {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 0}),
     	?assertMatch({reply, {ok, "Alice", _}, _}, Resp)
     end}.

broker_handle_statistics() ->
    {"Handle statistics message in RPS broker",
     fun() ->
     	PidA = list_to_pid("<0.10.0>"),
     	PidB = list_to_pid("<0.20.0>"),
     	Resp = rps_broker:handle_call(statistics,
     								  {PidA, "bob"},
     								  {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 10}),
     	?assertMatch({reply, {ok, 10, 1, 0}, _}, Resp)
     end}.

broker_handle_drain() ->
    {"Handle drain message in RPS broker",
     fun() ->
     	PidA = list_to_pid("<0.10.0>"),
     	PidB = list_to_pid("<0.20.0>"),
     	Resp = rps_broker:handle_call({drain, PidA, "STOP! HAMMER TIME!"},
     								  {PidA, "bob"},
     								  {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 10}),
     	?assertMatch("STOP! HAMMER TIME!", Resp)
     end}.