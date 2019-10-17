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
       broker_handle_drain(),
       coordinator_handle_no_move_rock(),
       coordinator_handle_no_move_paper(),
       coordinator_handle_no_move_scissors(),
       coordinator_handle_no_move_invalid(),
       coordinator_handle_rock_paper(),
       coordinator_handle_rock_scissors(),
       coordinator_handle_rock_rock(),
       coordinator_handle_rock_invalid()
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
     	PidC = list_to_pid("<0.30.0>"),
        ?assertMatch({ok, _}, rps_coordinator:start({PidA, make_ref(), 10, {PidB, "bob"}, {PidC, "alice"}}))
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
     	PidB = list_to_pid("<0.20.0>"),
     	rps_broker:handle_cast({drain, self(), "STOP! HAMMER TIME!"},
							   {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 10}),
     	receive
     		X ->
     			?assertMatch("STOP! HAMMER TIME!", X)
     	end
     end}.

coordinator_handle_no_move_rock() ->
    {"Handle rock input in no_move state in RPS coordinator",
     fun() ->
     	{PidA, State, ExpState} = no_move_test_setup(),
     	Res = rps_coordinator:no_move({call, {PidA, "bob"}}, rock, State),
     	?assertMatch({next_state, rock, ExpState}, Res)
     end}.

coordinator_handle_no_move_paper() ->
    {"Handle paper input in no_move state in RPS coordinator",
     fun() ->
     	{PidA, State, ExpState} = no_move_test_setup(),
     	Res = rps_coordinator:no_move({call, {PidA, "bob"}}, paper, State),
     	?assertMatch({next_state, paper, ExpState}, Res)
     end}.

coordinator_handle_no_move_scissors() ->
    {"Handle paper input in no_move state in RPS coordinator",
     fun() ->
     	{PidA, State, ExpState} = no_move_test_setup(),
     	Res = rps_coordinator:no_move({call, {PidA, "bob"}}, scissors, State),
     	?assertMatch({next_state, scissors, ExpState}, Res)
     end}.

coordinator_handle_no_move_invalid() ->
    {"Handle invalid input in no_move state in RPS coordinator",
     fun() ->
     	{PidA, State, ExpState} = no_move_test_setup(),
     	Res = rps_coordinator:no_move({call, {PidA, "bob"}}, invalid, State),
     	?assertMatch({next_state, invalid, ExpState}, Res)
     end}.

coordinator_handle_rock_paper() ->
    {"Handle paper input in rock state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_win(),
     	Res = rps_coordinator:rock({call, {PidB, "alice"}}, paper, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_rock_scissors() ->
    {"Handle scissors input in rock state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_loose(),
     	Res = rps_coordinator:rock({call, {PidB, "alice"}}, scissors, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_rock_rock() ->
    {"Handle rock input in rock state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_tie(),
     	Res = rps_coordinator:rock({call, {PidB, "alice"}}, rock, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_rock_invalid() ->
    {"Handle invalid input in rock state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_loose(),
     	Res = rps_coordinator:rock({call, {PidB, "alice"}}, invalid, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

no_move_test_setup() ->
	PidA = list_to_pid("<0.10.0>"),
 	BrokerRef = make_ref(),
 	CoordRef = make_ref(),
 	State = {{BrokerRef, CoordRef},
 			 {5, 0},
 			 #{}},
 	ExpState = {{BrokerRef, CoordRef},
 			   {5, 0},
 			   #{PidA => {{PidA, "bob"}, 0}}},
 	{PidA, State, ExpState}.

% Second to move player wins the round
move_test_setup_win() ->
	PidA = list_to_pid("<0.10.0>"),
	PidB = list_to_pid("<0.20.0>"),
 	BrokerRef = make_ref(),
 	CoordRef = make_ref(),
 	State = {{BrokerRef, CoordRef},
 			 {5, 0},
 			 #{PidA => {{PidA, "bob"}, 0}}},
 	ExpState = {{BrokerRef, CoordRef},
 			   {5, 1},
 			   #{PidA => {{PidA, "bob"}, 0},
 			     PidB => {{PidB, "alice"}, 1}}},
 	{PidB, State, ExpState}.

% Second to move player looses the round
move_test_setup_loose() ->
	PidA = list_to_pid("<0.10.0>"),
	PidB = list_to_pid("<0.20.0>"),
 	BrokerRef = make_ref(),
 	CoordRef = make_ref(),
 	State = {{BrokerRef, CoordRef},
 			 {5, 0},
 			 #{PidA => {{PidA, "bob"}, 0}}},
 	ExpState = {{BrokerRef, CoordRef},
 			   {5, 1},
 			   #{PidA => {{PidA, "bob"}, 1},
 			     PidB => {{PidB, "alice"}, 0}}},
 	{PidB, State, ExpState}.

% Second to move player looses the round
move_test_setup_tie() ->
	PidA = list_to_pid("<0.10.0>"),
	PidB = list_to_pid("<0.20.0>"),
 	BrokerRef = make_ref(),
 	CoordRef = make_ref(),
 	State = {{BrokerRef, CoordRef},
 			 {5, 0},
 			 #{PidA => {{PidA, "bob"}, 0}}},
 	ExpState = {{BrokerRef, CoordRef},
 			   {5, 1},
 			   #{PidA => {{PidA, "bob"}, 0},
 			     PidB => {{PidB, "alice"}, 0}}},
 	{PidB, State, ExpState}.