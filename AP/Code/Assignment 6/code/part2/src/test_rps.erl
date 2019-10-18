-module(test_rps).
-export([test_all/0]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       broker_fixture(),
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
       coordinator_handle_rock_invalid(),
       coordinator_handle_paper_paper(),
       coordinator_handle_paper_scissors(),
       coordinator_handle_paper_rock(),
       coordinator_handle_paper_invalid(),
       coordinator_handle_scissors_paper(),
       coordinator_handle_scissors_rock(),
       coordinator_handle_scissors_scissors(),
       coordinator_handle_scissors_invalid(),
       coordinator_handle_invalid_paper(),
       coordinator_handle_invalid_scissors(),
       coordinator_handle_invalid_rock(),
       coordinator_handle_invalid_invalid()
      ], [verbose]).

start_broker_setup() ->
	rps:start().

stop_broker_teardown({ok, _}) ->
	% rps:drain(BrokerRef, self(), "Stop!"),
	% receive
	% 	"Stop!" -> ok
	% end,
	% timer:sleep(10)
	ok.

broker_fixture() ->
	{
		foreach,
		local,
		fun start_broker_setup/0,
		fun stop_broker_teardown/1,
		[fun move_broker/1,
		 fun game_broker/1,
		 fun best_of_0/1]
	}.

move_broker({ok, BrokerRef}) ->
	{"Issue a move to RPS broker",
	 fun() ->
		spawn(fun() ->
	 			rps:queue_up(BrokerRef, "Ken", 1337)
	 		  end),
	 	spawn(fun() ->
	 			{ok, _, Coord1} = rps:queue_up(BrokerRef, "Bob", 10),
	 			?assertMatch(round_lost, rps:move(Coord1, rock)),
				?assertMatch(server_stopping, rps:move(Coord1, rock))
	 		  end),
	 	timer:sleep(10),
	 	{ok, _, Coord2} = rps:queue_up(BrokerRef, "Alice", 10),
	 	?assertMatch(round_won, rps:move(Coord2, paper)),
		?assertMatch({ok, 0, 1, 1}, rps:statistics(BrokerRef)),
		rps:drain(BrokerRef, self(), "Stop!"),
		timer:sleep(20),
		?assertMatch(server_stopping, rps:move(Coord2, paper)),
		receive
			"Stop!" -> ok
		end,
		timer:sleep(10)
	 end
	}.

game_broker({ok, BrokerRef}) ->
	{"Issue a game to RPS broker",
	 fun() ->
	 	spawn(fun() ->
	 			{ok, _, Coord1} = rps:queue_up(BrokerRef, "Bob", 6),
	 			?assertMatch(round_lost, rps:move(Coord1, rock)),
				?assertMatch(round_won, rps:move(Coord1, rock)),
				% ?assertMatch(round_lost, rps:move(Coord1, lizard)),
				?assertMatch(round_lost, rps:move(Coord1, rock)),
				?assertMatch(tie, rps:move(Coord1, paper)),
				?assertMatch(round_lost, rps:move(Coord1, scissors)),
				?assertMatch({game_over, 1, 4}, rps:move(Coord1, paper))
	 		  end),
	 	timer:sleep(10),
	 	{ok, _, Coord2} = rps:queue_up(BrokerRef, "Alice", 6),
	 	?assertMatch(round_won, rps:move(Coord2, paper)),
		?assertMatch(round_lost, rps:move(Coord2, scissors)),
		?assertMatch(round_won, rps:move(Coord2, paper)),
		?assertMatch(tie, rps:move(Coord2, paper)),
		?assertMatch(round_won, rps:move(Coord2, rock)),
		?assertMatch({game_over, 4, 1}, rps:move(Coord2, scissors)),
		?assertMatch({ok, 6, 0, 0}, rps:statistics(BrokerRef)),
		rps:drain(BrokerRef, self(), "Stop!"),
		receive
			"Stop!" -> ok
		end
	 end
	}.

best_of_0({ok, BrokerRef}) ->
	{"Issue a game to RPS broker",
	 fun() ->
	 	spawn(fun() ->
	 			{ok, _, Coord1} = rps:queue_up(BrokerRef, "ThisGuy", 0),
	 			?assertMatch(tie, rps:move(Coord1, someWeirdSign)),
				?assertMatch({game_over, 0, 1}, rps:move(Coord1, paper))
	 		  end),
	 	timer:sleep(10),
	 	{ok, _, Coord2} = rps:queue_up(BrokerRef, "ThatGuy", 0),
	 	?assertMatch(tie, rps:move(Coord2, theAlmightySpock)),
		?assertMatch({game_over, 1, 0}, rps:move(Coord2, scissors)),
		?assertMatch({ok, 2, 0, 0}, rps:statistics(BrokerRef)),
		rps:drain(BrokerRef, self(), "Stop!"),
		receive
			"Stop!" -> ok
		end
	 end
	}.


% start_coordinator() ->
%     {"Start a coordinator, and nothing else",
%      fun() ->
%      	PidA = list_to_pid("<0.10.0>"),
%      	PidB = list_to_pid("<0.20.0>"),
%      	PidC = list_to_pid("<0.30.0>"),
%         ?assertMatch({ok, _}, rps_coordinator:start({PidA, make_ref(), 10, {PidB, "bob"}, {PidC, "alice"}}))
%      end}.

broker_handle_queue_up_game_found() ->
    {"Handle queue up message in RPS broker",
     fun() ->
     	PidA = list_to_pid("<0.10.0>"),
     	PidB = list_to_pid("<0.20.0>"),
     	Resp = rps_broker:handle_call({q_up, "Bob", 4},
     								  {PidA, "bob"},
     								  {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 0, running}),
     	?assertMatch({reply, {ok, "Alice", _}, _}, Resp)
     end}.

broker_handle_statistics() ->
    {"Handle statistics message in RPS broker",
     fun() ->
     	PidA = list_to_pid("<0.10.0>"),
     	PidB = list_to_pid("<0.20.0>"),
     	Resp = rps_broker:handle_call(statistics,
     								  {PidA, "bob"},
     								  {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 10, running}),
     	?assertMatch({reply, {ok, 10, 1, 0}, _}, Resp)
     end}.

broker_handle_drain() ->
    {"Handle drain message in RPS broker",
     fun() ->
     	PidB = list_to_pid("<0.20.0>"),
     	rps_broker:handle_cast({drain, self(), "STOP! HAMMER TIME!"},
							   {#{4 => {{PidB, "alice"}, "Alice"}}, #{}, 10, running}),
		receive
     		Y ->
     			?assertMatch({_, drain_complete}, Y)
     	end,
		receive
     		X ->
     			?assertMatch("STOP! HAMMER TIME!", X)
     	end
     end}.


% Dumb state-by-state unit tests
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

coordinator_handle_paper_paper() ->
    {"Handle paper input in paper state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_tie(),
     	Res = rps_coordinator:paper({call, {PidB, "alice"}}, paper, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_paper_scissors() ->
    {"Handle scissors input in paper state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_win(),
     	Res = rps_coordinator:paper({call, {PidB, "alice"}}, scissors, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_paper_rock() ->
    {"Handle rock input in paper state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_loose(),
     	Res = rps_coordinator:paper({call, {PidB, "alice"}}, rock, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_paper_invalid() ->
    {"Handle invalid input in paper state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_loose(),
     	Res = rps_coordinator:paper({call, {PidB, "alice"}}, invalid, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_scissors_paper() ->
    {"Handle paper input in scissors state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_loose(),
     	Res = rps_coordinator:scissors({call, {PidB, "alice"}}, paper, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_scissors_scissors() ->
    {"Handle scissors input in scissors state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_tie(),
     	Res = rps_coordinator:scissors({call, {PidB, "alice"}}, scissors, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_scissors_rock() ->
    {"Handle rock input in scissors state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_win(),
     	Res = rps_coordinator:scissors({call, {PidB, "alice"}}, rock, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_scissors_invalid() ->
    {"Handle invalid input in scissors state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_loose(),
     	Res = rps_coordinator:scissors({call, {PidB, "alice"}}, invalid, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_invalid_paper() ->
    {"Handle paper input in invalid state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_win(),
     	Res = rps_coordinator:invalid({call, {PidB, "alice"}}, paper, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_invalid_scissors() ->
    {"Handle scissors input in invalid state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_win(),
     	Res = rps_coordinator:invalid({call, {PidB, "alice"}}, scissors, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_invalid_rock() ->
    {"Handle rock input in invalid state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_win(),
     	Res = rps_coordinator:invalid({call, {PidB, "alice"}}, rock, State),
     	?assertMatch({next_state, no_move, ExpState}, Res)
     end}.

coordinator_handle_invalid_invalid() ->
    {"Handle invalid input in invalid state in RPS coordinator",
     fun() ->
     	{PidB, State, ExpState} = move_test_setup_tie(),
     	Res = rps_coordinator:invalid({call, {PidB, "alice"}}, invalid, State),
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