-module(rps_broker).
-export([start/0, queue_up/3, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/3, terminate/2, code_change/3]).
-behaviour(gen_server).


start() -> gen_server:start({local, rps}, ?MODULE, [], []).

queue_up(BrokerRef, Name, Rounds) -> gen_server:call(BrokerRef, {q_up, Name, Rounds}, infinity).

statistics(BrokerRef) -> nope.

drain(BrokerRef, Pid, Msg) -> nope.

game_over(BrokerRef, ) -> 

%callback impls

init(_) -> {ok, {#{}, [], 0}}.

handle_call({q_up, Name, Rounds}, From, State) -> 
    case maps:get(Rounds, State) of 
        Player -> 
            CoordRef = make_ref(),
            rps_coordinator:start(self(), CoordRef, Player, From);
        _ -> 


handle_cast({game_over, ....})

handle_info
terminate
code_change