-module(rps).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).

start() -> rps_broker:start().

queue_up(BrokerRef, Name, Rounds) -> rps_broker:queue_up(BrokerRef, Name, Rounds).

move(Coordinator, Choice) -> rps_coordinator:move(Coordinator, Choice).

statistics(BrokerRef) -> nope.

drain(BrokerRef, Pid, Msg) ->  nope.


