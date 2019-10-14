-module(rps_broker).
-export([start/0, queue_up/3, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).


start() -> gen_server:start({local, rps}, ?MODULE, [], []).

queue_up(BrokerRef, Name, Rounds) -> gen_server:call(BrokerRef, {q_up, Name, Rounds}, infinity).

statistics(BrokerRef) -> gen_server:call(BrokerRef, statistics, infinity).

drain(BrokerRef, Pid, Msg) -> gen_server:cast(BrokerRef, {drain, Pid, Msg}).


%callback impls

init(_) -> {ok, {#{}, [], 0}}.

handle_call({q_up, Name, Rounds}, PidA, {Queue, Coords, Longest}=State) -> 
    case maps:find(Rounds, Queue) of 
        {ok, {PidB, Name2}} ->
            maps:remove(Rounds, Queue), 
            CoordRef = make_ref(),
            case rps_coordinator:start(self(), CoordRef, PidA, PidB) of
                {ok, Coord} ->
                    maps:put(Coord, CoordRef, Coords),
                    gen_server:reply(PidB, {ok, Name, Coord}),
                    {reply, {ok, Name2, Coord}, State};
                {error, Reason} -> 
                    gen_server:reply(PidB, {error, Reason}),
                    {reply, {error, Reason}, State}
            end;
        _ ->
            NewQ = maps:put(Rounds, {PidA, Name}, Queue),
            {noreply, {NewQ, Coords, Longest}}
    end;

handle_call(statistics, _, {Queue, Coords, Longest}=State) -> 
    {reply, {ok, Longest, maps:size(Queue), maps:size(Coords)}, State};

handle_call({drain, Pid, Msg}, _, {_, Coords, _}) ->
    BrokerRef = self(),
    spawn(fun() -> 
        maps:map(fun(Key, _) -> rps_coordinator:stop(Key) end, Coords),
        Pid ! Msg,
        gen_server:cast(BrokerRef, drain_complete)
    end).

handle_cast(drain_complete, _) ->
    {stop, server_drained, {}};

handle_cast({game_over, From, CoordRef, GameLength}, {Q, Coords, Longest})
    when GameLength > Longest ->
        NewCoords = removeCoord(From, Coords, CoordRef),
        {noreply, {Q, NewCoords, GameLength}};

handle_cast({game_over, From, CoordRef, _}, {Q, Coords, Longest}) ->
        NewCoords = removeCoord(From, Coords, CoordRef),
        {noreply, {Q, NewCoords, Longest}}.



handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.


% helper function to handle HIGH!!! SECURITY SECTION
removeCoord(Coord, Coords, CoordRef) ->
    case maps:find(Coord, Coords) of
        {ok, CoordRef} ->
            maps:remove(Coord, Coords);
        _ -> 
           Coords
    end.
