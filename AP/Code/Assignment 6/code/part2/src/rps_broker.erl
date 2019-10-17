-module(rps_broker).
-export([start/0, queue_up/3, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).


start() -> gen_server:start({local, rps}, ?MODULE, [], []).

queue_up(BrokerRef, Name, Rounds) -> gen_server:call(BrokerRef, {q_up, Name, Rounds}, infinity).

statistics(BrokerRef) -> gen_server:call(BrokerRef, statistics, infinity).

drain(BrokerRef, Pid, Msg) -> gen_server:cast(BrokerRef, {drain, Pid, Msg}).


%callback impls

init(_) -> {ok, {#{}, #{}, 0}}.

% Note - PidA that we get passed to the callback function is actually of form {Pid,Tag}...
% From gen_server documentation:
% From is a tuple {Pid,Tag}, where Pid is the pid of the client and Tag is a unique tag.
handle_call({q_up, Name, Rounds}, PidA, {Queue, Coords, Longest}=State) -> 
    case maps:find(Rounds, Queue) of 
        {ok, {PidB, Name2}} ->
            NewQ = maps:remove(Rounds, Queue),
            CoordRef = make_ref(),
            case rps_coordinator:start({self(), CoordRef, Rounds}) of
                {ok, Coord} ->
                    NewCoords = maps:put(Coord, CoordRef, Coords),
                    gen_server:reply(PidB, {ok, Name, Coord}),
                    {reply, {ok, Name2, Coord}, {NewQ, NewCoords, Longest}};
                {error, Reason} -> 
                    gen_server:reply(PidB, {error, Reason}),
                    {reply, {error, Reason}, State}
            end;
        _ ->
            NewQ = maps:put(Rounds, {PidA, Name}, Queue),
            {noreply, {NewQ, Coords, Longest}}
    end;

handle_call(statistics, _, {Queue, Coords, Longest}=State) -> 
    {reply, {ok, Longest, maps:size(Queue), maps:size(Coords)}, State}.

% This should probably be handled as a cast? Does sending a mesessage "!" count
% towards returning from handle_call function?
handle_cast({drain, Pid, Msg}, {_, Coords, _}) ->
    BrokerRef = self(),
    spawn(fun() -> 
        maps:map(fun(Key, _) -> rps_coordinator:stop(Key) end, Coords),
        Pid ! Msg,
        gen_server:cast(BrokerRef, drain_complete)
    end);

handle_cast(drain_complete, _) ->
    {stop, server_drained, {}};

handle_cast({game_over, Coord, CoordRef, GameLength}, {Q, Coords, Longest})
    when GameLength > Longest ->
        NewCoords = removeCoord(Coord, Coords, CoordRef),
        {noreply, {Q, NewCoords, GameLength}};

handle_cast({game_over, Coord, CoordRef, _}, {Q, Coords, Longest}) ->
        NewCoords = removeCoord(Coord, Coords, CoordRef),
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
