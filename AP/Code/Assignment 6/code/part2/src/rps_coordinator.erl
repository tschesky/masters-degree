-module(rps_coordinator).
-behaviour(gen_statem).


-export([start/1, move/2, stop/1]).
-export([no_move/3, rock/3, paper/3, scissors/3, invalid/3, stopping/3]).
-export([init/1, callback_mode/0, code_change/4, terminate/3]).

start(State) -> 
    gen_statem:start(?MODULE, State, []).

move(Coordinator, Choice) -> 
    gen_statem:call(Coordinator, Choice, infinity).

stop(Coordinator) ->
    gen_statem:cast(Coordinator, stopping).

%%%%%%%% 
callback_mode() ->
    state_functions.

init({BrokerRef, CoordRef, TargetRounds, {PidA, _}=PlayerA, {PidB, _}=PlayerB}) ->
    PlayerInfo = maps:put(PidB, {PlayerB, 0}, maps:put(PidA, {PlayerA, 0}, #{})),
    {ok, no_move, {{BrokerRef, CoordRef}, {TargetRounds, 0}, PlayerInfo}}.

terminate(normal, _, {_, _, PlayerInfo}) ->
    erlang:display(PlayerInfo),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

%%%% State functions

stopping({call, From}, _, {_, _, PlayerInfo})
    when (map_size(PlayerInfo) == 1) ->
        gen_statem:reply(From, server_stopping),
        ok;
stopping({call, {Pid, _}=From}, _, {Refs, Rounds, PlayerInfo}) ->
    gen_statem:reply(From, server_stopping),
    {keep_state, {Refs, Rounds, maps:remove(Pid, PlayerInfo)}};
stopping(cast, stopping, State) ->
    {keep_state, State}.


no_move({call, From}, Choice, State) ->
    case Choice of
        rock ->
            {next_state, rock, updateTag(From, State)};
        paper ->
            {next_state, paper, updateTag(From, State)};
        scissors ->
            {next_state, scissors, updateTag(From, State)};
        _ ->
            {next_state, invalid, updateTag(From, State)}
    end;
no_move(cast, stopping, State) ->
    {next_state, stopping, State}.

invalid({call, {Pid, _}=From}, Choice, State) ->
    case Choice of
        rock -> 
            {next_state, no_move, win(Pid, updateTag(From,State))};
        paper -> 
            {next_state, no_move, win(Pid, updateTag(From,State))};
        scissors ->
            {next_state, no_move, win(Pid, updateTag(From,State))};
        _ -> 
            {next_state, no_move, tie(updateTag(From, State))}
    end;
invalid(cast, stopping, State) ->
    {next_state, stopping, State}.

rock({call, {Pid, _}=From}, Choice, State) ->
    case Choice of
        rock -> 
            {next_state, no_move, tie(updateTag(From, State))};
        paper -> 
            {next_state, no_move, win(Pid, updateTag(From,State))};
        scissors ->
            {next_state, no_move, lose(Pid, updateTag(From, State))};
        _ ->
            {next_state, no_move, lose(Pid, updateTag(From, State))}
    end;
rock(cast, stopping, State) ->
    {next_state, stopping, State}.
        
paper({call, {Pid, _}=From}, Choice, State) ->
        case Choice of
            rock -> 
                {next_state, no_move, lose(Pid, updateTag(From, State))};
            paper -> 
                {next_state, no_move, tie(updateTag(From, State))};
            scissors ->
                {next_state, no_move, win(Pid, updateTag(From, State))};
            _ ->
                {next_state, no_move, lose(Pid, updateTag(From, State))}
        end;
paper(cast, stopping, State) ->
    {next_state, stopping, State}.

scissors({call, {Pid, _}=From}, Choice, State) -> 
        case Choice of
            rock -> 
                {next_state, no_move, win(Pid, updateTag(From, State))};
            paper -> 
                {next_state, no_move, lose(Pid, updateTag(From, State))};
            scissors ->
                {next_state, no_move, tie(updateTag(From, State))};
            _ ->
                {next_state, no_move, lose(Pid, updateTag(From, State))}
        end;
scissors(cast, stopping, State) ->
    {next_state, stopping, State}.


updateTag({Pid, _}=From, {Refs, Rounds, PlayerInfo}) ->
    case maps:find(Pid, PlayerInfo) of 
        {ok, {_, Wins}} -> 
            {Refs, Rounds, maps:update(Pid, {From, Wins}, PlayerInfo)};
        _ ->
            {Refs, Rounds, maps:put(Pid, {From, 0}, PlayerInfo)}
    end.
    

addWin(Pid, PlayerInfo) ->
    maps:update_with(Pid, fun({Info, Wins}) -> {Info, Wins+1} end, PlayerInfo).

% checkGameOver will return either {gameOver, InfoA, WinsA, InfoB, WinsB} or {notYet, InfoA, InfoB}
checkGameOver(TargetRounds, PlayerInfo) ->
    Wins = getWinsFromPlayerInfo(PlayerInfo),
    checkWins(TargetRounds, Wins).

checkWins(TargetRounds, {{InfoA, WinsA}, {InfoB, WinsB}})
    when (WinsA > (TargetRounds div 2)) or (WinsB > (TargetRounds div 2)) ->
        {gameOver, InfoA, WinsA, InfoB, WinsB};
checkWins(_, _) -> notYet.

getWinsFromPlayerInfo(PlayerInfo) ->
    [{InfoA, WinsA}, {InfoB, WinsB}] = maps:values(PlayerInfo),
    {{InfoA, WinsA}, {InfoB, WinsB}}.

playerInfos(PlayerInfo) ->
    {{InfoA, _}, {InfoB, _}} = getWinsFromPlayerInfo(PlayerInfo),
    {InfoA, InfoB}.

% returns tuple wiht infos for Pid and the other player, in that order!
playerInfos(Pid, PlayerInfo) ->
    case getWinsFromPlayerInfo(PlayerInfo) of
        {{{PidA, _} = InfoA, _}, {InfoB, _}} when PidA == Pid ->
            {InfoA, InfoB};
        {{InfoA, _}, {{PidB, _}=InfoB, _}} when PidB == Pid ->
            {InfoB, InfoA}
    end.
        
    % when (WinsA > (TargetRounds div 2)) or (WinsB > (TargetRounds div 2))  ->

win(Pid, {{BrokerRef, CoordRef}=Refs, {TargetRounds, CurrentRounds}, PlayerInfo}) ->
    NewPlayerInfo = addWin(Pid, PlayerInfo),
    case checkGameOver(TargetRounds, NewPlayerInfo) of
        notYet ->
            {This, Other} = playerInfos(Pid, NewPlayerInfo),
            gen_statem:reply(This, round_won),
            gen_statem:reply(Other, round_lost),
            {Refs, {TargetRounds, CurrentRounds+1}, NewPlayerInfo};
        {gameOver, InfoA, WinsA, InfoB, WinsB} ->
            gen_statem:reply(InfoA, {game_over, WinsA, WinsB}),
            gen_statem:reply(InfoB, {game_over, WinsB, WinsA}),
            gen_server:cast(BrokerRef, {game_over, self(), CoordRef, CurrentRounds+1}),
            {stop, normal, {none, none, #{}}}
    end.

lose(Pid, {{BrokerRef, CoordRef}=Refs, {TargetRounds, CurrentRounds}, PlayerInfo}) ->
    {This, {PidO, _}=Other} = playerInfos(Pid, PlayerInfo),
    NewPlayerInfo =  addWin(PidO, PlayerInfo), 
    case checkGameOver(TargetRounds, NewPlayerInfo) of
        notYet ->
            gen_statem:reply(This, round_lost),
            gen_statem:reply(Other, round_won),
            {Refs, {TargetRounds, CurrentRounds+1}, NewPlayerInfo};
        {gameOver, InfoA, WinsA, InfoB, WinsB} ->
            gen_statem:reply(InfoA, {game_over, WinsA, WinsB}),
            gen_statem:reply(InfoB, {game_over, WinsB, WinsA}),
            gen_server:cast(BrokerRef, {game_over, self(), CoordRef, CurrentRounds+1}),
            {stop, normal, {none, none, #{}}}
    end.


tie({Refs, {TargetRounds, CurrentRounds}, PlayerInfo}) ->
    {A, B} = playerInfos(PlayerInfo),
    gen_statem:reply(A, tie),
    gen_statem:reply(B, tie),
    {Refs, {TargetRounds, CurrentRounds+1}, PlayerInfo}.
