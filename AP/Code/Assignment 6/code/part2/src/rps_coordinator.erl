-module(rps_coordinator).
-behaviour(gen_statem).


-export([start/1, move/2, stop/1]).
-export([no_move/3, rock/3, paper/3, scissors/3, invalid/3, stopping1/3, stopping2/3]).
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

terminate(_, _, _) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

%%%% State functions

% stopping({call, From}, _, {_, _, PlayerInfo})
%     when (map_size(PlayerInfo) == 1) ->
%         gen_statem:reply(From, server_stopping),
%         ok;
stopping1({call, From}, _, State) ->
    {_,_, NewPlayerInfo} = update_tag(From, State),
    maps:map(fun(_, {Info, _}) -> gen_statem:reply(Info, server_stopping) end, NewPlayerInfo),
    timer:sleep(10),
    {stop, normal};
stopping1(cast, stopping, State) ->
    {keep_state, State}.

stopping2({call, From}, _, State) ->
    {next_state, stopping1, update_tag(From, State)};
stopping2(cast, stopping, State) ->
    {keep_state, State}.


no_move({call, From}, Choice, State) ->
    case Choice of
        rock ->
            {next_state, rock, update_tag(From, State)};
        paper ->
            {next_state, paper, update_tag(From, State)};
        scissors ->
            {next_state, scissors, update_tag(From, State)};
        _ ->
            {next_state, invalid, update_tag(From, State)}
    end;
no_move(cast, stopping, State) ->
    {next_state, stopping2, State}.

invalid({call, {Pid, _}=From}, Choice, State) ->
    case Choice of
        rock -> 
            {next_state, no_move, win(Pid, update_tag(From,State))};
        paper -> 
            {next_state, no_move, win(Pid, update_tag(From,State))};
        scissors ->
            {next_state, no_move, win(Pid, update_tag(From,State))};
        _ -> 
            {next_state, no_move, tie(update_tag(From, State))}
    end;
invalid(cast, stopping, State) ->
    {next_state, stopping1, State}.

rock({call, {Pid, _}=From}, Choice, State) ->
    case Choice of
        rock -> 
            {next_state, no_move, tie(update_tag(From, State))};
        paper -> 
            {next_state, no_move, win(Pid, update_tag(From,State))};
        scissors ->
            {next_state, no_move, lose(Pid, update_tag(From, State))};
        _ ->
            {next_state, no_move, lose(Pid, update_tag(From, State))}
    end;
rock(cast, stopping, State) ->
    {next_state, stopping1, State}.
        
paper({call, {Pid, _}=From}, Choice, State) ->
        case Choice of
            rock -> 
                {next_state, no_move, lose(Pid, update_tag(From, State))};
            paper -> 
                {next_state, no_move, tie(update_tag(From, State))};
            scissors ->
                {next_state, no_move, win(Pid, update_tag(From, State))};
            _ ->
                {next_state, no_move, lose(Pid, update_tag(From, State))}
        end;
paper(cast, stopping, State) ->
    {next_state, stopping1, State}.

scissors({call, {Pid, _}=From}, Choice, State) -> 
        case Choice of
            rock -> 
                {next_state, no_move, win(Pid, update_tag(From, State))};
            paper -> 
                {next_state, no_move, lose(Pid, update_tag(From, State))};
            scissors ->
                {next_state, no_move, tie(update_tag(From, State))};
            _ ->
                {next_state, no_move, lose(Pid, update_tag(From, State))}
        end;
scissors(cast, stopping, State) ->
    {next_state, stopping1, State}.


update_tag({Pid, _}=From, {Refs, Rounds, PlayerInfo}) ->
    case maps:find(Pid, PlayerInfo) of 
        {ok, {_, Wins}} -> 
            {Refs, Rounds, maps:update(Pid, {From, Wins}, PlayerInfo)};
        _ ->
            {Refs, Rounds, maps:put(Pid, {From, 0}, PlayerInfo)}
    end.
    

add_win(Pid, PlayerInfo) ->
    maps:update_with(Pid, fun({Info, Wins}) -> {Info, Wins+1} end, PlayerInfo).

% check_game_over will return either {gameOver, InfoA, WinsA, InfoB, WinsB} or {notYet, InfoA, InfoB}
check_game_over(TargetRounds, PlayerInfo) ->
    Wins = get_wins_from_playerinfo(PlayerInfo),
    check_wins(TargetRounds, Wins).

check_wins(TargetRounds, {{InfoA, WinsA}, {InfoB, WinsB}})
    when (WinsA > (TargetRounds div 2)) or (WinsB > (TargetRounds div 2)) ->
        {gameOver, InfoA, WinsA, InfoB, WinsB};
check_wins(_, _) -> notYet.

get_wins_from_playerinfo(PlayerInfo) ->
    [{InfoA, WinsA}, {InfoB, WinsB}] = maps:values(PlayerInfo),
    {{InfoA, WinsA}, {InfoB, WinsB}}.

playerinfos(PlayerInfo) ->
    {{InfoA, _}, {InfoB, _}} = get_wins_from_playerinfo(PlayerInfo),
    {InfoA, InfoB}.

% returns tuple wiht infos for Pid and the other player, in that order!
playerinfos(Pid, PlayerInfo) ->
    case get_wins_from_playerinfo(PlayerInfo) of
        {{{PidA, _} = InfoA, _}, {InfoB, _}} when PidA == Pid ->
            {InfoA, InfoB};
        {{InfoA, _}, {{PidB, _}=InfoB, _}} when PidB == Pid ->
            {InfoB, InfoA}
    end.
        
    % when (WinsA > (TargetRounds div 2)) or (WinsB > (TargetRounds div 2))  ->

win(Pid, {{BrokerRef, CoordRef}=Refs, {TargetRounds, CurrentRounds}, PlayerInfo}) ->
    NewPlayerInfo = add_win(Pid, PlayerInfo),
    case check_game_over(TargetRounds, NewPlayerInfo) of
        notYet ->
            {This, Other} = playerinfos(Pid, NewPlayerInfo),
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
    {This, {PidO, _}=Other} = playerinfos(Pid, PlayerInfo),
    NewPlayerInfo =  add_win(PidO, PlayerInfo), 
    case check_game_over(TargetRounds, NewPlayerInfo) of
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
    {A, B} = playerinfos(PlayerInfo),
    gen_statem:reply(A, tie),
    gen_statem:reply(B, tie),
    {Refs, {TargetRounds, CurrentRounds+1}, PlayerInfo}.
