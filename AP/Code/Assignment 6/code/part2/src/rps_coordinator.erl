-module(rps_coordinator).
-behaviour(gen_statem).


-export([start/1, move/2, stop/0]).
-export([no_move/3, rock/3, paper/3, scissors/3]).
-export([init/1, callback_mode/0, code_change/4, terminate/3]).

start(State) -> 
    gen_statem:start(?MODULE, State, []).

move(Coordinator, Choice) -> 
    gen_statem:call(Coordinator, {self(), Choice}, infinity).

stop(Coordinator) ->
    gen_statem:stop(Coordinator).

%%%%%%%% 
callback_mode() ->
    state_functions.

init({BrokerRef, CoordRef, TargetRounds, PidA, PidB}) -> 
    {ok, no_move, {{BrokerRef, CoordRef}, TargetRounds, {PidA, 0}, {PidB, 0}}}.

terminate(normal, _, {_, _, {PidA, _}, {PidB, _}}) ->
    gen_statem:reply(PidA, server_stopping),
    gen_statem:reply(PidB, server_stopping),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

%%%% State functions
% finishing
no_move(call, _, {{BrokerRef, CoordRef}, TargetRounds, {PidA, WinsA}, {PidB, WinsB}}) 
    when (WinsB >= (TargetRounds div 2)) or (WinsA >= (TargetRounds div 2)) ->
        gen_statem:reply(PidA, {game_over, WinsA, WinsB}),
        gen_statem:reply(PidB, {game_over, WinsB, WinsA}),
        gen_server:cast(BrokerRef, {game_over, self(), CoordRef, WinsA+WinsB}),
        stop;

no_move(call, {Pid, Choice}, State) ->
    case Choice of
        rock -> {next_state, rock, State};
        paper -> {next_state, paper, State};
        scissors -> {next_state, scissors, State};
        _ -> {next_state, no_move, lose(Pid, State)}
    end.
    
    
rock(call, {Pid, Choice}, State) ->
    case Choice of
        rock -> 
            {next_state, no_move, tie(State)};
        paper -> 
            {next_state, no_move, win(Pid, State)};
        scissors ->
            {next_state, no_move, lose(Pid, State)}
    end.
        
paper(call, {Pid, Choice}, State) ->
        case Choice of
            rock -> 
                {next_state, no_move, lose(Pid, State)};
            paper -> 
                {next_state, no_move, tie(State)};
            scissors ->
                {next_state, no_move, win(Pid, State)}
        end.
scissors(call, {Pid, Choice}, State) -> 
        case Choice of
            rock -> 
                {next_state, no_move, win(Pid, State)};
            paper -> 
                    {next_state, no_move, lose(Pid, State)};
            scissors ->
                {next_state, no_move, tie(State)}
        end.










%% pls don't scroll further. nothing to see down there.
























































win(Pid, {Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB}}) 
    when Pid == PidA ->
        gen_statem:reply(PidA, round_won),
        gen_statem:reply(PidB, round_lost),
        {Refs, TargetRounds, {PidA, WinsA+1}, {PidB, WinsB}};

win(Pid, {Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB}}) 
    when Pid == PidB ->
        gen_statem:reply(PidA, round_lost),
        gen_statem:reply(PidB, round_won),
        {Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB+1}}.

lose(Pid, {Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB}}) 
    when Pid == PidA ->
        gen_statem:reply(PidA, round_lost),
        gen_statem:reply(PidB, round_won),
        {Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB+1}};

lose(Pid, {Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB}}) 
    when Pid == PidB ->
        gen_statem:reply(PidA, round_won),
        gen_statem:reply(PidB, round_lost),
        {Refs, TargetRounds, {PidA, WinsA+1}, {PidB, WinsB}}.

tie({Refs, TargetRounds, {PidA, WinsA}, {PidB, WinsB}}) ->
    gen_statem:reply(PidA, tie),
    gen_statem:reply(PidB, tie),
    {Refs, TargetRounds + 2, {PidA, WinsA + 1}, {PidB, WinsB + 1}}.
