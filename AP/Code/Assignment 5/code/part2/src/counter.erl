-module(counter).
-export([server/0, try_it/3]).

inc(Pid, {_, Args}, _) ->
    Val = get_arg_x(Args),
    Pid ! {inc, self(), Val},
    receive
        {count, Count} -> {200,"text/plain", integer_to_list(Count)}
    end.

dec(Pid, {_, Args}, _) ->
    Val = get_arg_x(Args),
    Pid ! {dec, self(), Val},
    receive
        {count, Count} -> {200,"text/plain", integer_to_list(Count)}
    end.

get_arg_x([]) -> 1;
get_arg_x([{Var, Val} | _]) when Var == "x" -> parse_arg(Val);
get_arg_x([_ | Tail]) -> get_arg_x(Tail).

parse_arg(Val) -> {IntVal, Rest} = string:to_integer(Val),
                  case Rest of
                      [] ->
                          if
                              (IntVal > 0) -> IntVal;
                              true -> 1
                          end;
                      _ -> 1
                  end.

server() ->
    {ok, F} = flamingo:start("The Flamingo Server"),
    % Doesn't matter on what value Super starts, the worker is going to update him on first call anyway
    Super = spawn(fun() -> process_flag(trap_exit, true), 
    					   Worker = spawn_link(fun() -> counter_loop(0) end),
    					   supervisor(Worker, 0) end),
	flamingo:new_route(F, ["/inc_with"], fun(Req, Env) -> inc(Super, Req, Env) end),
	flamingo:new_route(F, ["/dec_with"], fun(Req, Env) -> dec(Super, Req, Env) end),
	F.

try_it(Server, Path, Args) ->
	Ref = make_ref(),
	flamingo:request(Server, {Path, Args}, self(), Ref),
	receive
        {Ref, Reply} -> Reply
    end.

supervisor(Worker, Counter) ->
	receive
		{'EXIT', _, _} ->
			NewWorker = spawn_link(fun() -> counter_loop(Counter) end),
			supervisor(NewWorker, Counter);
		{backup, NewCounter} ->
			supervisor(Worker, NewCounter);
		{count, Pid, NewCount} ->
			Pid ! {count, NewCount},
			supervisor(Worker, NewCount);
		Msg ->
			Worker ! {self(), Msg},
			supervisor(Worker, Counter)
	end.

% In the examples the worker loop has Super as an argument, but I don't see how this dependecy is beneficial tbh...
counter_loop(Counter) ->
    receive
        {Super, {inc, Pid, Val}} ->
            Super ! {backup, Counter},
            NewCount = Counter + Val,
            Super ! {count, Pid, NewCount},
            counter_loop(NewCount);
        {Super, {dec, Pid, Val}} ->
         	Super ! {backup, Counter},
            NewCount = Counter - Val,
            Super ! {count, Pid, NewCount},
            counter_loop(NewCount)
    end.