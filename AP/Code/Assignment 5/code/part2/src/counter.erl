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
    Pid = spawn(fun() -> counter_loop(0) end),
	flamingo:new_route(F, ["/inc_with"], fun(Req, Env) -> inc(Pid, Req, Env) end),
	flamingo:new_route(F, ["/dec_with"], fun(Req, Env) -> dec(Pid, Req, Env) end),
	F.

counter_loop(Counter) ->
    receive
        {inc, Pid, Val} ->
            NewCount = Counter + Val,
            Pid ! {count, NewCount},
            counter_loop(NewCount);
        {dec, Pid, Val} ->
            NewCount = Counter - Val,
            Pid ! {count, NewCount},
            counter_loop(NewCount)
    end.

try_it(Server, Path, Args) ->
	Ref = make_ref(),
	flamingo:request(Server, {Path, Args}, self(), Ref),
	receive
        {Ref, Reply} -> Reply
    end.
