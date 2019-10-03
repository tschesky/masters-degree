-module(mood).
-export([server/0, try_it/2]).

moo(Pid, _, _) ->
    Pid ! moo,
    {200, "text/plain", "That's funny"}.


mood(Pid, _, _) ->
    Pid ! {mood, self()},
    receive
        {mood, sad} -> {200, "text/plain", "Sad"};
        {mood, happy} -> {200, "text/plain", "Happy!"}
    end.

mood_loop(State) ->
    receive
        {mood, Pid} ->
            Pid ! {mood, State},
            mood_loop(State);
        moo ->
            mood_loop(happy)
    end.

server() ->
    {ok, F} = flamingo:start("The Flamingo Server"),
    Pid = spawn(fun() -> mood_loop(sad) end),
	flamingo:new_route(F, ["/moo"], fun(Path, Args) -> moo(Pid, Path, Args) end),
	flamingo:new_route(F, ["/mood"], fun(Path, Args) -> mood(Pid, Path, Args) end),
	F.

try_it(Server, Path) ->
	Ref = make_ref(),
	flamingo:request(Server, {Path, []}, self(), Ref),
	receive
        {Ref, Reply} -> Reply
    end.
