-module(hello).
-export([server/0, try_it/2]).

hello(_, _) ->
	{200, "text/plain", "Hello my dear friend"}.

goodbye(_, _) ->
	{200, "text/plain", "Sad to see you go already."}.

server() ->
	{ok, F} = flamingo:start("The Flamingo Server"),
	flamingo:new_route(F, ["/hello"], fun hello/2),
	flamingo:new_route(F, ["/goodbye"], fun goodbye/2),
	F.

try_it(Server, Path) ->
	Ref = make_ref(),
	flamingo:request(Server, {Path, []}, self(), Ref),
	receive
        {Ref, Reply} -> Reply
    end.

