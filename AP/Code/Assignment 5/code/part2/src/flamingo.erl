-module(flamingo).

-export([start/1, new_route/3, request/4, drop_route/2, lookup_route/2]).
-import(lists, [filter/2]).

get_response(Routing, Env, {Path, _}=Req) -> 
	try
		case lookup_route(Routing, Path) of
			none -> {404, "text/html", ""};
			{_,_, Action} -> Action(Req, Env)
		end
	catch
		throw :  e -> {500, "text/html", ""};
		error : e -> {500, "text/html", ""};
		exit :   e -> {500, "text/html", ""}
	end.

lookup_route(Routing, Path) -> 
	Candidates = lists:filter(fun({X, _, _}) -> filter_path(Path, X) end, Routing),
	case Candidates of
		[] -> none;
		_  -> get_longest_prefix(Candidates, {"", none, none})
	end.
	
get_longest_prefix([], Longest) -> Longest;
get_longest_prefix([{Prefix, _, _}=Head | Tail], {Longest_Prefix, _, _})
								when (length(Prefix) > length(Longest_Prefix)) -> get_longest_prefix(Tail, Head);
get_longest_prefix([_ | Tail], Longest) -> get_longest_prefix(Tail, Longest).


filter_path(Path, Prefix) -> 	case string:prefix(Path, Prefix) of
									nomatch -> false;
									_ -> true
								end.

add_routes(Routing, Prefixes, Action) ->
	try
		Id = make_ref(),
		TmpList = [{X, Id, Action} || X <- Prefixes],
		{ok, Id, TmpList ++ Routing}
	catch
		throw: e -> {error, e};
		error: e -> {error, e}
	end.

loop(Routing, Env) ->
	receive
		{register, Pid, Prefixes, Action} -> Res = add_routes(Routing, Prefixes, Action), % TODO: error handling
												case Res of
													{ok, Id, NewRouting} -> 
														Pid ! {ok, Id},
														loop(NewRouting, Env);
													{error, e} -> 
														Pid ! {error, e},
														loop(Routing, Env)
												end;
		{request, Req, From, Ref} ->  Res = get_response(Routing, Env, Req),
									  From ! {Ref, Res},
									  loop(Routing, Env)
	end.

start(Global) ->
	try 
    	{ok, spawn(fun() -> loop([], Global) end)}
	catch
		error: e -> {error, e}
	end.

request(Flamingo, Request, From, Ref) ->
    Flamingo ! {request, Request, From, Ref}.

new_route(Flamingo, Prefixes, Action) ->
    Flamingo ! {register, self(), Prefixes, Action},
	receive
		{ok, Id} 		->	{ok, Id};
		{error, Reason} -> 	{error, Reason}
	end.
		
drop_route(_Flamingo, _Id) ->
    not_implemented.
