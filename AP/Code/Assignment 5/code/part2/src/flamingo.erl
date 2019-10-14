-module(flamingo).

-export([start/1, new_route/3, request/4, drop_route/2, get_routes/1]).
-import(lists, [filter/2]).

handle_response(From, Ref, Routing, Env, {Path, _}=Req) -> 
	case lookup_route(Routing, Path) of
		none -> From ! {Ref, {404, "text/html", ""}};
		{_,_, Action} -> spawn(fun() -> call_action(From, Ref, Action, Req, Env) end)
	end.


call_action(From, Ref, Action, Req, Env) ->
	try
		Res = Action(Req, Env),
		% Make sure the result conforms to expected format
		case Res of
		 	{200, _, _} -> From ! {Ref, Res};
		 	{404, _, _} -> From ! {Ref, Res};
		 	{500, _, _} -> From ! {Ref, Res};
		 	_ -> From ! {Ref, {500, "text/html", ""}}
		end
	catch
		throw :  _ -> From ! {Ref, {500, "text/html", ""}};
		error : _ -> From ! {Ref, {500, "text/html", ""}};
		exit :   _ -> From ! {Ref, {500, "text/html", ""}}
	end.

lookup_route(Routing, Path) -> 
	Route = lists:foldr(fun(X, Longest) -> filter_path(Path, X, Longest) end, {"", none, none}, Routing),
	case Route of
		{"", _, _} -> none;
		_ -> Route
	end.

filter_path(Path, {X, _, _}=X1, {L, _, _}=L1) -> case string:prefix(Path, X) of
								         	         nomatch -> L1;
								        	         _ -> case (length(X) > length(L)) of
								        		 	          true -> X1;
								        		 	       false -> L1
								          		          end
								                     end.

drop_route_by_id(Routing, Id) -> 
	lists:filter(fun({_, Route_Id, _}) -> Route_Id /= Id end, Routing).

add_routes(Routing, Prefixes, Action) ->
	try
		Id = make_ref(),
		TmpList = [{X, Id, Action} || X <- Prefixes],
		{ok, Id, Routing ++ TmpList}
	catch
		throw: E -> {error, E};
		error: E -> {error, E}
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
		{request, Req, From, Ref} ->  handle_response(From, Ref, Routing, Env, Req),
									  loop(Routing, Env);
		{drop, Id} -> 	
			NewRouting = drop_route_by_id(Routing, Id),
			loop(NewRouting, Env);
		{get, From} -> 
			From ! Routing,
			loop(Routing, Env)
	end.

start(Global) ->
	try 
    	{ok, spawn(fun() -> loop([], Global) end)}
	catch
		error: E -> {error, E}
	end.

request(Flamingo, Request, From, Ref) ->
    Flamingo ! {request, Request, From, Ref}.

new_route(Flamingo, Prefixes, Action) ->
    Flamingo ! {register, self(), Prefixes, Action},
	receive
		{ok, Id} 		->	{ok, Id};
		{error, Reason} -> 	{error, Reason}
	end.
		
drop_route(Flamingo, Id) ->
    Flamingo ! {drop, Id}.

get_routes(Flamingo) -> 
	Flamingo ! {get, self()}.