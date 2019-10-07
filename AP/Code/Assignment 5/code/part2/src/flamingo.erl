-module(flamingo).

-export([start/1, new_route/3, request/4, drop_route/2, lookup_route/2]).
-import(lists, [filter/2, foldl/3]).

get_response(Routing, Env, {Path, _}=Req) -> 
	try
		case lookup_route(Routing, Path) of
			none -> {404, "text/html", ""};
			{_,_, Action} -> 
				% Quick and dirty - but we need to do sth like that here anyway, there was a discussion
				% On absalon that we should convert stupid output from Actions into reasonable response format
				case Action(Req, Env) of
				 	{200, _, _}=Resp -> Resp;
				 	{404, _, _}=Resp -> Resp;
				 	{500, _, _}=Resp -> Resp;
				 	_ -> {500, "text/html", ""}
				end
		end
	catch
		throw :  e -> {500, "text/html", ""};
		error : e -> {500, "text/html", ""};
		exit :   e -> {500, "text/html", ""}
	end.

lookup_route(Routing, Path) -> 
	Route = lists:foldl(fun(X, Longest) -> filter_path(Path, X, Longest) end, {"", none, none}, Routing),
	case Route of
		{"", _, _} -> none;
		_ -> Route
	end.

filter_path(Path, {X, _, _}=X1, {L, _, _}=L1) -> case string:prefix(X, Path) of
								         	         nomatch -> L1;
								        	         _ -> case (length(X) > length(L)) of
								        		 	          true -> X1;
								        		 	       false -> L1
								          		          end
								                     end.

add_routes(Routing, Prefixes, Action) ->
	try
		Id = make_ref(),
		TmpList = [{X, Id, Action} || X <- Prefixes],
		{ok, Id, TmpList ++ Routing}
	catch
		throw: e -> {error, e};
		exit: e -> {error, e};
		error: e -> {error, e}
	end.

loop(Routing, Env) ->
	receive
		{register, Pid, Prefixes, Action} -> Res =  add_routes(Routing, Prefixes, Action), % TODO: error handling
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
		{error, Reason} -> 	{error, Reason};
		_ -> blah
	end.
		
drop_route(_Flamingo, _Id) ->
    not_implemented.
