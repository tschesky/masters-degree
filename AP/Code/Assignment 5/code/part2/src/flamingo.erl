-module(flamingo).

-export([start/1, new_route/3, request/4, drop_route/2]).
-import(maps, [from_list/1,
			   merge/2]).

add_to_env(Env, Prefixes, Action) ->
	TmpList = [X, Y || X <- Prefixes, Y <- Action],
	TmpDict = maps:from_list(TmpList),
	maps:merge(Env, TmpDict).

flamingo_main_function(Env) ->
	receive
		{register, Prefixes, Action} -> NewEnv = add_to_env(Env, Prefixes, Action),
		flamingo_main_function(NewEnv)
	end.

start(_Global) ->
    nope.

request(_Flamingo, _Request, _From, _Ref) ->
    nope.

new_route(_Flamingo, _Prefixes, _Action) ->
    nope.

drop_route(_Flamingo, _Id) ->
    not_implemented.
