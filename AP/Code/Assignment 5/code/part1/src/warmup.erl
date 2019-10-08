-module(warmup).
-export([move/2,
	     makeEmptyDict/0,
	     insert/3,
	     lookup/2,
	     calculate/1]).

% direction is one of the atoms north, south, east or west

move(north, {X, Y}) -> {X, Y+1};
move(west,  {X, Y}) -> {subOrExcept(X), Y};
move(south, {X, Y}) -> {X, subOrExcept(Y)};
move(east,  {X, Y}) -> {X+1, Y}.

subOrExcept(X) -> if
				  	X - 1 < 0 -> throw("Negative coordinate!");
				  	true -> X - 1
				  end.

% Make empty tree
makeEmptyDict() -> {node, none, none, leaf, leaf}.

% Case for an empty tree
insert(Key, Value, {node, none, none, leaf, leaf}) -> {node, Key, Value, leaf, leaf};

% Cases for inserting Key higher then current key, for both when we reached the bottom and not
insert(Key, Value, {node, K, V, L, R}) when Key > K -> {node, K, V, L, insert(Key, Value, R)};
insert(Key, Value, {node, K, V, L, R}) when Key < K -> {node, K, V, insert(Key, Value, L), R};
insert(Key, Value, leaf) -> {node, Key, Value, leaf, leaf};
% Update value at found key
insert(Key, Value, {node, K, _, L, R}) when Key == K -> {node, K, Value, L, R}.

% Very similar implementation for lookup, but we return the value istead of recursively inserting
lookup(_, {node, none, none, leaf, leaf}) -> none;
lookup(Key, {node, K, _, _, leaf}) when Key > K -> none;
lookup(Key, {node, K, _, _, R}) when Key > K -> lookup(Key, R);
lookup(Key, {node, K, _, leaf, _}) when Key < K -> none;
lookup(Key, {node, K, _, L, _}) when Key < K -> lookup(Key, L);
lookup(Key, {node, K, V, _, _}) when Key == K -> {ok, V};
lookup(_, leaf) -> none.

% Non-mandatory part of the warmup
% Example of an aritmetic AST tree:
% {node, "+", {node, "*", 10, 20}, {node, "-", 50, 5}}

calculate(X) when is_integer(X) -> X;
calculate({node, "+", L, R}) -> calculate(L) + calculate(R);
calculate({node, "*", L, R}) -> calculate(L) * calculate(R);
calculate({node, "-", L, R}) -> calculate(L) - calculate(R).

