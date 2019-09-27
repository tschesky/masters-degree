% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%

equal([], []).
equal([A | R], [A|R2]) :- equal(R, R2).

append([], Y, Y).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

%copied from tau-pl
select(E, [E|Xs], Xs).
select(E, [X|Xs], [X|Ys]) :- select(E, Xs, Ys).

% check element of list
elem(X, [X|_]).
elem(X, [_|R]) :- elem(X, R). 

different([person(X, _) | R], X, Y) :- elem(person(Y, _), R).
different([person(Y, _) | R], X, Y) :- elem(person(X,_), R).
different([_|R], X, Y) :- different(R, X, Y). 

% follows(G, X, Y)
follows([person(X, L) | _], X, Y) :- elem(Y, L). 
follows([_|Pr], X, Y) :- follows(Pr, X, Y).

%get_followers(G, X, L) is true, iff L is the list of followers X has in G.
get_followers([person(X, L)|_], X, L).
get_followers([_|R], X, Y) :- get_followers(R, X, Y).


% all_different(G, X, L) checks if all persons in L are different from X in graph G
all_different(_, _, []). 
all_different(G, X, [Y | R]) :- different(G, X, Y), all_different(G, X, R). 

% ignores(G, X, Y)
ignores(G, X, Y) :- get_followers(G, X, L),
                    follows(G, Y, X),
                    all_different(G, Y, L).                    

%%% level 1 %%%

% popular(G, X)

% outcast(G, X)

% friendly(G, X)

% hostile(G, X)

%%% level 2 %%%

% aware(G, X, Y)

% ignorant(G, X, Y)

%%% level 3 %%%

% same_world(G, H, K)

% optional!
% different_world(G, H)
