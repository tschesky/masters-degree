% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%

equal([], []).
equal([A | R], [A|R2]) :- equal(R, R2).

append([], Y, Y).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

%copied from tau-pl - seelct(E, L1, L2) is true if and only if we get L2 by removing E from L1
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

%get_following(G, X, L) is true, iff L is the list of persons X follows in G.
get_following([person(X, L)|_], X, L).
get_following([_|R], X, Y) :- get_following(R, X, Y). 

% all_different(G, X, L) checks if all persons in L are different from X in graph G --  enumerates all posibilites
check_all_different(G, X, L) :- all_persons(G, AP), check_all_different_p(G, X, L, AP).
% check_all_different_p(G, X, L) -
check_all_different_p(_, _, [], _).
check_all_different_p(G, X, [Y | R], FL) :- select(Y, FL, SL), different(G, X, Y), check_all_different_p(G, X, R, SL).

% check_all_different(_, _, []). % :- different(G, X, Y).
% check_all_different(G, X, [Y | R]) :- different(G, X, Y), check_all_different(G, X, R). 

% creates list of different people, only one result
get_all_different(G, X, Y) :- get_all_different_p(G, G, X, Y).
 
% get_all_different_p(CG, G, X, L)
get_all_different_p(G, [person(Y, _)], X, [Y]) :- different(G, X, Y).
get_all_different_p(_, [person(X, _)], X, []).
get_all_different_p(G, [person(Y, _) | GR], X, [Y | R]) :- different(G, X, Y), get_all_different_p(G, GR, X, R).
get_all_different_p(G, [person(X,_) | GR], X, R) :- get_all_different_p(G, GR, X, R).



% not_follows(G, X, Y) :- get_following(G, X, L), all_different(G, Y, L).
% ignores(G, X, Y)
ignores(G, X, Y) :- get_following(G, X, L),
                    follows(G, Y, X),
                    check_all_different(G, Y, L).                    

%%% level 1 %%%

% all_follow(G, L, X) true whenever all members of L follow X
all_follow(_, [], _).
all_follow(G, [P | R], X) :- follows(G, P, X), all_follow(G, R, X).
% popular(G, X) 

popular(G, X) :- get_following(G, X, L), all_follow(G, L, X).


all_ignore(_, [], _).
all_ignore(G, [P | R], X) :- ignores(G, P, X), all_ignore(G, R, X).
% outcast(G, X)
outcast(G, X) :- get_following(G, X, L), all_ignore(G, L, X).



% get_followers(G, X, L) - true iff the list L contains all followers of X in G
get_followers(G, X, L) :- get_followers_p(G, G, X, L).
% get_followers_p(CG, G, X, L) - subroutine that always has the complete initial graph as first argument
get_followers_p(_, [], _, []).
get_followers_p(G, [person(Y, L)|GR], X, [Y|R]) :- elem(X, L), get_followers_p(G, GR, X, R).
get_followers_p(G, [person(_, L)|GR], X, R) :- check_all_different(G, X, L), get_followers_p(G, GR, X, R).

% follows_all(G, X, L) - true if X follows all persons in L in G
follows_all(_, _, []).
follows_all(G, X, [Y|R]) :- follows(G, X, Y), follows_all(G, X, R).


% friendly(G, X)
friendly(G, X) :- get_followers(G, X, L), follows_all(G, X, L).

% hostile(G, X)
ignores_all(_, _, []).
ignores_all(G,X, [Y|R]) :- ignores(G, X, Y), ignores_all(G, X, R).

hostile(G, X) :- get_followers(G, X, L), ignores_all(G, X, L).

%%% level 2 %%%

% aware(G, X, Y)

aware(G, X, Y) :- aware_p(G, X, Y, [X]).

aware_p(G, X, Y, _) :- follows(G, X, Y).
aware_p(G, X, Y, ACC) :- follows(G, X, Z),
                         check_all_different(G, Y, ACC),
                         check_all_different(G, Z, ACC),
                         aware_p(G, Z, Y, [Z | ACC]).

% all_persons(G, L) retreives list containing all persons of G
all_persons([], []).
all_persons([person(X, _) | R1], [X | R]) :- all_persons(R1, R).


% acc_aware(CG, G, X, L).
% acc_aware(G, X, L) :- get_following(G, X, L).                             
% acc_aware(G, X, R) :- get_following(G, X, [Y | L]),
%                       elem(L, Y),
%                       check_all_different(G, Y, R),
%                       append(L, SHIT, R),
                    %   acc_aware(G, Y, SHIT).

% get_followers_inc(G, X, L, ACC).

followed_by_any(G, X, [Y | _]) :- follows(G, Y, X).
followed_by_any(G, X, [_ | R]) :- followed_by_any(G, X, R).


get_followers_inc(G, X, L) :- get_followers_inc_p(G, [X], [X | L]).

% get_followers_inc_p(G, )
% get_followers_inc_p(G, R, [Y | R1]) :- followed_by_any(G, Y, R),
%                                        check_all_different(G, Y, R1),
%                                        get_followers_inc(G, [Y | R], R1).


get_followers_inc_p(G, [], _).
get_followers_inc_p(G, [X | R], [X, Y | L]) :- follows(G, X, Y),
                                      check_all_different(G, Y, L),
                                      get_followers_inc(G, [Y | R], L).




% ignorant(G, X, Y)
ignorant(G, X, Y) :- different(G, X, Y), get_aware(G, X, L), check_all_different(G, Y, L).

% ignorant_p(G, X, Y, [Z | ACC]) :- aware(G, X, Z), ignorant_p()
%%% level 3 %%%

% same_world(G, H, K)

% optional!
% different_world(G, H)
