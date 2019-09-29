% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%   

% "Pure prolog" nonsense, so override "in"
inList(ELEM, [ELEM | _]).
inList(ELEM, [_ | TAIL]) :- inList(ELEM, TAIL).

% No good way to implement that, as only facts not stated are considered to be true
% notInList(ELEM, []).
% notInList(ELEM, [_ | TAIL]) :- notInList(ELEM, TAIL).

%follows(G, X, Y)
follows([person(FOLLOWER, FOLLOWS) | _], FOLLOWER, FOLLOWED) :- inList(FOLLOWED, FOLLOWS).
follows([_ | TAIL], FOLLOWER, FOLLOWED) :- follows(TAIL, FOLLOWER, FOLLOWED).

%different(G, X, Y)
different([person(PERSON1, _) | REST], PERSON1, PERSON2) :- inList(person(PERSON2, _), REST). % We found person 1, now check if person 2 is in remainder of the graph
different([person(PERSON2, _) | REST], PERSON1, PERSON2) :- inList(person(PERSON1, _), REST). % Symmetric case
different([_ | REST], PERSON1, PERSON2) :- different(REST, PERSON1, PERSON2).                 % Just traverse the graph


%doesNotFollow(G, X, Y)
doesNotFollow(G, FOLLOWER, FOLLOWED) :- follows(G, FOLLOWER, TMP),
										different(G, TMP, FOLLOWED).

%ingores(G, X, Y)
ignores(HUB, IGNORES, IGNORED) :- different(HUB, IGNORES, IGNORED),
								  follows(HUB, IGNORED, IGNORES),
								  doesNotFollow(HUB, IGNORES, IGNORED).

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
