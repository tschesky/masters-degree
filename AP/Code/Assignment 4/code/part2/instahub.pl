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


allDifferent(_, _, []).
allDifferent(G, PERSON1, [PERSON2 | REST]) :- different(G, PERSON1, PERSON2),
											  allDifferent(G, PERSON1, REST).

%doesNotFollow(G, X, Y)
doesNotFollow(G, FOLLOWER, NOT_FOLLOWED) :- followers(G, FOLLOWER, FOLLOWED),
										    allDifferent(G, NOT_FOLLOWED, FOLLOWED).

%ingores(G, X, Y)
ignores(HUB, IGNORES, IGNORED) :- different(HUB, IGNORES, IGNORED),
								  follows(HUB, IGNORED, IGNORES),
								  doesNotFollow(HUB, IGNORES, IGNORED).

%%% level 1 %%%

% Popular

% Get people that a particular person follows
% Good note for report - I had to implement it after trying to iterate over the graph in each separate function
% This however, left me with modified graph (only the remainder) after the person of intereet. Therefore had to delegate it to auxiliary.
followers([person(PERSON, FOLLOWERS) | _], PERSON, FOLLOWERS).
followers([_ | REST], PESRON, FOLLOWERS) :- followers(REST, PESRON, FOLLOWERS).

allFollowBack(_, _, []).
allFollowBack(G, PERSON, [FOLLOWED | REST]) :- follows(G, FOLLOWED, PERSON),
											   allFollowBack(G, PERSON, REST).

popular(G, PERSON) :- followers(G, PERSON, FOLLOWERS),
					  allFollowBack(G, PERSON, FOLLOWERS).

% Outcast 

allIgnore(_, _, []).
allIgnore(G, PERSON, [FOLLOWED | REST]) :- ignores(G, FOLLOWED, PERSON),
										   allIgnore(G, PERSON, REST).

outcast(G, PERSON) :- followers(G, PERSON, FOLLOWERS),
					  allIgnore(G, PERSON, FOLLOWERS).

% friendly(G, X)

% hostile(G, X)

%%% level 2 %%%

% aware(G, X, Y)

% ignorant(G, X, Y)

%%% level 3 %%%

% same_world(G, H, K)

% optional!
% different_world(G, H)
