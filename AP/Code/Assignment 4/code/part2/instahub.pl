% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%   

% "Pure prolog" nonsense, so override "in"
inList(ELEM, [ELEM | _]).
inList(ELEM, [_ | TAIL]) :- inList(ELEM, TAIL).

% No good way to implement that, as only facts not stated are considered to be true
% ...
equalLists([], []).
equalLists([H | T1], [H | T2]) :- equalLists(T1, T2).

sameLengthLists([], []).
sameLengthLists([_ | T1], [_ | T2]) :- sameLengthLists(T1, T2).

equalLists2_([], _).
equalLists2_([H | T1], L2) :- inList(H, L2),
							  equalLists2_(T1, L2).
equalLists2(L1, L2) :- sameLengthLists(L1, L2),
					   equalLists2_(L1, L2).

selectCustom(X, [Head|Tail], Rest) :-
    selectCustom3_(Tail, Head, X, Rest).

selectCustom3_(Tail, Head, Head, Tail).
selectCustom3_([Head2|Tail], Head, X, [Head|Rest]) :-
    selectCustom3_(Tail, Head2, X, Rest).

notInList(ELEM, LIST) :- selectCustom(ELEM, LIST, TMP),
						 equalLists(LIST, TMP).

%follows(G, X, Y)
follows([person(FOLLOWER, FOLLOWS) | _], FOLLOWER, FOLLOWED) :- inList(FOLLOWED, FOLLOWS).
follows([_ | TAIL], FOLLOWER, FOLLOWED) :- follows(TAIL, FOLLOWER, FOLLOWED).

%different(G, X, Y)
different([person(PERSON1, _) | REST], PERSON1, PERSON2) :- inList(person(PERSON2, _), REST). % We found person 1, now check if person 2 is in remainder of the graph
different([person(PERSON2, _) | REST], PERSON1, PERSON2) :- inList(person(PERSON1, _), REST). % Symmetric case
different([_ | REST], PERSON1, PERSON2) :- different(REST, PERSON1, PERSON2).                 % Just traverse the graph

% Again - need to have a separate function for iterating over graphs for each predicate, as I cant use "impure" prolog
% for passing predicates as function arguments...
allDifferent(_, _, []).
allDifferent(G, PERSON1, [PERSON2 | REST]) :- different(G, PERSON1, PERSON2),
											  allDifferent(G, PERSON1, REST).

%doesNotFollow(G, X, Y)
doesNotFollow(G, FOLLOWER, NOT_FOLLOWED) :- getFollowed(G, FOLLOWER, FOLLOWED),
										    allDifferent(G, NOT_FOLLOWED, FOLLOWED).

%ingores(G, X, Y)
ignores(HUB, IGNORES, IGNORED) :- different(HUB, IGNORES, IGNORED),
								  follows(HUB, IGNORED, IGNORES),
								  doesNotFollow(HUB, IGNORES, IGNORED).

%%% level 1 %%%

% Get people that a particular person follows
% Good note for report - I had to implement it after trying to iterate over the graph in each separate function
% This however, left me with modified graph (only the remainder) after the person of intereet. Therefore had to delegate it to auxiliary.
getFollowed([person(PERSON, FOLLOWERS) | _], PERSON, FOLLOWERS).
getFollowed([_ | REST], PERSON, FOLLOWERS) :- getFollowed(REST, PERSON, FOLLOWERS).

allFollowBack(_, _, []).
allFollowBack(G, PERSON, [FOLLOWED | REST]) :- follows(G, FOLLOWED, PERSON),
											   allFollowBack(G, PERSON, REST).

popular(G, PERSON) :- getFollowed(G, PERSON, FOLLOWERS),
					  allFollowBack(G, PERSON, FOLLOWERS).

allIgnore(_, _, []).
allIgnore(G, PERSON, [FOLLOWED | REST]) :- ignores(G, FOLLOWED, PERSON),
										   allIgnore(G, PERSON, REST).

outcast(G, PERSON) :- getFollowed(G, PERSON, FOLLOWERS),
					  allIgnore(G, PERSON, FOLLOWERS).

conditionalList(_, [], _, []).
conditionalList(FOLLOWED, [FOLLOWED | _], NAME, [NAME]).
conditionalList(FOLLOWED, [_ | REST], NAME , RETURN) :- conditionalList(FOLLOWED, REST, NAME, RETURN).

isEmpty(L, [], L).
isEmpty(L, R, RESULT) :- append(L, R, RESULT). 

followers([], _, _).
followers([person(NAME, FOLLOWERS) | REST], FOLLOWED, RETURN) :- conditionalList(FOLLOWED, FOLLOWERS, NAME, TMP),
																 append(RETURN, TMP, X),
																 followers(REST, FOLLOWED, X).

cool(_, _, _, _).
cool(G, COOL, COOL, NAME) :- follows(G, COOL, NAME).

isCool(_, _, [], _).
isCool(G, COOL, [HEAD | REST], NAME) :- cool(G, COOL, HEAD, NAME),
									    isCool(G, COOL, REST, NAME).

isFriendly(_, [], _).
isFriendly(G, [person(NAME, FOLLOWERS) | REST], COOL) :- isCool(G, COOL, FOLLOWERS, NAME),
														 isFriendly(G, REST, COOL).

friendly(G, X) :- isFriendly(G, G, X).

% hostile(G, X)

%%% level 2 %%%

% Convert this whole nonsense to graph-term form so its easier to actually search for paths
% Graph-term: g(E, V), where E is a list of edges and V is a list of getVertices
% Now both aware and ignorant should be a matter of finding a path in the graph
getVertices([], []).
getVertices([person(NAME, _) | REST], [NAME | V]) :- getVertices(REST, V).


toEdges(_, [], []).
toEdges(NAME, [H | T], [e(NAME, H) | X]) :- toEdges(NAME, T, X).

getEdges([], []).
getEdges([person(NAME, FOLLOWERS) | REST], [X | V]) :- toEdges(NAME, FOLLOWERS, X),
												       getEdges(REST, V).

toGraph(G, g(V, E)) :- getVertices(G, V),
					   getEdges(G, TMP),
					   appendLists(TMP, E).

inEdges([e(A, B) | _], A, B).
inEdges([_ | REST], A, B) :- inEdges(REST, A, B).

isEdge(g(V, E), A, B) :- inList(A, V),
						 inList(B, V),
						 inEdges(E, A, B).

path(G, A, B) :- walk(G, A, B, []).

walk(G, A, B, V) :- isEdge(G, A, X),
					not(inList(X, V)),         % Write a predicate for notInList...?
					(
						B = X
					;
						walk(G, X, B, [A | V])
					)
					.

% Auxiliary function for concatenation of list of ListOfList
% Its obviously just copy-paste of implementation form SWI, but hey - "Pure Prolog", amirite?
appendLists(ListOfLists, List) :-
    appendLists_(ListOfLists, List).

appendLists_([], []).
appendLists_([L|Ls], As) :-
    appendLists(L, Ws, As),
    appendLists_(Ls, Ws).

appendLists([], L, L).
appendLists([H|T], L, [H|R]) :-
    appendLists(T, L, R).

aware(G, X, Y) :- different(G, X, Y),
				  toGraph(G, GT),
				  path(GT, X, Y).

ignorant(G, X, Y) :- different(G, X, Y), 
					 not(aware(G, X, Y)).   % Meh...

%%% level 3 %%%

permuteCustom([], []).
permuteCustom(L, [P | P1]) :-
    selectCustom(P, L, L1),
    permuteCustom(L1, P1).

listOfPermutations(L, OUT) :- findall(X, permuteCustom(L, X), OUT).

packList([], [], []).
packList([H1 | T1], [H2 | T2], [p(H1, H2) | REST]) :- packList(T1, T2, REST).

substituteV(X, [p(X, TMP) | _], TMP).
substituteV(X, [_ | TAIL], TMP) :- substituteV(X, TAIL, TMP).

substituteEdges([], _, []).
substituteEdges([e(X, Y) | TAIL], SUBLIST, [e(TMP, TMP2) | REST]) :- substituteV(X, SUBLIST, TMP),
																     substituteV(Y, SUBLIST, TMP2),
																     substituteEdges(TAIL, SUBLIST, REST).

substituteGraph(g(V1, E1), V2, g(V2, E2)) :- packList(V1, V2, SUBLIST),
											 substituteEdges(E1, SUBLIST, E2).

sameGraph(g(V1, E1), g(V2, E2)) :- equalLists2(V1, V2),
								   equalLists2(E1, E2).

doWork(g(V1,E1), SUB, H, K) :- substituteGraph(g(V1, E1), SUB, OUTG),
							   sameGraph(H, OUTG),
							   packList(V1, SUB, K).

iterateSubstitutions(g(V1, E1), H, [SUB | _], K) :- doWork(g(V1,E1), SUB, H, K).
iterateSubstitutions(g(V1, E1), H, [_ | TAIL], K) :- iterateSubstitutions(g(V1, E1), H, TAIL, K).

cmpWorlds(g(V1, E1), g(V2, E2), K) :- listOfPermutations(V2, V2MIXED),
									  iterateSubstitutions(g(V1, E1), g(V2, E2), V2MIXED, K).

same_world(G, H, K) :- toGraph(G, GT),
					   toGraph(H, HT),
					   cmpWorlds(GT, HT, K).

% optional!
% different_world(G, H)
