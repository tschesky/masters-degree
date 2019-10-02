% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%   

% Check if element ins present in a given list
inList(ELEM, [ELEM | _]).
inList(ELEM, [_ | TAIL]) :- inList(ELEM, TAIL).

% Check if two lists are the same (including order of elements)
equalLists([], []).
equalLists([H | T1], [H | T2]) :- equalLists(T1, T2).

% Check if two lists have the same length
sameLengthLists([], []).
sameLengthLists([_ | T1], [_ | T2]) :- sameLengthLists(T1, T2).

% Check if two lists are the same (very weak comparison, but if we assume that a list contains unique elements - it works)
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

% follows(G, X, Y)
follows([person(FOLLOWER, FOLLOWS) | _], FOLLOWER, FOLLOWED) :- inList(FOLLOWED, FOLLOWS).
follows([_ | TAIL], FOLLOWER, FOLLOWED) :- follows(TAIL, FOLLOWER, FOLLOWED).

% different(G, X, Y)
different([person(PERSON1, _) | REST], PERSON1, PERSON2) :- inList(person(PERSON2, _), REST). % We found person 1, now check if person 2 is in remainder of the graph
different([person(PERSON2, _) | REST], PERSON1, PERSON2) :- inList(person(PERSON1, _), REST). % Symmetric case
different([_ | REST], PERSON1, PERSON2) :- different(REST, PERSON1, PERSON2).                 % Just traverse the graph

% Apply different over the whole list
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

% get_followers(G, X, L) - true iff the list L contains all followers of X in G
get_followers(G, X, L) :- get_followers_p(G, G, X, L).

% get_followers_p(CG, G, X, L) - subroutine that always has the complete initial graph as first argument
get_followers_p(_, [], _, []).
get_followers_p(G, [person(Y, L)|GR], X, [Y|R]) :- inList(X, L),
												   get_followers_p(G, GR, X, R).

get_followers_p(G, [person(_, L)|GR], X, R) :- allDifferent(G, X, L),
											   get_followers_p(G, GR, X, R).

% follows_all(G, X, L) - true if X follows all persons in L in G
follows_all(_, _, []).
follows_all(G, X, [Y|R]) :- follows(G, X, Y),
					        follows_all(G, X, R).

% friendly(G, X)
friendly(G, X) :- get_followers(G, X, L),
			      follows_all(G, X, L).

% hostile(G, X)
ignores_all(_, _, []).
ignores_all(G,X, [Y|R]) :- ignores(G, X, Y), ignores_all(G, X, R).

hostile(G, X) :- get_followers(G, X, L), ignores_all(G, X, L).

%%% level 2 %%%

% Convert this whole nonsense to graph-term form so its easier to actually search for paths
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

% Auxiliary function for concatenation of list of ListOfList
appendLists(ListOfLists, List) :-
    appendLists_(ListOfLists, List).

appendLists_([], []).
appendLists_([L|Ls], As) :-
    appendLists(L, Ws, As),
    appendLists_(Ls, Ws).

appendLists([], L, L).
appendLists([H|T], L, [H|R]) :-
    appendLists(T, L, R).

all_persons([], []).
all_persons([person(X, _) | R1], [X | R]) :- all_persons(R1, R).

check_all_different(G, X, L) :- all_persons(G, AP), check_all_different_p(G, X, L, AP).
check_all_different_p(_, _, [], _).
check_all_different_p(G, X, [Y | R], FL) :- select(Y, FL, SL), different(G, X, Y), check_all_different_p(G, X, R, SL).

aware(G, X, Y) :- aware_p(G, X, Y, [X]).

aware_p(G, X, Y, _) :- follows(G, X, Y).
aware_p(G, X, Y, ACC) :- follows(G, X, Z),
                         check_all_different(G, Y, ACC),
                         check_all_different(G, Z, ACC),
                         aware_p(G, Z, Y, [Z | ACC]).

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
