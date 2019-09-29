% AP2019 Assignment 3
% Skeleton for warm-up part. Predicates to implement:

numeral(z).

numeral(s(X)) :- numeral(X).

add(z, N2, N2).

% add(N1, z, N1). - not needed, as we would succeed more than once on add(z, z, X)

add(s(N1), N2, N) :- add(N1, s(N2), N).

mult(z, _, z).

% mult(s(z), _, s(z)). - pointless base case, as above

mult(s(N1), N2, N) :- mult(N1, N2, X), add(N2, X, N).

comp(z, z, eq).

comp(z, _, lt).

comp(_, z, gt).

comp(s(N1), s(N2), OP) :- comp(N1, N2, OP).

insert(X, leaf, node(X, leaf, leaf)).

insert(X, node(N1, L1, R1), node(N2, L2, R2)) :-
	( comp(X, N1, lt) ->
		insert(X, L1, Y),
		(L2, N2, R2) = (Y, N1, R1);
	  comp(X, N1, gt) ->
	  	insert(X, R1, Y),
	  	(L2, N2, R2) = (L1, N1, Y);
	  (L2, N2, R2) = (L1, N1, R1)).

insertlist([], T1, T1).
insertlist([H | R], T1, TR) :- insert(H, T1, TMP), insertlist(R, TMP, TR).


% Some syntactic sugar for switch maybe...?
switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).