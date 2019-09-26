% AP2019 Assignment 3
% Skeleton for warm-up part. Predicates to implement:

% add(N1, N2, N)
add(N, z, N).
add(N1, s(N2), s(N)) :- add(N1, N2, N).

% mult(N1, N2, N)
mult(z, _, z).
mult(s(N1), N2, N) :- add(T, N2, N), mult(N1, N2, T).


% comp(N1, N2, A)
comp(z, z, eq).
comp(s(z), z, gt).
comp(z, s(z), lt).
comp(s(N1), s(N2), A) :- comp(N1, N2, A).

% insert(N, TI, TO)
insert(N, leaf, node(N, leaf, leaf)).
insert(N, node(N1, leaf, leaf), node(N1, node(N, leaf, leaf), leaf)) :- comp(N, N1, lt).
insert(N, node(N1, leaf, leaf), node(N1, leaf, node(N, leaf, leaf))) :- comp(N, N1, gt).
insert(N, node(N1, T1, T2), node(N1, T1, T2)) :- comp(N, N1, eq).
 % make sure it's a node so we don't pattern-match leaves again
insert(N, node(N1, node(T1N, T11, T12), _), TO) :- comp(N,N1, lt), insert(N, node(T1N, T11, T12), TO).
insert(N, node(N1, _, node(T2N, T21, T22)), TO) :- comp(N,N1, gt), insert(N, node(T2N, T21, T22), TO).

% insertlist(Ns, TI, TO)
insertlist([], TI, TI).
insertlist([N | Ns], TI, TO) :- insert(N, TI, TT), insertlist(Ns, TT, TO).

