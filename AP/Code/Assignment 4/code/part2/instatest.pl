% Rudimentary test suite. Feel free to replace anything

% Can run as: swipl -g run_tests -g halt instahub.pl instatest.pl

% The sample graphs from the assignment text:
g1([person(kara, [barry, clark]),
    person(bruce,[clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

g1_popular([person(kara, [barry, clark, oliver]),
            person(bruce,[clark, oliver]),
            person(barry, [kara, oliver]),
            person(clark, [oliver, kara]),
            person(oliver, [kara, barry])]).

g2([person(batman, [green_arrow, superman]),
    person(green_arrow, [supergirl]),
    person(supergirl, [flash, superman]),
    person(flash, [green_arrow, supergirl]),
    person(superman, [green_arrow, supergirl])]).

:- begin_tests(instahub).

test(follows1, [nondet]) :-
    g1(G), follows(G, bruce, clark).

test(follows2, [fail]) :-
    g1(G), follows(G, clark, bruce).

test(follows3, [set(X == [barry,clark,oliver])]) :-
    g1(G), follows(G, X, kara).

test(ignores1, [nondet]) :-
    g1(G), ignores(G, clark, bruce).

test(ignores2, [fail]) :-
    g1(G), ignores(G, bruce, bruce).

test(ignores3, [set(X == [bruce,barry,clark])]) :-
    g1(G), ignores(G, oliver, X).

test(ignores4, [set(X == [clark, oliver])]) :-
    g1(G), ignores(G, X, bruce).

test(popular1, [nondet]) :-
    g1(G), popular(G, kara).

test(popular2, [nondet]) :-
    g1_popular(G), popular(G, kara).

test(popular3, [nondet]) :-
    g1_popular(G), popular(G, oliver).

test(popular4, [fail]) :-
    g1_popular(G), popular(G, bruce).

test(popular5, [set(X == [kara, barry, oliver])]) :-
    g1_popular(G), popular(G, X).

test(outcast1, [nondet]) :-
    g1(G), outcast(G, bruce).

test(outcast2, [fail]) :-
    g1(G), outcast(G, clark).

test(friendly, [nondet]) :-
    g1(G), friendly(G, barry).

test(friendly2, [set(X == [barry, bruce])]) :-
    g1(G), friendly(G, X).

test(friendly3, [fail]) :-
    g1(G), friendly(G, kara).

test(hostile, [nondet]) :-
    g1(G), hostile(G, bruce).

test(hostile2, [set(X == [bruce, oliver])]) :-
    g1(G), hostile(G, X).

test(hostile3, [fail]) :-
    g1(G), hostile(G, clark).

test(aware1, [nondet]) :-
    g1(G), aware(G, kara, barry).

test(aware2, [set(X == [barry, clark, oliver])]) :-
    g1(G), aware(G, kara, X).

test(aware3, [fail]) :-
    g1(G), aware(G, kara, bruce).

:- end_tests(instahub).
