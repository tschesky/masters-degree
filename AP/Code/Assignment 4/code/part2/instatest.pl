% Rudimentary test suite. Feel free to replace anything

% Can run as: swipl -g run_tests -g halt instahub.pl instatest.pl

% The sample graphs from the assignment text:
g0([
    person(kara, [test, clark]),
    person(test, [])
    ]).

g1([person(kara, [barry, clark]),
    person(bruce,[clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])
    ]).

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

:- end_tests(instahub).
