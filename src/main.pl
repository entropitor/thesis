:- module(main, [test/0, test/1, test/2]).

:- use_module(problems).
:- use_module(parser).

test :-
    %parse_problem(vakantiedagen, X),
    parse_problem(zebra, Y),
    %X == nil,
    Y == nil.
test(Sentence) :-
    parse_sentences([Sentence], _).
test(Name, N) :-
    problem(Name, Sentences),
    nth1(N, Sentences, Sentence),
    test(Sentence).
