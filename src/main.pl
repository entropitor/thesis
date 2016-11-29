:- module(main, [test/0,
                 test/1,
                 test/2]).

:- use_module(problems).
:- use_module(parser).

test :-
    test_problems([zebra, extra]).
test(Sentence) :-
    parse_sentences([Sentence], _).
test(Name, N) :-
    problem(Name, Sentences),
    nth1(N, Sentences, Sentence),
    test(Sentence).

test_problems(Problems) :-
    maplist(parse_problem, Problems, WrongSentences),
    maplist(=(nil), WrongSentences).
