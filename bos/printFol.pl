:- module(printFol, [printFol/1]).

printFol(X) :-
    printFol(0, X).

printFol(_, que(X, R, B)) :-
    write('?'),
    print(X),
    write('['),
    print(R),
    write(', '),
    print(B),
    write(']').

printFol(Order, eq(X, Y)) :-
    printParantheses(Order, 2, (
                         print(X),
                         write(' = '),
                         print(Y)
                     )).
printFol(_, pred(Name, Arg)) :-
    X =.. [Name, Arg],
    write(X).
printFol(_, rel(Name, Arg1, Arg2)) :-
    X =.. [Name, Arg1, Arg2],
    write(X).

printFol(Order, some(X, F)) :-
    printQuantifier(Order, 0, X, F, '∃').
printFol(Order, all(X, F)) :-
    printQuantifier(Order, 0, X, F, '∀').

printFol(Order, not(F)) :-
    printUnaryConnector(Order, 1, F, '~ ').
printFol(Order, and(F1, F2)) :-
    printBinaryConnector(Order, 2, F1, F2, ' & ').
printFol(Order, or(F1, F2)) :-
    printBinaryConnector(Order, 3, F1, F2, ' v ').
printFol(Order, imp(F1, F2)) :-
    printBinaryConnector(Order, 4, F1, F2, ' => ').
printFol(Order, bimp(F1, F2)) :-
    printBinaryConnector(Order, 5, F1, F2, ' <=> ').

printUnaryConnector(Order, NewOrder, F, Connector) :-
    printParantheses(Order, NewOrder, (
                         write(Connector),
                         printFol(NewOrder, F)
                     )).
printBinaryConnector(Order, NewOrder, F1, F2, Connector) :-
    printParantheses(Order, NewOrder, (
                         printFol(NewOrder, F1),
                         write(Connector),
                         printFol(NewOrder, F2)
                     )).
printQuantifier(Order, NewOrder, X, F2, Quantifier) :-
    write(Quantifier),
    printVar(X),
    write(':'),
    tab(1),
    printParantheses(Order, NewOrder, (
                         printFol(NewOrder, F2)
                     )).

printParantheses(Order, NewOrder, Goal) :-
    Order < NewOrder,
    !,
    write('('),
    call(Goal),
    write(')').
printParantheses(_, _, Goal) :-
    call(Goal).


printVar(Var) :-
    nonvar(Var),
    Var = var(X, Type),
    !,
    format('~p [~p]', [X, Type]).
printVar(Var) :-
    write(Var).
