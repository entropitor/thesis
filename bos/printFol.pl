:- module(printFol, [printFol/1, printFol/2]).

printFol(X) :-
    printFol(console, X).

printFol(Mode, X) :-
    printFol(Mode, 6, X),
    !,
    write('.'),
    nl.

printFol(_Mode, _, que(X, R, B)) :-
    write('?'),
    print(X),
    write('['),
    print(R),
    write(', '),
    print(B),
    write(']').

printFol(_, Order, eq(X, Y)) :-
    printParantheses(Order, 2, (
                         print(X),
                         write(' = '),
                         print(Y)
                     )).
printFol(_, Order, eq(X)) :-
    printParantheses(Order, 2, (
                         print(X)
                     )).
printFol(_, _, pred(Name, Arg)) :-
    X =.. [Name, Arg],
    write(X).
printFol(_, _, rel(Name, Arg1, Arg2)) :-
    X =.. [Name, Arg1, Arg2],
    write(X).

printFol(Mode, Order, some(X, some(X2, F))) :-
    !,
    printFol(Mode, Order, some([X, X2], F)).
printFol(Mode, Order, some(X, F)) :-
    printQuantifier(Mode, Order, [X], F, exists).
printFol(Mode, Order, all(X, all(X2, F))) :-
    !,
    printFol(Mode, Order, all([X, X2], F)).
printFol(Mode, Order, all(X, F)) :-
    printQuantifier(Mode, Order, [X], F, forall).

printFol(Mode, Order, not(F)) :-
    printUnaryConnector(Mode, Order, 1, F, '~ ').
printFol(Mode, Order, and(F1, F2)) :-
    printBinaryConnector(Mode, Order, 2, F1, F2, conjunction).
printFol(Mode, Order, or(F1, F2)) :-
    printBinaryConnector(Mode, Order, 3, F1, F2, disjunction).
printFol(Mode, Order, imp(F1, F2)) :-
    printBinaryConnector(Mode, Order, 4, F1, F2, implication).
printFol(Mode, Order, bimp(F1, F2)) :-
    printBinaryConnector(Mode, Order, 5, F1, F2, equivalence).

printUnaryConnector(Mode, Order, NewOrder, F, Connector) :-
    printParantheses(Order, NewOrder, (
                         write(Connector),
                         printFol(Mode, NewOrder, F)
                     )).
printBinaryConnector(Mode, Order, NewOrder, F1, F2, Connector) :-
    printParantheses(Order, NewOrder, (
                         printFol(Mode, NewOrder, F1),
                         getSymbol(Mode, Connector, ConnectorCode),
                         format(' ~w ', [ConnectorCode]),
                         printFol(Mode, NewOrder, F2)
                     )).
printQuantifier(Mode, Order, Xs, F2, Quantifier) :-
    NewOrder = 6,
    getSymbol(Mode, Quantifier, QuantifierSymbol),
    flatten(Xs, FlatXs),
    printParantheses(Order, NewOrder, (
                         write(QuantifierSymbol),
                         printQuantifierVars(FlatXs),
                         write(':'),
                         tab(1),
                         printFol(Mode, NewOrder, F2)
                     )).

getSymbol(idp, exists, '?').
getSymbol(idp, forall, '!').
getSymbol(idp, conjunction, '&').
getSymbol(idp, disjunction, '|').
getSymbol(idp, implication, '=>').
getSymbol(idp, equivalence, '<=>').

getSymbol(console, exists, '∃').
getSymbol(console, forall, '∀').
getSymbol(console, conjunction, '∧').
getSymbol(console, disjunction, '∨').
getSymbol(console, implication, '=>').
getSymbol(console, equivalence, '<=>').

printQuantifierVars([Var]) :-
    !,
    printVar(Var).
printQuantifierVars([Var | Vars]) :-
    printVar(Var),
    write(' '),
    printQuantifierVars(Vars).

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
    Var = variable(X, Type, decl),
    !,
    format('~p [~@]', [X, printVar(Type)]).
printVar(Var) :-
    nonvar(Var),
    Var = variable(X, Type, int),
    !,
    format('??~p [~@]??', [X, printVar(Type)]).
printVar(Var) :-
    write(Var).
