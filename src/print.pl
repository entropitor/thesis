:- module(print, [show/1]).
:- use_module(dcg_macro).

show(X) :-
    writeln(X),
    writeln('---'),
    show(X, print{}.default()-_),
    nl, nl, nl.

show([], In-In) :-
    !.
show([H], In-Out) :-
    !,
    show(H, In-Out).
show([H | T], In-Out) :-
    !,
    show(H, In-Temp),
    write(' ??? '),
    show(T, Temp-Out).
show(=(X, Y), In-Out) :-
    !,
    show(X, In-Temp),
    write('='),
    show(Y, Temp-Out).
show(+(X, Y), In-Out) :-
    !,
    show(X, In-Temp),
    write('+'),
    show(Y, Temp-Out).
show(-(X, Y), In-Out) :-
    !,
    show(X, In-Temp),
    write('-'),
    show(Y, Temp-Out).
show(abs(X), In-Out) :-
    !,
    write('|'),
    show(X, In-Out),
    write('|').
show(literal(X), In-In) :-
    !,
    write(X).
show(predicate(X, rev(Y), Z), In-Out) :-
    !,
    show(predicate(X, Z, Y), In-Out).
show(predicate(X, Y, Z), In-Out) :-
    !,
    write(X),
    write('('),
    show_argument(Y, In-Temp),
    write(', '),
    show_argument(Z, Temp-Out),
    write(')').
show(property(X, Y), In-Out) :-
    !,
    write(X),
    write('('),
    show_argument(Y, In-Out),
    write(')').
show(exists(X, Y), In-Out) :-
    !,
    write('? '),
    show_argument(X, In-Temp),
    write(': '),
    show(Y, Temp-Out).
show(quantified(exactly(literal(Times)), X), In-Out) :-
    !,
    write('?='),
    write(Times),
    write(' '),
    show_argument(X, In-Out).
show(and(X, Y), In-Out) :-
    !,
    show(X, In-Temp),
    write(' & '),
    show(Y, Temp-Out).
show(if(Cond, Expr), In-Out) :-
    !,
    show(Expr, In-Temp),
    write(' <- '),
    show(Cond, Temp-Out).
show(Y, In-In) :-
    !,
    write("Unknown: { "),
    write(Y),
    write(' }').


show_argument(literal(Name, _Variable), In-In) :-
    !,
    write(Name).
show_argument(literal(Name), In-In) :-
    !,
    write(Name).
show_argument(var(X, _Type), In-Out) :-
    !,
    Out = In.get_variable_name(X, Y),
    write(Y).
show_argument(Y, In-In) :-
    !,
    write(Y).

_.default() := print{variables: [], counter: 1}.
%In.add_variable(Variable) := In.put(variables, [Variable | In.variables]) :- true.

In.get_variable_name(named(X), X) := In.
In.get_variable_name(unnamed(X), Y) := In.put([variables=[X=Y | In.variables], counter=Counter1]) :-
    var(X),
    !,
    atom_concat(x, In.counter, Y),
    X=Y,
    Counter1 is In.counter + 1.
In.get_variable_name(unnamed(X), Y) := In :-
    member(X=Y, In.variables),
    !.

