:- module(print, [show_rules/2]).
:- use_module(dcg_macro).

show_rules(Atoms, Facts) :-
    writeln('####################################################################'),
    atomics_to_string(Atoms, ' ', Sentence),
    wwriteln(Sentence),
    section,
    wwriteln(Facts),
    section,
    show(Facts, print{}.default()-Out),
    wwriteln(Out.body),
    section,
    wwriteln(Out.variables),
    wwriteln(Out.bound),
    section,
    maplist(show_quantified_variable(Out), Out.bound, Strs),
    atomics_to_string(Strs, ' ', Str),
    wwrite(Str),
    write(' '),
    writeln(Out.body),
    writeln('####################################################################'),
    !,
    nl, nl.
show(X) :-
    writeln('Unknown: FAILED ERROR'),
    writeln(X),
    nl,
    writeln('---').


wwrite(X) :-
    write('###   '),
    write(X).
wwriteln(X) :-
    wwrite(X),
    nl.
section :-
    wwriteln('--------------------------------------------------------   ###').

show([], In-In) :-
    !.
show([H], In-Out) :-
    !,
    show(H, In-Out).
show([H | T], In-Out) :-
    !,
    show(H, In-Out1),
    Out2 = Out1.write(' ??? '),
    show(T, Out2-Out).
show(=(X, Y), In-Out) :-
    !,
    show(X, In-Out1),
    Out2 = Out1.write('='),
    show(Y, Out2-Out).
show(+(X, Y), In-Out) :-
    !,
    show(X, In-Out1),
    Out2 = Out1.write('+'),
    show(Y, Out2-Out).
show(-(X, Y), In-Out) :-
    !,
    show(X, In-Out1),
    Out2 = Out1.write('-'),
    show(Y, Out2-Out).
show(abs(X), In-Out) :-
    !,
    Out1 = In.write('|'),
    show(X, Out1-Out2),
    Out = Out2.write('|').
show(literal(X), In-Out) :-
    !,
    Out = In.write(X).
show(predicate(X, Y, Z), In-Out) :-
    !,
    Out1 = In.write(X).write('('),
    show_argument(Y, Out1-Out2),
    Out3 = Out2.write(', '),
    show_argument(Z, Out3-Out4),
    Out = Out4.write(')').
show(property(X, Y), In-Out) :-
    !,
    Out1 = In.write(X).write('('),
    show_argument(Y, Out1-Out2),
    Out = Out2.write(')').
show(exists(X, Y), In-Out) :-
    !,
    Out1 = In.write('? '),
    show_argument(X, Out1-Out2),
    Out3 = Out2.bound_variable(X, exists),
    Out4 = Out3.write(': '),
    show(Y, Out4-Out).
show(quantified(Quantity, X), In-Out) :-
    Quantity = exactly(literal(Times)),
    !,
    Out1 = In.write('?=').write(Times).write(' '),
    show_argument(X, Out1-Out2),
    Out = Out2.bound_variable(X, quantified(Quantity)).
show(quantor(Quantor, Variable), In-Out) :-
    !,
    show_quantor(Quantor, In-Out1),
    Out2 = Out1.write(' '),
    show_argument(Variable, Out2-Out3),
    Out = Out3.bound_variable(Variable, Quantor).
show(and(X, Y), In-Out) :-
    !,
    show(X, In-Out1),
    Out2 = Out1.write(' & '),
    show(Y, Out2-Out).
show(if(Cond, Expr), In-Out) :-
    !,
    show(Expr, In-Out1),
    Out2 = Out1.write(' <- '),
    show(Cond, Out2-Out).
show(Y, In-Out) :-
    !,
    Out = In.write('Unknown: { ').write(Y).write(' }').

show_quantified_variable(_, _:literal(_, _), "") :-
    !.
show_quantified_variable(In, Quantor:Variable, Str) :-
    Out1 = In.reset_body(),
    show_quantor(Quantor, Out1-Out2),
    Out3 = Out2.write(' '),
    show_argument(Variable, Out3-Out4),
    Str = Out4.body.

show_quantor(exists, In-Out) :-
    !,
    Out = In.write('?').
show_quantor(forall, In-Out) :-
    !,
    Out = In.write('!').
show_quantor(quantified(exactly(literal(X))), In-Out) :-
    !,
    Out = In.write('?=').write(X).
show_quantor(quantified(at_least(literal(X))), In-Out) :-
    !,
    Out = In.write('?>=').write(X).write(' ').
show_quantor(X, In-Out) :-
    !,
    Out = In.write('Quantor(').write(X).write(')').

show_argument(literal(Name, _Variable), In-Out) :-
    !,
    Out = In.write(Name).
show_argument(literal(Name), In-Out) :-
    !,
    Out = In.write(Name).
show_argument(var(X, _Type), In-Out) :-
    !,
    Out1 = In.get_variable_name(X, Y),
    Out = Out1.write(Y).
show_argument(Y, In-Out) :-
    !,
    Out = In.write(Y).

_.default() := print{variables: [], counter: 1, bound: [], body: ""}.

In.get_variable_name(named(X), X) := Out :-
    (
        member(X=X, In.variables)
    ->
        Out = In
    ;
        Out = In.put([variables=[X=X | In.variables]])
    ).
In.get_variable_name(unnamed(X), Y) := In.put([variables=[X=Y | In.variables], counter=Counter1]) :-
    var(X),
    !,
    atom_concat(x, In.counter, Y),
    X=Y,
    Counter1 is In.counter + 1.
In.get_variable_name(unnamed(X), Y) := In :-
    member(X=Y, In.variables),
    !.

In.bound_variable(X, Quantor) := In.put([bound = [Quantor:X | In.bound]]).

In.write(Term) := In.put([body = NewBody]) :- format(atom(Str), "~w", [Term]), string_concat(In.body, Str, NewBody).

In.reset_body() := In.put([body = ""]).
