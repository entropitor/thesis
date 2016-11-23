:- module(print, [show_rules/3]).
:- use_module(dcg_macro).
:- use_module(tree).

show_rules(Atoms, Facts, _Tree) :-
    atomics_to_string(Atoms, ' ', Sentence),
    reverse(Facts.quantors, ReversedQuantors),
    simplify_quantors(ReversedQuantors, QuantifiedVariables),
    maplist(quantified_variable_string, QuantifiedVariables, QuantorStrs),
    join_strings(QuantorStrs, QuantorStr),
    simplify_conditions(Facts.conditions, QuantifiedVariables, Conditionss),
    findall(Str, (member(Condition, Conditionss), show_body(Condition, QuantorStr, Str)), Strs),
    !,
    writeln(Sentence),
    %writeln(Tree),
    %display_tree(vertical, Tree),
    %writeln(Facts.conditions),
    maplist(writeln, Strs),
    nl,
    %% header,
    %% wwriteln(Sentence),
    %% section,
    %% maplist(wwriteln, Strs),
    %% header,
    %wwriteln(Facts.conditions),
    %section,
    %wwriteln(Out.body),
    %section,
    %wwriteln(Facts.quantors),
    %wwriteln(QuantifiedVariables),
    %section,
    %maplist(show_quantified_variable(Out), Out.bound, Strs),
    %atomics_to_string(Strs, ' ', Str),
    %wwrite(Str),
    %write(' '),
    %writeln(Out.body),
    %wwriteln(Str2),
    %nl, nl,
    true.
show_rules(_Atoms, Facts, _Tree) :-
    header,
    wwriteln('Unknown: FAILED ERROR'),
    wwriteln(Facts),
    header,
    nl.

show_body(Conditions, QuantorStr, Str) :-
    show(Conditions, print{}.default()-Out),
    join_strings([QuantorStr, Out.body, "."], Str).


header :-
    writeln('####################################################################').

wwrite(X) :-
    write('###   '),
    write(X).
wwriteln(X) :-
    wwrite(X),
    nl.
section :-
    wwriteln('--------------------------------------------------------   ###').

% ------------------------
simplify_quantors(Quantors, Simplified) :-
    foldl(simplify_quantor, Quantors, Simplified1, 1, _),
    exclude(literal, Simplified1, Simplified).

simplify_quantor(quantor(quantified(Quantity), Variable), quantified(SimplifiedQuantity):Name, In, Out) :-
    !,
    simplify_quantity(Quantity, SimplifiedQuantity),
    simplify_variable(Variable, Name, In, Out).
simplify_quantor(quantor(Quantor, Variable), Quantor:Name, In, Out) :-
    !,
    simplify_variable(Variable, Name, In, Out).

simplify_quantity(exactly(literal(Times)), =(Times)).
simplify_quantity(at_least(literal(Times)), >=(Times)).

simplify_variable(literal(Name, _Variable), literal:Name, In, In) :-
    !.
simplify_variable(literal(Name), literal:Name, In, In) :-
    !.
simplify_variable(var(Variable, _Type), Name, In, Out) :-
    !,
    simplify_variable_name(Variable, Name, In, Out).

simplify_variable_name(named(X), X, In, In) :-
    !.
simplify_variable_name(unnamed(X), X, In, Out) :-
    var(X),
    !,
    atom_concat(x, In, X),
    Out is In + 1.
simplify_variable_name(unnamed(X), X, In, In) :-
    !.

literal(_Quantor:literal:_Name).

quantified_variable_string(Quantor:Variable, String) :-
    quantor_string(Quantor, S1),
    join_strings([S1, " ", Variable, ": "], String).
quantor_string(forall, "∀").
quantor_string(exists, "∃").
quantor_string(quantified(Q), Str) :-
    Q =.. [Functor, Amount],
    join_strings(["∃", Functor, Amount], Str).

join_strings(Strs, Str) :-
    foldl(string_concat_, Strs, "", Str).
string_concat_(A, B, C) :-
    string_concat(B, A, C).

simplify_conditions(Conds, Quantors, NewConds) :-
    findall(NewCond, (
                simplify_condition(Conds, Quantors, NewCond)
            ), NewConds).

simplify_condition([Head | Body], Quantors, impl(ReversedBody, Head)) :-
    last(Quantors, forall:_),
    length(Body, L),
    L >= 1,
    !,
    reverse(Body, ReversedBody).
simplify_condition([and(X1, X2) | Rest], Quantors, impl(Body, [Head1, Head2])) :-
    last(Quantors, forall:_),
    !,
    X1.conditions = [Head1 | Body1],
    X2.conditions = [Head2 | Body2],
    append(Body1, Body2, Bodies),
    append(Bodies, Rest, Body),
    length(Body, L),
    L >= 1.
%% simplify_condition([and(Cond1, Cond2) | Rest], Quantors, NewCond) :-
%%     !,
%%     member(Cond, [Cond1, Cond2]),
%%     append(Cond.conditions, Rest, Conds),
%%     simplify_condition(Conds, Quantors, NewCond).
simplify_condition([H | T], _Quantors, Reversed) :-
    !,
    reverse([H | T], Reversed).
simplify_condition(In, _Quantors, In).

% ------------------------

show([], In-In) :-
    !.
show([H], In-Out) :-
    !,
    show(H, In-Out).
show([H | T], In-Out) :-
    !,
    show(H, In-Out1),
    Out2 = Out1.write(' ∧ '),
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
show(predicate(X, rev(Y), Z), In-Out) :-
    !,
    show(predicate(X, Z, Y), In-Out).
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
show(and(X, Y), In-Out) :-
    !,
    show(X.conditions, In-Out1),
    Out2 = Out1.write(' ∧ '),
    show(Y.conditions, Out2-Out).
show(if(Cond, Expr), In-Out) :-
    !,
    show(Expr.conditions, In-Out1),
    Out2 = Out1.write(' <- '),
    show(Cond.conditions, Out2-Out).
show(impl(Body, Head), In-Out) :-
    !,
    show(Body, In-Out1),
    Out2 = Out1.write(' => '),
    show(Head, Out2-Out).
show(Y, In-Out) :-
    !,
    Out = In.write('Unknown: { ').write(Y).write(' }').


show_argument(literal(Name, _Variable), In-Out) :-
    !,
    Out = In.write(Name).
show_argument(literal(Name), In-Out) :-
    !,
    Out = In.write(Name).
show_argument(var(X, _Type), In-Out) :-
    !,
    variable_name(X, Y),
    Out = In.write(Y).
show_argument(Y, In-Out) :-
    !,
    Out = In.write(Y).

_.default() := print{body: ""}.

variable_name(named(X), X).
variable_name(unnamed(X), X).

In.write(Term) := In.put([body = NewBody]) :-
    format(atom(Str), "~w", [Term]), string_concat(In.body, Str, NewBody).
