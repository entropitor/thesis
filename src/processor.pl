:- module(processor, [process/4]).

% process the list of atoms given the list of conditions
% and the given parse tree resulting in the theory.
process(Sentence, Facts, _Tree, Theory) :-
    reverse_quantors(Facts.quantors, ReversedQuantors),
    %writeln(ReversedQuantors),
    simplify_quantors(ReversedQuantors, QuantifiedVariables),
    %writeln(QuantifiedVariables),
    %simplify_conditions(Facts.conditions, SimplifiedConditions),
    combine_conditions(Facts.conditions, QuantifiedVariables, Theory),
    !,
    writeln(Sentence),
    %writeln(Tree),
    %display_tree(vertical, Tree),
    %writeln(Facts.conditions),
    %writeln(SimplifiedConditions),
    %writeln(Theory),
    once(maplist(write_theory, Theory)), nl,
    nl.
process(Sentence, Facts, _Tree, []) :-
    writeln(Sentence),
    writeln('Unknown: FAILED ERROR'),
    writeln(Facts),
    nl.

% ----------------------------------------------------------------------------------
reverse_quantors(Quantors, Reversed) :-
    reverse(Quantors, Reversed1),
    once(maplist(reverse_sub_quantors, Reversed1, Reversed)).

reverse_sub_quantors(and(A, B), and(ReversedA, ReversedB)) :-
    reverse_quantors(A, ReversedA),
    reverse_quantors(B, ReversedB).
reverse_sub_quantors(or(A, B), or(ReversedA, ReversedB)) :-
    reverse_quantors(A, ReversedA),
    reverse_quantors(B, ReversedB).
reverse_sub_quantors(X, X).

% ----------------------------------------------------------------------------------

simplify_quantors(Quantors, Simplified) :-
    simplify_quantors(Quantors, Simplified, 1, _).
simplify_quantors(Quantors, Simplified, In, Out) :-
    foldl(simplify_quantor, Quantors, Simplified1, In, Out),
    exclude(quantified_literal, Simplified1, Simplified).


simplify_quantor(quantor(quantified(Quantity), Variable), quantified(SimplifiedQuantity):Name, In, Out) :-
    !,
    simplify_quantity(Quantity, SimplifiedQuantity),
    simplify_variable(Variable, Name, In, Out).
simplify_quantor(quantor(Quantor, Variable), Quantor:Name, In, Out) :-
    !,
    simplify_variable(Variable, Name, In, Out).
simplify_quantor(and(QsA, QsB), and(SimplifiedQsA, SimplifiedQsB), In, Out) :-
    !,
    simplify_quantors(QsA, SimplifiedQsA, In, Out1),
    simplify_quantors(QsB, SimplifiedQsB, Out1, Out).
simplify_quantor(or(QsA, QsB), or(SimplifiedQsA, SimplifiedQsB), In, Out) :-
    !,
    simplify_quantors(QsA, SimplifiedQsA, In, Out1),
    simplify_quantors(QsB, SimplifiedQsB, Out1, Out).

simplify_quantity(exactly(literal(Times)), =(Times)).
simplify_quantity(at_least(literal(Times)), >=(Times)).

simplify_variable(literal(Name, _Variable), literal:Name, In, In) :-
    !.
simplify_variable(literal(Name), literal:Name, In, In) :-
    !.
simplify_variable(var(Variable, _Type), Name, In, Out) :-
    !,
    simplify_variable_name(Variable, Name, In, Out).

simplify_variable_name(X, X, In, Out) :-
    var(X),
    !,
    atom_concat(x, In, X),
    Out is In + 1.
simplify_variable_name(X, X, In, In) :-
    !.

quantified_literal(_Quantor:literal:_Name).

% ----------------------------------------------------------------------------------
simplify_conditions(Conds, SimplifiedConditions) :-
    once(maplist(simplify_condition, Conds, SimplifiedConditions)).

simplify_condition(equal(Property, Amount), equal(Property, SAmount)) :-
    simplify_amount(Amount, SAmount).
simplify_condition(and(A, B), and(SA, SB)) :-
    simplify_condition(A, SA),
    simplify_condition(B, SB).
simplify_condition(or(A, B), or(SA, SB)) :-
    simplify_condition(A, SA),
    simplify_condition(B, SB).
%% simplify_condition(if(A, B), if(SA, SB)) :-
%%     simplify_condition(A.conditions, SA),
%%     simplify_condition(B.conditions, SB).
simplify_condition(predicate(PredicateName, Entity, Attachment), predicate(PredicateName, SEntity, SAttachment)) :-
    simplify_entity(Entity, SEntity),
    simplify_attachment(Attachment, SAttachment).
simplify_condition(compare(Function, Property, Amount), compare(Function, Property, SAmount)) :-
    simplify_amount(Amount, SAmount).

simplify_amount(exactly(Value), Value).
simplify_amount(Amount, Amount).

simplify_entity(literal(Name, _Variable), Name).
simplify_entity(var(Variable, _), Variable).

simplify_attachment(Entity, SEntity) :-
    simplify_entity(Entity, SEntity).
simplify_attachment(literal(Name), Name).
simplify_attachment(Property, Property).

% ----------------------------------------------------------------------------------

combine_conditions(Conds, QuantifiedVariables, [and(Body, Theory)]) :-
    once(maplist(get_variables, Conds, VariableConds)),
    pairs_keys_values(Pairs, VariableConds, Conds),
    %maplist(writeln, Pairs),
    partition(at_most_variables([]), Pairs, BodyPairs, RestPairs),
    pairs_values(BodyPairs, Body),
    combine_conditions(RestPairs-[], QuantifiedVariables, Theory, []).

combine_conditions(P-P, [], [], _Variables).
combine_conditions(Pairs-PairsOut, [Quantor:Variable | QuantifiedVariables], quantify(Quantor, Variable, Expression), Variables) :-
    partition(at_most_variables([Variable]), Pairs, BodyPairs, RestPairs),
    partition(at_most_variables([Variable | Variables]), RestPairs, HeadPairs, RestPairs2),
    combine_conditions(RestPairs2-PairsOut, QuantifiedVariables, Head, [Variable | Variables]),
    combine(Quantor, BodyPairs, HeadPairs, Head, Expression).
combine_conditions(Pairs-PairsOut, [and(QsA, QsB) | QuantifiedVariables], and(ExprA, ExprB), Variables) :-
    handle_sub_conditions(Pairs-PairsOut, QsA, QsB, ExprA, ExprB, QuantifiedVariables, Variables).
combine_conditions(Pairs-PairsOut, [or(QsA, QsB) | QuantifiedVariables], or(ExprA, ExprB), Variables) :-
    handle_sub_conditions(Pairs-PairsOut, QsA, QsB, ExprA, ExprB, QuantifiedVariables, Variables).

handle_sub_conditions(Pairs-PairsOut, QsA, QsB, ExprA, ExprB, QuantifiedVariables, Variables) :-
    handle_sub_condition(Pairs-RestPairsA, QsA, ExprA, QuantifiedVariables, Variables),
    handle_sub_condition(Pairs-RestPairsB, QsB, ExprB, QuantifiedVariables, Variables),
    intersection(RestPairsA, RestPairsB, PairsOut).
handle_sub_condition(Pairs-PairsOut, Qs, Expr, QuantifiedVariables, Variables) :-
    append(Qs, QuantifiedVariables, Qs2),
    combine_conditions(Pairs-PairsOut, Qs2, Expr, Variables).

combine(Quantor, BodyPairs, HeadPairs, Head2, Theory) :-
    pairs_values(BodyPairs, Body),
    pairs_values(HeadPairs, Head1),
    append(Head1, Head2, Head),
    combine_(Quantor, Body, Head, Theory).

combine_(Quantor, [Head | Body], [], Theory) :-
    !,
    combine_(Quantor, Body, Head, Theory).
combine_(forall, Body, Head, impl(Body, Head)) :-
    !.
combine_(_, Body, Head, and(Body, Head)).

% ----------------------------------------------------------------------------------
get_variables(equal(_, Amount), Vars) :-
    get_variables_amount(Amount, Vars).
get_variables(and(A, B), Vars) :-
    get_variables(A, VarsA),
    get_variables(B, VarsB),
    append(VarsA, VarsB, Vars).
get_variables(or(A, B), Vars) :-
    get_variables(A, VarsA),
    get_variables(B, VarsB),
    append(VarsA, VarsB, Vars).
%% get_variables(if(A, B), Vars) :-
%%     get_variables(A.conditions, VarsA),
%%     get_variables(B.conditions, VarsB),
%%     append(VarsA, VarsB, Vars).
get_variables(predicate(_, Entity, Attachment), Vars) :-
    get_variables_entity(Entity, VarsA),
    get_variables_attachment(Attachment, VarsB),
    append(VarsA, VarsB, Vars).
get_variables(compare(_, AmountA, AmountB), Vars) :-
    get_variables_amount(AmountA, VarsA),
    get_variables_amount(AmountB, VarsB),
    append(VarsA, VarsB, Vars).

get_variables_entity(literal(_, Variable), [Variable]) :-
    nonvar(Variable),
    !.
get_variables_entity(literal(_, _Var), []).
get_variables_entity(var(Variable, _), [Variable]).

get_variables_attachment(Entity, Vars) :-
    get_variables_entity(Entity, Vars).
get_variables_attachment(literal(_), []).
get_variables_attachment(_Property, []).

get_variables_amount(exactly(Value), Vars) :-
    get_variables_amount_value(Value, Vars).
get_variables_amount(+(ValueA, ValueB), Vars) :-
    get_variables_amount_value(ValueA, VarsA),
    get_variables_amount_value(ValueB, VarsB),
    append(VarsA, VarsB, Vars).
get_variables_amount(Value, Vars) :-
    get_variables_amount_value(Value, Vars).

get_variables_amount_value(literal(_), []).
get_variables_amount_value(property(_, Entity), Vars) :-
    get_variables_entity(Entity, Vars).
% ----------------------------------------------------------------------------------

at_most_variables(Vars, UsedVars-_Condition) :-
    subset(UsedVars, Vars).

same_sets(A, B) :-
    list_to_ord_set(A, SetA),
    list_to_ord_set(B, SetB),
    ord_subtract(SetA, SetB, []),
    ord_subtract(SetB, SetA, []).


% ----------------------------------------------------------------------------------

write_theory(and([], A)) :-
    write_theory(A).
write_theory(and(A, [])) :-
    write_theory(A).
write_theory(and(A, and([], B))) :-
    write_theory(and(A, B)).
write_theory(impl([], A)) :-
    write_theory(A).

write_theory([]) :-
    write("true").
write_theory([A]) :-
    write_theory(A).
write_theory([A | B]) :-
    write_theory(A),
    write(" ∧ "),
    write_theory(B).
write_theory(and(A, B)) :-
    write_theory(A),
    write(" ∧ "),
    write_theory(B).
write_theory(or(A, B)) :-
    write('['),
    write('('),
    write_theory(A),
    write(')'),
    write(" ∨ "),
    write('('),
    write_theory(B),
    write(')'),
    write(']').
write_theory(impl(A, B)) :-
    write_theory(A),
    write(" => "),
    write_theory(B).
write_theory(compare(Functor, A, B)) :-
    write_amount(A),
    write(Functor),
    write_amount(B).

write_theory(quantify(Quantor, Variable, Expr)) :-
    write_quantor(Quantor),
    write(Variable),
    write(': '),
    write_theory(Expr).

write_theory(predicate(Name, X, Y)) :-
    write(Name),
    write('('),
    write_argument(X),
    write(','),
    write_argument(Y),
    write(')').

write_theory(X) :-
    write(X).

write_quantor(forall) :-
    write('∀').
write_quantor(exists) :-
    write('∃').
write_quantor(quantified(X)) :-
    write('∃'),
    write(X),
    write(' ').

write_argument(literal(X)) :-
    write(X).
write_argument(literal(X, _)) :-
    write(X).
write_argument(var(X, _)) :-
    write(X).

write_amount(exactly(X)) :-
    write_amount_value(X).
write_amount(+(X, Y)) :-
    write_amount_value(X),
    write('+'),
    write_amount(Y).
write_amount(X) :-
    write_amount_value(X).

write_amount_value(literal(X)) :-
    write(X).
write_amount_value(property(X, Entity)) :-
    write(X),
    write('('),
    write_argument(Entity),
    write(')').
