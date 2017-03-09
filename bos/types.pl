:- module(types, [
              combineTypes/2,
              addType/2
          ]).

combineTypes(In, Out) :-
    checkUnkownVars(In, Out1, Success),
    !,
    call(Success),
    combineTypes2(Out1, Out).

combineTypes2([], []) :-
    !.
combineTypes2([type(Var, Type) | In], Out) :-
    selectchk(type(Var, Type2), In, RestIn),
    !,
    matchType(Var, Type, Type2),
    combineTypes([type(Var, Type) | RestIn], Out).
combineTypes2([type(Var, Type) | In], [type(Var, Type) | Out]) :-
    !,
    combineTypes(In, Out).

matchType(_, Type1, Type2) :-
    Type1 = Type2,
    !.
matchType(Var, Type1, Type2) :-
    format("~nError matchingType for ~p: ~p doesn't match with ~p~n", [Var, Type1, Type2]),
    fail.

addType(_, _) :-
\+ nb_current(types, _),
nb_setval(types, []),
fail.
addType(Symbol, Type) :-
    b_getval(types, Types),
    b_setval(types, [type(Symbol, Type) | Types]).


checkUnkownVars([], [], true).
checkUnkownVars([type(Var, Type) | Rest], [type(Var, Type) | Out], Success) :-
    nonvar(Var),
    checkUnkownVars(Rest, Out, Success).
checkUnkownVars([type(Var, Type) | Rest], [type(Var, Type) | Out], true) :-
    var(Var),
    checkMatchingVar(type(Var, Type), Rest),
    checkUnkownVars(Rest, Out, _).

checkMatchingVar(type(_, Type), []) :-
    format("~nError finding predicate for type: ~p", [Type]),
    fail.
checkMatchingVar(type(Var, Type), [type(Var, Type) | _]).
checkMatchingVar(Type, [_ | Rest]) :-
    checkMatchingVar(Type, Rest).

