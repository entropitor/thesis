:- module(types, [
              combineTypes/2,
              addType/2
          ]).

combineTypes([], []) :-
    !.
combineTypes([type(Var, Type) | In], Out) :-
    selectchk(type(Var, Type2), In, RestIn),
    !,
    matchType(Type, Type2),
    combineTypes([type(Var, Type) | RestIn], Out).
combineTypes([type(Var, Type) | In], [type(Var, Type) | Out]) :-
    !,
    combineTypes(In, Out).

matchType(Type1, Type2) :-
    Type1 = Type2,
    !.
matchType(Type1, Type2) :-
    format("Error matchingType: ~p doesn't match with ~p~n", [Type1, Type2]),
    fail.

addType(_, _) :-
\+ nb_current(types, _),
nb_setval(types, []),
fail.
addType(Symbol, Type) :-
    b_getval(types, Types),
    b_setval(types, [type(Symbol, Type) | Types]).
