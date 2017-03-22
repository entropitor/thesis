:- module(types, [
              combineTypes/2,
              addType/2,
              nameTypes/1
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
    format("~nWarning matchingType for ~p: ~p doesn't match with ~p~n", [Var, Type1, Type2]),
    fail.

addType(_, _) :-
\+ nb_current(types, _),
nb_setval(types, []),
fail.
addType(Symbol, Type) :-
    b_getval(types, Types),
    b_setval(types, [type(Symbol, Type) | Types]).


checkUnkownVars([], [], true).
checkUnkownVars([type(WordSort-Var, Type) | Rest], [type(WordSort-Var, Type) | Out], Success) :-
    nonvar(Var),
    checkUnkownVars(Rest, Out, Success).
checkUnkownVars([type(WordSort-Var, Type) | Rest], [type(WordSort-Var, Type) | Out], true) :-
    var(Var),
    checkMatchingVar(type(WordSort-Var, Type), Rest),
    checkUnkownVars(Rest, Out, _).

checkMatchingVar(type(_, Type), []) :-
    format("~nError finding predicate for type: ~p", [Type]),
    fail.
checkMatchingVar(type(Var, Type), [type(Var, Type2) | _]) :-
    nonvar(Type2),
    Type = Type2.
checkMatchingVar(Type, [_ | Rest]) :-
    checkMatchingVar(Type, Rest).

nameTypes(Types) :-
    maplist(nameType, Types).

nameType(type(noun-Symbol, qualified(Type))) :-
    var(Type),
    !,
    Symbol = Type.
nameType(type(noun-Symbol, countable(Type))) :-
    var(Type),
    !,
    Symbol = Type.
nameType(_).

