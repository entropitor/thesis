:- module(types, [
              combineTypes/2,
              addType/2,
              addTypeAttribute/2,
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
combineTypes2([T | In], [T | Out]) :-
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
addTypeAttribute(Type, Attribute) :-
    b_getval(types, Types),
    b_setval(types, [attr(Type, Attribute) | Types]).


checkUnkownVars([], [], true).
checkUnkownVars([type(WordSort-Var, Type) | Rest], [type(WordSort-Var, Type) | Out], Success) :-
    nonvar(Var),
    !,
    checkUnkownVars(Rest, Out, Success).
checkUnkownVars([type(WordSort-Var, Type) | Rest], [type(WordSort-Var, Type) | Out], true) :-
    var(Var),
    !,
    checkMatchingVar(type(WordSort-Var, Type), Rest),
    checkUnkownVars(Rest, Out, _).
checkUnkownVars([T | Rest], [T | Out], Success) :-
    checkUnkownVars(Rest, Out, Success).

checkMatchingVar(type(_, Type), []) :-
    format("~nError finding predicate for type: ~p", [Type]),
    fail.
checkMatchingVar(type(Var, Type), [type(Var, Type2) | _]) :-
    nonvar(Type2),
    Type = Type2.
checkMatchingVar(Type, [_ | Rest]) :-
    checkMatchingVar(Type, Rest).

nameTypes(Types) :-
    maplist(nameType, Types),
    term_variables(Types, UnnamedTypes),
    nameUnnamedTypes(UnnamedTypes, 1).

nameType(type(noun-Symbol, Type)) :-
    var(Type),
    !,
    Symbol = Type.
nameType(_).

nameUnnamedTypes([], _).
nameUnnamedTypes([Type | Types], Number) :-
    Number1 is Number + 1,
    name(Number, NumberCodes),
    atom_codes(Type, [116, 121, 112, 101 | NumberCodes]),
    nameUnnamedTypes(Types, Number1).

