:- module(types, [
              combineTypes/2,
              addType/2,
              addTypeAttribute/2,
              nameTypes/1
          ]).

combineTypes(In, Out) :-
    toTypesAndAttributes(In, Types, Attributes),
    list_to_set(Attributes, AttributeSet),
    combineTypes2(Types, TypesOut),
    append(TypesOut, AttributeSet, Out).

toTypesAndAttributes([], [], []).
toTypesAndAttributes([type(X, Y) | Rest], [type(X, Y) | Types], Attributes) :-
    !,
    toTypesAndAttributes(Rest, Types, Attributes).
toTypesAndAttributes([attr(X, Y) | Rest], Types, [attr(X, Y) | Attributes]) :-
    !,
    toTypesAndAttributes(Rest, Types, Attributes).

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


%% checkUnkownVars([], [], true).
%% checkUnkownVars([type(WordSort-Var, Type) | Rest], [type(WordSort-Var, Type) | Out], Success) :-
%%     nonvar(Var),
%%     !,
%%     checkUnkownVars(Rest, Out, Success).
%% checkUnkownVars([type(WordSort-Var, Type) | Rest], [type(WordSort-Var, Type) | Out], true) :-
%%     var(Var),
%%     !,
%%     reverse(Rest, RestReversed),
%%     writeln(RestReversed),
%%     checkMatchingVar(type(WordSort-Var, Type), RestReversed),
%%     !,
%%     checkUnkownVars(Rest, Out, _).
%% checkUnkownVars([T | Rest], [T | Out], Success) :-
%%     checkUnkownVars(Rest, Out, Success).

%% checkMatchingVar(type(_, Type), []) :-
%%     format("~nError finding predicate for type: ~p", [Type]),
%%     fail.
%% checkMatchingVar(type(WS-Var, Type), [type(WS-Var2, Type2) | _]) :-
%%     nonvar(Var2),
%%     Var = Var2,
%%     nonvar(Type2),
%%     Type = Type2.
%% checkMatchingVar(Type, [_ | Rest]) :-
%%     checkMatchingVar(Type, Rest).

nameTypes(Types) :-
    maplist(nameNounType, Types),
    nameDerivedTypes(Types, 1, End),
    term_variables(Types, UnnamedTypes),
    nameUnnamedTypes(UnnamedTypes, End).

nameNounType(type(noun-Symbol, Type)) :-
    var(Type),
    !,
    Symbol = Type.
nameNounType(_).

nameUnnamedTypes([], _).
nameUnnamedTypes([Type | Types], Number) :-
    Number1 is Number + 1,
    name(Number, NumberCodes),
    atom_codes(Type, [116, 121, 112, 101 | NumberCodes]),
    nameUnnamedTypes(Types, Number1).

nameDerivedTypes(Types, Start, End) :-
    findall(BaseType-DerivedType, member(attr(DerivedType, derivedCountable(BaseType)), Types), DerivedTypePairs),
    nameDerivedTypesFromPairs(DerivedTypePairs, Start, End).
nameDerivedTypesFromPairs([], Start, Start).
nameDerivedTypesFromPairs([BaseType-DerivedType | Rest], Start, End) :-
    atomic(BaseType),
    !,
    atom_concat(BaseType, 'Difference', DerivedType),
    nameDerivedTypesFromPairs(Rest, Start, End).
nameDerivedTypesFromPairs([BaseType-DerivedType | Rest], Start, End) :-
    Start1 is Start + 1,
    nameUnnamedTypes([BaseType], Start),
    atom_concat(BaseType, 'Difference', DerivedType),
    nameDerivedTypesFromPairs(Rest, Start1, End).


