:- module(types, [
              combineTypes/2,
              combineTypesToMatrix/4,
              addType/2,
              addTypeAttribute/2,
              addMissingType/2,
              nameTypes/1,
              resolveMissingTypes/2,
              getRealBaseTypeCandidates/2
          ]).
:- use_module(questions, [askQuestion/3]).

combineTypes(In, Out) :-
    toTypesAndAttributes(In, Types-MissingTypes, Attributes),
    combineTypes2(Types, TypesOut),
    list_to_set(Attributes, AttributeSet),
    append(TypesOut, MissingTypes, Out1),
    append(Out1, AttributeSet, Out).

toTypesAndAttributes([], []-[], []).
toTypesAndAttributes([type(X, Y) | Rest], [type(X, Y) | Types]-MissingTypes, Attributes) :-
    !,
    toTypesAndAttributes(Rest, Types-MissingTypes, Attributes).
toTypesAndAttributes([missingType(X, Y) | Rest], Types-[missingType(X, Y) | MissingTypes], Attributes) :-
    !,
    toTypesAndAttributes(Rest, Types-MissingTypes, Attributes).
toTypesAndAttributes([attr(X, Y) | Rest], Types, [attr(X, Y) | Attributes]) :-
    !,
    toTypesAndAttributes(Rest, Types, Attributes).

resolveMissingTypes(Types, Types) :-
    \+ memberchk(missingType(_, _), Types),
    !.
resolveMissingTypes(Types, TypesOut2) :-
    selectchk(missingType(X, Y), Types, TypesOut),
    resolveMissingType(X, Y, TypesOut, TypesOut1),
    resolveMissingTypes(TypesOut1, TypesOut2).
resolveMissingType(X, Y, TypesIn, TypesIn) :-
    member(type(_-X, Type), TypesIn),
    Type == Y,
    !.
resolveMissingType(X, Y, TypesIn, TypesOut) :-
    member(type(_-Name, Type), TypesIn),
    nonvar(Type),
    Type = pred(S, O),
    Y == pred(O, S),
    !,
    TypesOut = [type(new-X, Y) | TypesIn],
    format(atom(X), '~p_rev', [Name]).
resolveMissingType(X, Y, TypesIn, TypesOut) :-
    nonvar(Y),
    Y = pred(S, O),
    !,
    format(atom(X), 'unknown_relation_~p_~p', [S, O]),
    TypesOut = [type(new-X, Y) | TypesIn].
resolveMissingType(_, _, _, _) :-
    writeln("Couldn't resolve missingType"),
    fail.

combineTypes2([], []) :-
    !.
combineTypes2([type(Var, Type) | In], Out) :-
    Var \= number-_,
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
    %% (
    %%     var(Type)
    %%  ->
    %%      Type1 = baseType(Type)
    %%  ;
    %%      Type1 = Type
    %% ),
    b_setval(types, [type(Symbol, Type) | Types]).
addTypeAttribute(Type, Attribute) :-
    b_getval(types, Types),
    b_setval(types, [attr(Type, Attribute) | Types]).
addMissingType(Symbol, Type) :-
    b_getval(types, Types),
    b_setval(types, [missingType(Symbol, Type) | Types]).

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

combineTypesToMatrix(CombinedTypesDirty, NbBaseTypes, _NbConceptsPerType, TypesForMatrix) :-
    filterGroupSizeTypes(CombinedTypesDirty, CombinedTypes),
    getRealBaseTypeCandidates(CombinedTypes, RealCandidates),
    simplifyCandidates(CombinedTypes, NbBaseTypes, RealCandidates, TypesForMatrix).

filterGroupSizeTypes(CombinedTypesDirty, CombinedTypes) :-
    select(type(number-_, Type), CombinedTypesDirty, Rest),
    \+ (member(attr(Type1, countable), Rest), Type1 == Type),
    !,
    filterGroupSizeTypes(Rest, CombinedTypes).
filterGroupSizeTypes(CombinedTypes, CombinedTypes).

getRealBaseTypeCandidates(CombinedTypes, RealCandidates) :-
    getDerivedTypes(CombinedTypes, DerivedTypes),
    getBaseTypeCandidates(CombinedTypes, BaseTypeCandidates),
    list_to_ord_set(DerivedTypes, DerivedTypesSet),
    list_to_ord_set(BaseTypeCandidates, BaseTypeCandidatesSet),
    getIntermediateTypes(BaseTypeCandidatesSet, CombinedTypes, IntermediateTypes),
    list_to_ord_set(IntermediateTypes, IntermediateTypesSet),
    ord_subtract(BaseTypeCandidatesSet, DerivedTypesSet, RealCandidates1),
    ord_subtract(RealCandidates1, IntermediateTypesSet, RealCandidates).

getIntermediateTypes([], _, []).
getIntermediateTypes([BaseType | BaseTypes], CombinedTypes, [BaseType | IntermediateTypes]) :-
    \+ (member(attr(Type, _), CombinedTypes), Type == BaseType),
    !,
    getIntermediateTypes(BaseTypes, CombinedTypes, IntermediateTypes).
getIntermediateTypes([_ | BaseTypes], CombinedTypes, IntermediateTypes) :-
  getIntermediateTypes(BaseTypes, CombinedTypes, IntermediateTypes).


getDerivedTypes([], []).
getDerivedTypes([attr(DerivedType, derivedCountable(_)) | Rest], [DerivedType | Types]) :-
    !,
    getDerivedTypes(Rest, Types).
getDerivedTypes([_ | Rest], Types) :-
    getDerivedTypes(Rest, Types).

getBaseTypeCandidates([], []).
getBaseTypeCandidates([type(_, Type) | Types], Vars) :-
    !,
    term_variables(Type, Vars1),
    append(Vars1, Vars2, Vars),
    getBaseTypeCandidates(Types, Vars2).
getBaseTypeCandidates([_ | Types], Vars) :-
    getBaseTypeCandidates(Types, Vars).

simplifyCandidates(CombinedTypes, NbBaseTypes, RealCandidates, TypesForMatrix) :-
    length(RealCandidates, NbBaseTypes),
    !,
    include(\=(neq(_, _)), CombinedTypes, Temp),
    include(\=(eq(_, _)), Temp, Temp1),
    include(\=(notObjectOf(_, _)), Temp1, Temp2),
    include(\=(neq_rev(_, _)), Temp2, Temp3),
    include(\=(eq_rev(_, _)), Temp3, RealCombinedTypes),
    combineTypes(RealCombinedTypes, TypesForMatrix).
simplifyCandidates(CombinedTypes, NbBaseTypes, RealCandidates, TypesForMatrix) :-
    length(RealCandidates, N),
    N > NbBaseTypes,
    askSimplificationQuestion(CombinedTypes, NewCombinedTypes),
    !,
    list_to_set(RealCandidates, NewCandidates),
    simplifyCandidates(NewCombinedTypes, NbBaseTypes, NewCandidates, TypesForMatrix).
simplifyCandidates(CombinedTypes, NbBaseTypes, RealCandidates, TypesForMatrix) :-
    select(X, RealCandidates, OtherCandidates),
    member(type(number-_, X), CombinedTypes),
    \+ (member(attr(Y, countable), CombinedTypes), X == Y),
    !,
    simplifyCandidates(CombinedTypes, NbBaseTypes, OtherCandidates, TypesForMatrix).
simplifyCandidates(CombinedTypes, NbBaseTypes, RealCandidates, _TypesForMatrix) :-
    length(RealCandidates, N),
    N > NbBaseTypes,
    format("All possible questions are asked but types are not yet unified enough~n"),
    writeln(RealCandidates),
    writeln(CombinedTypes),
    fail.

askSimplificationQuestion(CombinedTypes, NewCombinedTypes) :-
    member(type(_-Symbol1, Type1), CombinedTypes),
    nonvar(Type1),
    Type1 = pred(S1, O1),
    member(type(_-Symbol2, Type2), CombinedTypes),
    Type1 \== Type2,
    nonvar(Type2),
    Type2 = pred(S2, O2),
    \+ \+ (S1 = S2, O1 = O2),
    \+ compared(Symbol1, Symbol2, CombinedTypes),
    format(string(Question), "Are '~p' and '~p' the same relation? [yes/no]", [Symbol1, Symbol2]),
    askQuestion(eq(Symbol1, Symbol2), Question, Answer),
    (
        Answer = yes
    ->
        S1 = S2,
        O1 = O2,
        NewCombinedTypes = [eq(Symbol1, Symbol2) | CombinedTypes]
    ;
        NewCombinedTypes = [neq(Symbol1, Symbol2) | CombinedTypes]
    ).
askSimplificationQuestion(CombinedTypes, NewCombinedTypes) :-
    member(type(_-Symbol1, Type1), CombinedTypes),
    nonvar(Type1),
    Type1 = pred(S1, O1),
    member(type(_-Symbol2, Type2), CombinedTypes),
    Type1 \== Type2,
    nonvar(Type2),
    Type2 = pred(S2, O2),
    \+ \+ (S1 = O2, O1 = S2),
    \+ compared_rev(Symbol1, Symbol2, CombinedTypes),
    format(string(Question), "Are '~p' and '~p' each other reverse relation? [yes/no]", [Symbol1, Symbol2]),
    askQuestion(eq_rev(Symbol1, Symbol2), Question, Answer),
    (
        Answer = yes
    ->
        S1 = O2,
        O1 = S2,
        NewCombinedTypes = [eq_rev(Symbol1, Symbol2) | CombinedTypes]
    ;
        NewCombinedTypes = [neq_rev(Symbol1, Symbol2) | CombinedTypes]
    ).
askSimplificationQuestion(CombinedTypes, NewCombinedTypes) :-
    member(missingType(_, pred(_, Type1)), CombinedTypes),
    member(type(number-Number, TypeNumber), CombinedTypes),
    Type1 == TypeNumber,
    member(attr(Type2, countable), CombinedTypes),
    \+ (Type1 == Type2),
    member(type(_-Symbol, PredType), CombinedTypes),
    nonvar(PredType),
    PredType = pred(_S, O),
    O == Type2,
    \+ (member(notObjectOf(Number, Symbol), CombinedTypes)),
    format(string(Question), "Is '~p' a possible object of the '~p' relation? [yes/no]", [Number, Symbol]),
    askQuestion(objectOf(Number, Symbol), Question, Answer),
    (
        Answer = yes
     ->
         Type1 = Type2,
         NewCombinedTypes = CombinedTypes
     ;
        NewCombinedTypes = [notObjectOf(Number, Symbol) | CombinedTypes]
    ).
askSimplificationQuestion(CombinedTypes, NewCombinedTypes) :-
    member(missingType(_, pred(_, Type1)), CombinedTypes),
    member(type(pn-Symbol1, TypePN), CombinedTypes),
    TypePN == Type1,
    member(attr(Type2, qualified), CombinedTypes),
    \+ (Type1 == Type2),
    member(type(_-Symbol2, PredType), CombinedTypes),
    nonvar(PredType),
    PredType = pred(_S, O),
    O == Type2,
    \+ (member(notObjectOf(Symbol1, Symbol2), CombinedTypes)),
    format(string(Question), "Is '~p' a possible object of the '~p' relation? [yes/no]", [Symbol1, Symbol2]),
    askQuestion(objectOf(Symbol1, Symbol2), Question, Answer),
    (
        Answer = yes
    ->
        Type1 = Type2,
        NewCombinedTypes = CombinedTypes
    ;
        NewCombinedTypes = [notObjectOf(Symbol1, Symbol2) | CombinedTypes]
    ).


compared(S1, S2, Types) :-
    eq(S1, S2, Types).
compared(S1, S2, Types) :-
    neq(S1, S2, Types).
eq(S1, S2, Types) :-
    member(eq(S1, S2), Types).
eq(S1, S2, Types) :-
    member(eq(S2, S1), Types).
neq(S1, S2, Types) :-
    member(neq(S1, S2), Types).
neq(S1, S2, Types) :-
    member(neq(S2, S1), Types).

compared_rev(S1, S2, Types) :-
    eq_rev(S1, S2, Types).
compared_rev(S1, S2, Types) :-
    neq_rev(S1, S2, Types).
eq_rev(S1, S2, Types) :-
    member(eq_rev(S1, S2), Types).
eq_rev(S1, S2, Types) :-
    member(eq_rev(S2, S1), Types).
neq_rev(S1, S2, Types) :-
    member(neq_rev(S1, S2), Types).
neq_rev(S1, S2, Types) :-
    member(neq_rev(S2, S1), Types).
