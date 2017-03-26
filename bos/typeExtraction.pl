:- module(typeExtraction, [
              getPredicates/2,
              getBaseTypes/4,
              getBaseTypeAtoms/2,
              getDerivedTypes/3
          ]).
:- use_module(questions, [
                  askQuestion/3
              ]).


getPredicates([], []).
getPredicates([type(_Wordsort-Name, pred(T1, T2)) | Types], [predicate(Name, T1, T2) | Preds]) :-
    !,
    getPredicates(Types, Preds).
getPredicates([type(_Wordsort-Name, fun(T1, T2)) | Types], [predicate(Name, T1, T2) | Preds]) :-
    !,
    getPredicates(Types, Preds).
getPredicates([_ | Types], Preds) :-
    !,
    getPredicates(Types, Preds).

getBaseTypes(Types, BaseTypes, NbBaseTypes, NbConceptsPerType) :-
    getBaseTypeAtoms(Types, BaseTypesAtoms),
    length(BaseTypesAtoms, NbBaseTypes),
    maplist(toBaseType(Types, NbConceptsPerType), BaseTypesAtoms, BaseTypes).

getBaseTypeAtoms(Types, BaseTypesAtoms) :-
    maplist(getBaseTypesForType, Types, Temp1),
    flatten(Temp1, BaseTypesAtomsList),
    list_to_set(BaseTypesAtomsList, BaseTypesAtoms).

getBaseTypesForType(type(_, pred(T1, T2)), [T1, T2]) :-
    !.
getBaseTypesForType(type(_, fun(T1, T2)), [T1, T2]) :-
    !.
getBaseTypesForType(type(_, T), [T]) :-
    !.
getBaseTypesForType(attr(_, _), []) :-
    !.


toBaseType(Types, NbConceptsPerType, BaseType, baseType(BaseType, constructed:Symbols)) :-
    member(attr(BaseType, qualified), Types),
    !,
    maplist(getPNsForBaseType(BaseType), Types, Symbols1),
    include(\=(null), Symbols1, Symbols2),
    Symbols2 \= [],
    completeConstructedSet(Symbols2, NbConceptsPerType, BaseType, Symbols),
    (
        \+ member(attr(BaseType, countable), Types),
        !
    ;
        error(err('Constructed type used as countable', BaseType))
    ),
    !.
toBaseType(Types, _NbConceptsPerType, BaseType, baseType(BaseType, int:Range)) :-
    member(attr(BaseType, countable), Types),
    !,
    getRepresentativeUseForBaseType(Types, BaseType, RepresentativeUse),
    format(string(Str), "What are the possible values for ~p ~w?", [BaseType, RepresentativeUse]),
    askQuestion(q(BaseType, intRange), Str,Range).
toBaseType(_, _, Type, baseType(Type, unknown)).

getPNsForBaseType(BaseType, type(pn-Symbol, BaseType), Symbol) :-
    !.
getPNsForBaseType(_, _, null).


completeConstructedSet(Symbols, L, _, Symbols) :-
    length(Symbols, L),
    !.
completeConstructedSet(Symbols, _, BaseType, [OtherSymbol | Symbols]) :-
    atom_concat('the_other_', BaseType, OtherSymbol).

getRepresentativeUseForBaseType(Types, BaseType, RepresentativeUse) :-
    member(type(_WordSort-Symbol, pred(_, BaseType)), Types),
    !,
    format(string(RepresentativeUse), "(e.g. the object of ~w)", [Symbol]).
getRepresentativeUseForBaseType(Types, BaseType, RepresentativeUse) :-
    member(type(_WordSort-Symbol, pred(BaseType, _)), Types),
    !,
    format(string(RepresentativeUse), "(e.g. the subject of ~w)", [Symbol]).
getRepresentativeUseForBaseType(Types, _, RepresentativeUse) :-
    format(string(RepresentativeUse), "(Whoops, couldn't find example of this type in ~p)", [Types]).


getDerivedTypes(Types, BaseTypes, DerivedTypes) :-
    findall(BaseType-DerivedType, member(attr(DerivedType, derivedCountable(BaseType)), Types), DerivedTypePairs),
    maplist(pairToDerivedType(BaseTypes), DerivedTypePairs, DerivedTypes).
pairToDerivedType(BaseTypes, BaseType-DerivedType, derivedType(DerivedType, BaseType, int:Range)) :-
    member(baseType(BaseType, int:BaseTypeRange), BaseTypes),
    !,
    baseTypeRangeToDerivedTypeRange(BaseTypeRange, Range1),
    list_to_set(Range1, Range).
baseTypeRangeToDerivedTypeRange([], []).
baseTypeRangeToDerivedTypeRange([X | Rest], Range) :-
    findall(Z, (member(Y, Rest), (Z is X - Y ; Z is Y - X)), Zs),
    baseTypeRangeToDerivedTypeRange(Rest, Range1),
    append(Zs, Range1, Range).

