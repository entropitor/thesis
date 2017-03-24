:- module(typeExtraction, [
              getPredicates/2,
              getBaseTypes/4,
              getBaseTypeAtoms/2
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
    include(=(attr(BaseType, qualified)), Types, X),
    X \= [],
    !,
    maplist(getPNsForBaseType(BaseType), Types, Symbols1),
    include(\=(null), Symbols1, Symbols2),
    Symbols2 \= [],
    completeConstructedSet(Symbols2, NbConceptsPerType, BaseType, Symbols),
    (
        include(=(attr(BaseType, countable)), Types, []),
        !
    ;
        error(err('Constructed type used as countable', BaseType))
    ),
    !.
toBaseType(Types, _NbConceptsPerType, BaseType, baseType(BaseType, int)) :-
    include(=(attr(BaseType, countable)), Types, X),
    X \= [],
    !.
toBaseType(_, _, Type, baseType(Type, unknown)).

getPNsForBaseType(BaseType, type(pn-Symbol, BaseType), Symbol) :-
    !.
getPNsForBaseType(_, _, null).


completeConstructedSet(Symbols, L, _, Symbols) :-
    length(Symbols, L),
    !.
completeConstructedSet(Symbols, _, BaseType, [OtherSymbol | Symbols]) :-
    atom_concat('the_other_', BaseType, OtherSymbol).
