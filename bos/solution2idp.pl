:- module(solution2idp, [
              solution2idp/2
          ]).

:- use_module(drs2fol, [drs2fol/2]).
:- use_module(printFol, [printFol/2]).
:- use_module(types, [
                  nameTypes/1
              ]).

solution2idp(solution(Sentences, DRSs, Types), Problem) :-
    maplist(drs2fol, DRSs, FOLs),
    pairs_keys_values(SentencePairs, Sentences, FOLs),
    nameTypes(Types),
    nameVariables(FOLs),
    getPredicates(Types, Predicates),
    getBaseTypes(Types, BaseTypes),

    nl,
    maplist(writeln, FOLs),
    nl,
    writeln(Types),
    writeln(Predicates),
    writeln(BaseTypes),

    \+ \+ printFile(Problem, SentencePairs, voc(BaseTypes, Predicates)).

printFile(Problem, SentencePairs, Vocabularium) :-
    problemToFileName(Problem, FileName),
    tell(FileName),
    write('// Problem '),
    writeln(Problem),
    nl,
    printVocabulary(Vocabularium),
    nl,
    printStructure(),
    nl,
    printTheory(SentencePairs, Vocabularium),
    nl,
    printMain(),
    nl,
    told.
problemToFileName(Problem, FileName) :-
    atom_concat('output/', Problem, Temp1),
    atom_concat(Temp1, '.idp', FileName).

printVocabulary(voc(BaseTypes, Predicates)) :-
    writeln('vocabulary V {'),
    maplist(printType, BaseTypes),
    nl,
    maplist(printPredicate, Predicates),
    writeln('}').
printType(baseType(Type, constructed:List)) :-
    !,
    atomic_list_concat(List, ', ', ListString),
    format('    type ~p constructed from {~w}~n', [Type, ListString]).
printType(baseType(Type, int)) :-
    !,
    format('    type ~p isa int~n', [Type]).
printType(baseType(Type, X)) :-
    !,
    format('    type ~p //~p~n', [Type, X]).
printPredicate(predicate(Name, Type1, Type2)) :-
    format('    ~p(~p, ~p)~n', [Name, Type1, Type2]).

printStructure() :-
    writeln('structure S : V {'),
    writeln('}').

printTheory(SentencePairs, voc(_, Predicates)) :-
    writeln('theory T : V {'),
    maplist(printSentence, SentencePairs),
    nl,
    format('    // Logigram bijection axioms:~n'),
    maplist(printLogigramAxiomsForPredicate, Predicates),
    format('    // Logigram synonym axioms:~n'),
    printSynonymAxioms(Predicates),
    writeln('}').
printSentence(Sentence-FOL) :-
    format('    // ~w~n    ~@~n', [Sentence, printFol(idp, FOL)]).
printLogigramAxiomsForPredicate(predicate(Name, Type1, Type2)) :-
    format('    ! x [~p]: ?=1 y [~p]: ~p(x, y).~n', [Type1, Type2, Name]),
    format('    ! x [~p]: ?=1 y [~p]: ~p(y, x).~n~n', [Type2, Type1, Name]).
printSynonymAxioms([]).
printSynonymAxioms([predicate(Name, Type1, Type2) | Preds]) :-
    include(=(predicate(_, Type1, Type2)), Preds, Synonyms),
    maplist(printSynonymAxioms(predicate(Name, Type1, Type2)), Synonyms),
    printSynonymAxioms(Preds).
printSynonymAxioms(predicate(Name1, Type1, Type2), predicate(Name2, Type1, Type2)) :-
    format('    ! x [~p] y [~p]: ~p(x, y) <=> ~p(x, y).~n', [Type1, Type2, Name1, Name2]).

printMain() :-
    writeln('procedure main() {'),
    writeln('    stdoptions.nbmodels = 2;'),
    writeln('    printmodels(modelexpand(T,S))'),
    writeln('    model = modelexpand(T,S)'),
    writeln('}').


nameVariables(FOL) :-
    term_variables(FOL, Vars),
    nameVariables(Vars, 0).
nameVariables([], _).
nameVariables([Var | Vars], Code) :-
    Code1 is Code + 1,
    codeToAtom(Code, Var),
    nameVariables(Vars, Code1).

codeToAtom(Code, Atom) :-
    Code < 26,
    !,
    C is Code + 97,
    atom_codes(Atom, [C]).
codeToAtom(Code, Atom) :-
    Code < 26*26,
    !,
    C1 is Code // 26 + 96,
    C2 is (Code mod 26) + 97,
    atom_codes(Atom, [C1, C2]).


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

getBaseTypes(Types, BaseTypes) :-
    maplist(getBaseTypesForType, Types, Temp1),
    flatten(Temp1, BaseTypesAtomsList),
    list_to_set(BaseTypesAtomsList, BaseTypesAtoms),
    writeln(BaseTypesAtoms),
    maplist(toBaseType(Types), BaseTypesAtoms, BaseTypes).

getBaseTypesForType(type(_, pred(T1, T2)), [T1, T2]) :-
    !.
getBaseTypesForType(type(_, fun(T1, T2)), [T1, T2]) :-
    !.
getBaseTypesForType(type(_, T), [T]) :-
    !.
getBaseTypesForType(attr(_, _), []) :-
    !.


toBaseType(Types, BaseType, baseType(BaseType, constructed:Symbols)) :-
    maplist(getPNsForBaseType(BaseType), Types, Symbols1),
    include(\=(null), Symbols1, Symbols),
    Symbols \= [],
    (
        include(=(attr(BaseType, countable)), Types, []),
        !
    ;
        error('Constructed type used as countable')
    ),
    !.
toBaseType(Types, BaseType, baseType(BaseType, int)) :-
    include(=(attr(BaseType, countable)), Types, X),
    X \= [],
    !.
toBaseType(_, Type, baseType(Type, unknown)).

getPNsForBaseType(BaseType, type(pn-Symbol, BaseType), Symbol) :-
    !.
getPNsForBaseType(_, _, null).

