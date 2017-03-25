:- module(solution2idp, [
              solution2idp/3
          ]).

:- use_module(drs2fol, [drs2fol/2]).
:- use_module(printFol, [printFol/2]).
:- use_module(types, [
                  nameTypes/1
              ]).
:- use_module(typeExtraction, [
                  getPredicates/2,
                  getBaseTypes/4,
                  getDerivedTypes/3
              ]).
:- use_module(questions, [
                  setQuestionTopic/1,
                  clearQuestionTopic/0
              ]).

solution2idp(solution(Sentences, DRSs, Types), ProblemName, Problem) :-
    setQuestionTopic(ProblemName),
    Problem = p(NbBaseTypes, NbConceptsPerType, _),
    maplist(drs2fol, DRSs, FOLs),
    pairs_keys_values(SentencePairs, Sentences, FOLs),
    nameTypes(Types),
    nameVariables(FOLs),
    getPredicates(Types, Predicates),
    getBaseTypes(Types, BaseTypes, NbBaseTypes, NbConceptsPerType),
    getDerivedTypes(Types, BaseTypes, DerivedTypes),
    \+ \+ printFile(ProblemName, SentencePairs, voc(BaseTypes, DerivedTypes, Predicates)),
    clearQuestionTopic.

printFile(ProblemName, SentencePairs, Vocabularium) :-
    problemToFileName(ProblemName, FileName),
    tell(FileName),
    write('// Problem '),
    writeln(ProblemName),
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
problemToFileName(ProblemName, FileName) :-
    atom_concat('output/', ProblemName, Temp1),
    atom_concat(Temp1, '.idp', FileName).

printVocabulary(voc(BaseTypes, DerivedTypes, Predicates)) :-
    writeln('vocabulary V {'),
    maplist(printType, BaseTypes),
    maplist(printType, DerivedTypes),
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
printType(baseType(Type, int:Range)) :-
    !,
    atomic_list_concat(Range, '; ', RangeString),
    format('    type ~p = {~w} isa int~n', [Type, RangeString]).
printType(baseType(Type, X)) :-
    !,
    format('    type ~p //~p~n', [Type, X]).
printType(derivedType(Type, BaseType, int:Range)) :-
    !,
    atomic_list_concat(Range, '; ', RangeString),
    format('    type ~p = {~w} isa int // differences between values of type ~p~n', [Type, RangeString, BaseType]).
printPredicate(predicate(Name, Type1, Type2)) :-
    format('    ~p(~p, ~p)~n', [Name, Type1, Type2]).

printStructure() :-
    writeln('structure S : V {'),
    writeln('}').

printTheory(SentencePairs, voc(_, _, Predicates)) :-
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
    writeln('    stdoptions.nbmodels = 10;'),
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


