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
    pairs_keys_values(Pairs, Sentences, FOLs),
    nameTypes(Types),
    writeln(Types),
    nameVariables(FOLs),
    \+ \+ printFile(Problem, Pairs).

printFile(Problem, Pairs) :-
    problemToFileName(Problem, FileName),
    tell(FileName),
    write('// Problem '),
    writeln(Problem),
    nl,
    printVocabulary(),
    nl,
    printStructure(),
    nl,
    printTheory(Pairs),
    nl,
    printMain(),
    nl,
    told.
problemToFileName(Problem, FileName) :-
    atom_concat('output/', Problem, Temp1),
    atom_concat(Temp1, '.idp', FileName).

printVocabulary() :-
    writeln('vocabulary V {'),
    writeln('}').
printStructure() :-
    writeln('structure S : V {'),
    writeln('}').
printTheory(Pairs) :-
    writeln('theory T : V {'),
    maplist(printSentence, Pairs),
    writeln('}').
printMain() :-
    writeln('procedure main() {'),
    writeln('    stdoptions.nbmodels = 2;'),
    writeln('    printmodels(modelexpand(T,S))'),
    writeln('    model = modelexpand(T,S)'),
    writeln('}').


printSentence(Sentence-FOL) :-
    write('    // '),
    writeln(Sentence),
    write('    '),
    printFol(idp, FOL).

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
