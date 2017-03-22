/*************************************************************************

     File: lambdaDRT.pl
     Copyright (C) 2004, 2006 Patrick Blackburn & Johan Bos

     This file is part of BB2, version 2.0 (November 2006).

     BB2 is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.

     BB2 is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with BB2; if not, write to the Free Software Foundation, Inc.,
     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(main, [test/0,
                 testAll/1,
                 test/1,
                 test/3,
                 lambdaDRT/4,
                 infix/0,
                 prefix/0,
                 useLexicon/1
                ]).

:- use_module(readLine, [readLine/1,
                        readFromString/2]).

:- use_module(comsemPredicates, [prefix/0,
                                 infix/0,
                                 printRepresentations/2]).

:- use_module(betaConversionDRT, [betaConvert/2]).
:- use_module(mergeDRT, [mergeDrs/2]).
simplify(X, Y) :-
    betaConvert(X, X1),
    mergeDrs(X1, Y).

:- use_module(types, [combineTypes/2, nameTypes/1]).

:- use_module(myGrammar, [t/3]).

:- use_module(problems, [problem/2]).
:- use_module(problemLexiconRules, [useLexicon/1]).

:- use_module(drs2fol, [drs2fol/2]).
:- use_module(printFol, [printFol/1]).

/*========================================================================
    Driver Predicates
========================================================================*/

test :-
    testAll([
                   p1,
                   p2
             ]).

% Test the list of problems
% fails if any one of the problems fails
testAll(Problems) :-
    maplist(testp, Problems, Resultss, Typess),
    format('~n~n~nResults (Number of sentences with 1 meaning):~n#############################################~n'),
    maplist(printResults, Resultss, Typess),
    \+ (
        member([_, X, Y], Resultss),
        X \= Y
    ).

printResults(Results, Types) :-
    format('Problem ~p: ~p/~p~n', Results),
    typesToSetOfVariables(Types, FixedTypes, TypesPerVariable),
    writeln(FixedTypes),
    maplist(writeln, TypesPerVariable),
    nl.

typesToSetOfVariables(Types, FixedTypes, TypesPerVariable) :-
    combineTypes(Types, CombinedTypes),
    findall(Bag, (
                group_by(Var, X, (
                             maplist(term_variables, CombinedTypes, Vars),
                             pairs_keys_values(Pairs, CombinedTypes, Vars),
                             numbervars(CombinedTypes, 0, End),
                             between(0, End, VarNb),
                             Var =.. ['$VAR', VarNb],
                             member(X-XVars, Pairs),
                             member(Var, XVars)
                         ), Bag)
            ), TypesPerVariable),
    findall(X, (
                member(X, CombinedTypes),
                term_variables(X, [])
            ), FixedTypes).


testp(Problem, [Problem, NbCorrect, NbSentences], FlattenTypes) :-
    useLexicon(Problem),
    problem(Problem, Sentences),
    format('~n###############################~n###   ~p~n###############################~n', [Problem]),
    maplist(testSentence, Sentences, NbDRSes, Results),
    filterResults(Results, NewResults),
    length(NewResults, NbResults),
    maplist(pairs_keys_values, NewResults, NewDRSss, Types),
    flatten(Types, FlattenTypes),
    findall(1, member(1, NbDRSes), L),
    length(L, NbCorrect),
    length(Sentences, NbSentences),
    nl, print(NbDRSes),
    format('~nNumber of sentences with 1 meaning: ~p/~p~nNumber of possible meanings in total: ~p', [NbCorrect, NbSentences, NbResults]),
    nl,
    maplist(toFol(Sentences), Types, NewDRSss).

toFol(Sentences, Types, DRSs) :-
    maplist(drs2fol, DRSs, FOLs),
    pairs_keys_values(Pairs, Sentences, FOLs),
    nl,
    flatten(Types, FlattenedTypes),
    \+ \+ (nameTypes(FlattenedTypes), numbervars(FlattenedTypes, 0, End), numbervars(FOLs, End, _), printTypes(FlattenedTypes), maplist(printSentence, Pairs)).

printTypes(Types) :-
    maplist(getType, Types, RealTypes),
    list_to_set(RealTypes, AllTypes),
    maplist(getRepresentativeForType(Types), AllTypes, Representatives),
    pairs_keys_values(Pairs, AllTypes, Representatives),
    write('Types: '),
    print(Pairs),
    nl.
getType(type(_, Type), Type).
getRepresentativeForType([type(_-Var, Type) | _], Type, Var) :-
    !.
getRepresentativeForType([_ | Types], Type, Var) :-
    getRepresentativeForType(Types, Type, Var).
printSentence(Sentence-FOL) :-
    write('// '),
    writeln(Sentence),
    printFol(FOL).


filterResults(Results, NewResults) :-
    findall(PossibleResult, (
                maplist(memberIfMultiple, PossibleResult, Results),
                pairs_keys_values(PossibleResult, _DRSs, Types),
                flatten(Types, FlattenTypes),
                combineTypes(FlattenTypes, NewTypes),
                \+ endoPredicateType(NewTypes)
            ), NewResults).

endoPredicateType([type(_, pred(X, Y)) | _]) :-
    X == Y.
endoPredicateType([_ | Types]) :-
    endoPredicateType(Types).

memberIfMultiple(fail-[], []).
memberIfMultiple(H, [H|_]).
memberIfMultiple(X, [_|T]) :-
    member(X, T).

testSentence(Sentence, NbDRS, Results) :-
    format('~nSentence: ~p', [Sentence]),
    test(Sentence, DRSs, Types),
    pairs_keys_values(Results, DRSs, Types),
    length(DRSs, NbDRS).

test(String) :-
    test(String, _, _).
test(String, DRSs, Types) :-
    readFromString(String, Discourse),
    lambdaDRT(Discourse, drs([], []), DRSs, Types),
    printRepresentations(DRSs, Types).

lambdaDRT(Discourse, Old, Sems, Types) :-
     b_setval(types, []),
     findall(Sem-Types, (
                     t([sem:Drs], Discourse, []),
                     (
                         simplify(merge(Old, Drs), Sem)
                     ->
                         b_getval(types, Types1),
                         combineTypes(Types1, Types)
                     ;
                         nl, write('failed conversion: '),
                         numbervars(Drs, 0, _),
                         print(Drs),
                         fail
                     )
                  ), Sols),
     pairs_keys_values(Sols, Sems, Types).
