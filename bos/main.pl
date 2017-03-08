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
                 prefix/0]).

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

:- use_module(types, [combineTypes/2]).

:- use_module(myGrammar, [t/3]).

:- use_module(problems, [problem/2]).

/*========================================================================
    Driver Predicates
========================================================================*/

test :-
    testAll([
                   zebra,
                   p1
                 %% thieves,
                 %% translators,
                 %% swimming_suits
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
            ), Bags),
    findall(X, (
                member(X, CombinedTypes),
                term_variables(X, [])
            ), KnownTypes),
    writeln(KnownTypes),
    maplist(writeln, Bags),
    nl.


testp(Problem, [Problem, NbCorrect, NbSentences], FlattenTypes) :-
    problem(Problem, Sentences),
    format('~n###############################~n###   ~p~n###############################~n', [Problem]),
    maplist(testSentence, Sentences, NbDRSes, Types),
    flatten(Types, FlattenTypes),
    findall(1, member(1, NbDRSes), L),
    length(L, NbCorrect),
    length(Sentences, NbSentences),
    nl, print(NbDRSes),
    format('~nNumber of sentences with 1 meaning: ~p/~p', [NbCorrect, NbSentences]).

testSentence(Sentence, NbDRS, Types) :-
    format('~nSentence: ~p', [Sentence]),
    test(Sentence, DRSs, Types),
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
                         b_getval(types, Types)
                     ;
                         nl, write('failed conversion: '),
                         numbervars(Drs, 0, _),
                         print(Drs),
                         fail
                     )
                  ), Sols),
     pairs_keys_values(Sols, Sems, Types).
