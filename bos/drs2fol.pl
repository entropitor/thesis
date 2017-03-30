/*************************************************************************

     File: drs2fol.pl
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

:- module(drs2fol, [drs2fol/2]).


/*========================================================================
    Translate DRSs into FOL formulas
========================================================================*/

drs2fol(drs([], [Cond]), Formula) :-
    !,
    cond2fol(Cond, Formula).

drs2fol(drs([], Conds), and(Formula1, Formula2)) :-
    partition(isAlldifferent, Conds, AllDifferents, NormalConds),
    AllDifferents \= [],
    !,
    alldifferent2fol(AllDifferents, Formula1),
    drs2fol(drs([], NormalConds), Formula2).

drs2fol(drs([], [Cond1, Cond2|Conds]), and(Formula1, Formula2)) :-
    !,
    cond2fol(Cond1, Formula1),
    drs2fol(drs([], [Cond2|Conds]), Formula2).

drs2fol(drs([X|Referents], Conds), some(X, Formula)) :-
    !,
    drs2fol(drs(Referents, Conds), Formula).


/*========================================================================
    Translate DRS-Conditions into FOL formulas
========================================================================*/

cond2fol(not(Drs), not(Formula)) :-
    drs2fol(Drs, Formula).

cond2fol(or(Drs1, Drs2), or(Formula1, Formula2)) :-
    drs2fol(Drs1, Formula1),
    drs2fol(Drs2, Formula2).

cond2fol(imp(drs([], Conds), Drs2), imp(Formula1, Formula2)) :-
    drs2fol(drs([], Conds), Formula1),
    drs2fol(Drs2, Formula2).

cond2fol(imp(drs([X|Referents], Conds), Drs2), all(X, Formula)) :-
    cond2fol(imp(drs(Referents, Conds), Drs2), Formula).

cond2fol(eq(X, Y), eq(X, Y)).

cond2fol(eq(X), eq(X)).

cond2fol(pred(Sym, X), pred(Sym, X)).

cond2fol(rel(Sym, X, Y), rel(Sym, X, Y)).

/*========================================================================
    Translate alldifferent constraints into FOL formulas
========================================================================*/

isAlldifferent(alldifferent(_)).
alldifferentToVariable(alldifferent(X), X).

alldifferent2fol(AllDifferents, Formula) :-
    maplist(alldifferentToVariable, AllDifferents, Variables),
    alldifferentVariablesToConstraints(Variables, Formula).

alldifferentVariablesToConstraints([A, B], not(eq(A, B))) :-
    !.
alldifferentVariablesToConstraints([A, B | C], and(Formula1, Formula2)) :-
    variableDifferentFromOthers(A, [B|C], Formula1),
    alldifferentVariablesToConstraints([B | C], Formula2).

variableDifferentFromOthers(A, [B], not(eq(A, B))) :-
    !.
variableDifferentFromOthers(A, [B, C | D], and(not(eq(A, B)), Formula)) :-
    variableDifferentFromOthers(A, [C | D], Formula).
