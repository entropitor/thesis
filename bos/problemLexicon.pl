/*************************************************************************

     File: englishMyLexicon.pl
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
:- module(problemLexicon, [
              pLexicon/2,
              concept/2,
              property/3,
              relation/5,
              actor/5
          ]).

/*========================================================================
    Puzzle: general predicates
========================================================================*/
:- discontiguous problemLexicon:concept/2.
:- discontiguous problemLexicon:property/3.
%% :- discontiguous problemLexicon:relation/2.
:- discontiguous problemLexicon:relation/5.
:- discontiguous problemLexicon:actor/5.

/*========================================================================
    Puzzle: p1
========================================================================*/
pLexicon(p1, [
            noun(species, [species], [species]),
            noun(species, [animal], [animals]),
            pn(species, [the, perens, pig]),
            pn(species, [the, byengo, bat]),
            pn(species, [the, nibner, newt]),
            pn(species, [the, eldar, elk]),
            pn(species, [the, osbele, oryx]),
            %% noun(country, [country], [countries]),
            pn(country, [ghana]),
            pn(country, [honduras]),
            pn(country, [poland]),
            pn(country, [russia]),
            pn(country, [slovakia]),
            noun(year, [year], [years]),
            ivpp(pred(species, country), [lives], [in], [live]),
            ivpp(pred(species, year), [was, recognized, as, endangered], [in], [are, recognized, as, endangered]),
            ivpp(pred(species, year), [was, listed], [in], [are, listed]),
            ivpp(pred(species, population), [has, a, population, size], [of], [have, a, population, size]),
            ivpp(pred(species, population), [has, a, surviving, population, size], [of], [have, a, surviving, population, size]),
            prep(fun(country, species), [from]),
            prep(fun(population, species), [with, a, population, size, of])
        ]).

/*========================================================================
    Puzzle: p2
========================================================================*/
pLexicon(p2, [
            noun(contestant, [contestant], [contestants]),
            noun(contestant, [person], [persons]),
            noun(contestant, [player], [players]),
            pn(contestant, [bill]),
            pn(contestant, [colin]),
            pn(contestant, [ira]),
            pn(contestant, [oscar]),
            pn(contestant, [pedro]),
            pn(darts, [the, black, darts]),
            pn(darts, [the, orange, darts]),
            pn(darts, [the, red, darts]),
            pn(darts, [the, white, darts]),
            pn(darts, [the, yellow, darts]),
            pn(city, [mount, union]),
            pn(city, [gillbertville]),
            pn(city, [lohrville]),
            pn(city, [worthington]),
            pn(city, [yorktown]),
            noun(score, [point], [points]),
            tv(pred(contestant, darts), [threw], [throw]),
            tv(pred(contestant, score), [scored], [score]),
            ivpp(pred(contestant, score), [finishes], [with], [finish]),
            prep(fun(city, contestant), [from])
        ]).

