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
    Puzzle: Zebra
========================================================================*/

concept(person, constructed:[[the, englishman], [the, spaniard], [the, ukrainian], [the, japanese], [the, norwegian]]).
concept(house, nominal).
concept(color, constructed:[[red], [green], [ivory], [yellow], [blue]]).
concept(animal, constructed:[[the, dog], [the, zebra], [the, snail], [the, fox], [the, horse]]).
concept(drink, constructed:[[coffee], [tea], [milk], [water], [orange, juice]]).
concept(cigarette, constructed:[[chesterfields], [kools], [parliaments], [old, gold], [lucky, strike]]).

property(house, [position], int).
property(house, [color], color).

relation(house, house, [is, next], [to], [is, next]).
relation(person, house, [lives], [in], [live]).
relation(person, animal, [keeps], [], [keep]).
relation(person, drink, [drinks], [], [drink]).
relation(person, cigarette, [smokes], [], [smoke]).

actor(person, cigarette, [smoker], [of], [smokes]).

/*========================================================================
    Puzzle: p1
========================================================================*/
concept(species, constructed:[[the, perens, pig], [the, byengo, bat], [the, nibner, newt], [the, eldar, elk], [the, osbele, oryx]]).
concept(population, constructed:[[210], [280], [315], [490], [525]]).
concept(country, constructed:[[ghana], [honduras], [poland], [russia], [slovakia]]).
concept(year, constructed:[[2006], [2007], [2008], [2009], [2010]]).

property(species, [population, size], population).

%% relation(species, country, [lives], [in], [live]).
relation(species, year, [is, recognized, as, endangered], [in], [is, recognized, as, endangered]).
relation(species, year, [is, listed], [in], [is, listed]).
relation(species, population, [has, a, population, size], [of], [have, a, population, size]).


/*========================================================================
    Puzzle: p2
========================================================================*/
concept(contestant, constructed:[[bill], [colin], [ira], [oscar], [pedro]]).
concept(darts, constructed:[[the, black, darts], [the, orange, darts], [the, red, darts], [the, white, darts], [the, yellow, darts]]).
concept(city, constructed:[[mount, union], [gillbertville], [lohrville], [worthington], [yorktown]]).
concept(total, countable:[points]).


relation(contestant, darts, [threw], [], [throw]).
relation(contestant, score, [scored], [], [score]).

actor(contestant, city, [blabla], [from], [from]).

