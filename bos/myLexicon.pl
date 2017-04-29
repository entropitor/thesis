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
:- module(myLexicon, [lexEntry/2]).

/*========================================================================
    Determiners
========================================================================*/

lexEntry(det, [syntax:[the], mood:decl, num:sg, type:indef]).
lexEntry(det, [syntax:[every], mood:decl, num:sg, type:uni]).
lexEntry(det, [syntax:[a], mood:decl, num:sg, type:indef]).
lexEntry(det, [syntax:[an], mood:decl, num:sg, type:indef]).
%% lexEntry(det, [syntax:[one], mood:decl, num:sg, type:indef]).
lexEntry(det, [syntax:[some], mood:decl, num:sg, type:indef]).
lexEntry(det, [syntax:[no], mood:decl, num:sg, type:neg]).
lexEntry(det, [syntax:[the], mood:decl, num:sg, type:def]).
%% lexEntry(det, [syntax:[his], mood:decl, num:sg, type:poss(male)]).
%% lexEntry(det, [syntax:[her], mood:decl, num:sg, type:poss(female)]).
%% lexEntry(det, [syntax:[its], mood:decl, num:sg, type:poss(neuter)]).

%% lexEntry(det, [syntax:[some], mood:decl, num:pl, type:atleast(one)]).
%% lexEntry(det, [syntax:[several], mood:decl, num:pl, type:atleast(two)]).
%% lexEntry(det, [syntax:[two], mood:decl, num:pl, type:atleast(two)]).
%% lexEntry(det, [syntax:[three], mood:decl, num:pl, type:atleast(three)]).
%% lexEntry(det, [syntax:[four], mood:decl, num:pl, type:atleast(four)]).
%% lexEntry(det, [syntax:[five], mood:decl, num:pl, type:atleast(five)]).
%% lexEntry(det, [syntax:[no], mood:decl, num:pl, type:neg]).
%% lexEntry(det, [syntax:[all], mood:decl, num:pl, type:uni]).
%% lexEntry(det, [syntax:[most], mood:decl, num:pl, type:most]).
%% lexEntry(det, [syntax:[the], mood:decl, num:pl, type:def]).

/*========================================================================
    Numbers
========================================================================*/
lexEntry(number, [syntax:[one], number:1]).
lexEntry(number, [syntax:[two], number:2]).
lexEntry(number, [syntax:[three], number:3]).
lexEntry(number, [syntax:[four], number:4]).
lexEntry(number, [syntax:[five], number:5]).
lexEntry(number, [syntax:[six], number:6]).
lexEntry(number, [syntax:[seven], number:7]).
lexEntry(number, [syntax:[eight], number:8]).
lexEntry(number, [syntax:[nine], number:9]).
lexEntry(number, [syntax:[ten], number:10]).
lexEntry(number, [syntax:[eleven], number:11]).
lexEntry(number, [syntax:[twelve], number:12]).
lexEntry(number, [syntax:[thirtheen], number:13]).
lexEntry(number, [syntax:[fourteen], number:14]).
lexEntry(number, [syntax:[fifteen], number:15]).

/*========================================================================
    Copula
========================================================================*/
lexEntry(cop, [pol:pos, syntax:[is], inf:fin, num:sg]).
lexEntry(cop, [pol:neg, syntax:[isn, t], inf:fin, num:sg]).
lexEntry(cop, [pol:neg, syntax:[is, not], inf:fin, num:sg]).

lexEntry(cop, [pol:pos, syntax:[was], inf:fin, num:sg]).
lexEntry(cop, [pol:neg, syntax:[was, not], inf:fin, num:sg]).
lexEntry(cop, [pol:neg, syntax:[wasn, t], inf:fin, num:sg]).

lexEntry(cop, [pol:pos, syntax:[are], inf:fin, num:pl]).
lexEntry(cop, [pol:neg, syntax:[are, not], inf:fin, num:pl]).

lexEntry(cop, [pol:pos, syntax:[were], inf:fin, num:pl]).
lexEntry(cop, [pol:neg, syntax:[were, not], inf:fin, num:pl]).
lexEntry(cop, [pol:neg, syntax:[weren, t], inf:fin, num:pl]).

/*========================================================================
    Relative Pronouns
========================================================================*/
lexEntry(relpro, [syntax:[who]]).
lexEntry(relpro, [syntax:[which]]).
lexEntry(relpro, [syntax:[that]]).
%% lexEntry(relpro, [syntax:[]]).

/*========================================================================
    Coordinations
========================================================================*/
lexEntry(coord, [syntax:[and], type:conj]).
lexEntry(coord, [syntax:[or], type:disj]).
lexEntry(coord, [syntax:[nor], type:neg]).

lexEntry(coordPrefix, [syntax:[either], type:disj]).
lexEntry(coordPrefix, [syntax:[neither], type:neg]).

/*========================================================================
    Auxiliary Verbs
========================================================================*/
lexEntry(av, [syntax:[does], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[does, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[doesn, t], inf:fin, num:sg, pol:neg]).

lexEntry(av, [syntax:[do], inf:fin, num:pl, pol:pos]).
lexEntry(av, [syntax:[do, not], inf:fin, num:pl, pol:neg]).
lexEntry(av, [syntax:[don, t], inf:fin, num:pl, pol:neg]).

lexEntry(av, [syntax:[did], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[did, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[didn, t], inf:fin, num:sg, pol:neg]).

lexEntry(av, [syntax:[did], inf:fin, num:pl, pol:pos]).
lexEntry(av, [syntax:[did, not], inf:fin, num:pl, pol:neg]).
lexEntry(av, [syntax:[didn, t], inf:fin, num:pl, pol:neg]).

lexEntry(av, [syntax:[is], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[is, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[isn, t], inf:fin, num:sg, pol:neg]).

lexEntry(av, [syntax:[was], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[was, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[wasn, t], inf:fin, num:sg, pol:neg]).

lexEntry(av, [syntax:[will], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[will, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[won, t], inf:fin, num:sg, pol:neg]).

lexEntry(av, [syntax:[will], inf:fin, num:pl, pol:pos]).
lexEntry(av, [syntax:[will, not], inf:fin, num:pl, pol:neg]).
lexEntry(av, [syntax:[won, t], inf:fin, num:pl, pol:neg]).

/*========================================================================
    Nouns
========================================================================*/
%% lexEntry(noun, [symbol:animal, num:sg, syntax:[animal]]).
%% lexEntry(noun, [symbol:animal, num:pl, syntax:[animals]]).

/*========================================================================
    Proper Names
========================================================================*/
%% lexEntry(pn, [symbol:butch, syntax:[butch]]).

/*========================================================================
    Quantified Noun Phrases
========================================================================*/
lexEntry(qnp, [symbol:question_person, syntax:[who], mood:int, type:wh]).
lexEntry(qnp, [symbol:question_thing, syntax:[what], mood:int, type:wh]).

/*========================================================================
    Intransitive Verbs
========================================================================*/
%% lexEntry(iv, [symbol:dance, syntax:[dance], inf:inf, num:sg]).
%% lexEntry(iv, [symbol:dance, syntax:[dances], inf:fin, num:sg]).
%% lexEntry(iv, [symbol:dance, syntax:[dance], inf:fin, num:pl]).

/*========================================================================
    Transitive Verbs
========================================================================*/
%% lexEntry(tv, [symbol:clean, syntax:[clean], inf:inf, num:sg]).
%% lexEntry(tv, [symbol:clean, syntax:[cleans], inf:fin, num:sg]).
%% lexEntry(tv, [symbol:clean, syntax:[clean], inf:fin, num:pl]).

/*========================================================================
    Prepositions
========================================================================*/
%% lexEntry(prep, [symbol:in, syntax:[in]]).

/*========================================================================
    Adjectives
========================================================================*/
%% lexEntry(adj, [symbol:big, syntax:[big]]).

/*========================================================================
    Adverbs
========================================================================*/
%% lexEntry(adv, [symbol:quick, syntax:[quickly]]).

/*========================================================================
    Pronouns (third person)
========================================================================*/
%% lexEntry(pro, [symbol:male,  ref:no, syntax:[he]]).
%% lexEntry(pro, [symbol:male,  ref:no, syntax:[him]]).
%% lexEntry(pro, [symbol:male,  ref:yes, syntax:[himself]]).
%% lexEntry(pro, [symbol:female, ref:no, syntax:[she]]).
%% lexEntry(pro, [symbol:female, ref:yes, syntax:[herself]]).
%% lexEntry(pro, [symbol:female, ref:no, syntax:[her]]).
%% lexEntry(pro, [symbol:neuter, ref:no, syntax:[it]]).
%% lexEntry(pro, [symbol:neuter, ref:yes, syntax:[itself]]).


/*========================================================================
    Comparison operators
========================================================================*/
lexEntry(comp, [type:lower, syntax:[lower, than]]).
lexEntry(comp, [type:lower, syntax:[less, than]]).
lexEntry(comp, [type:lower, syntax:[below]]).
lexEntry(comp, [type:lower, syntax:[before]]).

lexEntry(comp, [type:higher, syntax:[higher, than]]).
lexEntry(comp, [type:higher, syntax:[more, than]]).
lexEntry(comp, [type:higher, syntax:[above]]).
lexEntry(comp, [type:higher, syntax:[after]]).

/*========================================================================
    Some phrase
========================================================================*/
lexEntry(somePhrase, [syntax:[sometime]]).
lexEntry(somePhrase, [syntax:[somewhat]]).
