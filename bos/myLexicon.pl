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

:- discontiguous myLexicon:lexEntry/2.

addType(_, _) :-
  \+ nb_current(types, _),
  nb_setval(types, []),
  fail.
addType(Symbol, Type) :-
  b_getval(types, Types),
  b_setval(types, [type(Symbol, Type) | Types]).

/*========================================================================
    Puzzle: general predicates
========================================================================*/
lexEntry(det, [syntax:[the], mood:decl, num:sg, type:indef]).

lexEntry(noun, [symbol:Symbol, num:sg, syntax:Syntax, vType:Type]) :-
    concept(Symbol, _),
    addType(Symbol, Type),
    symbol_syntax(Symbol, Syntax).
lexEntry(pn, [symbol:Symbol, syntax:Syntax, vType:Type]) :-
    concept(_Type, constructed:Elements),
    addType(Symbol, Type),
    member(Syntax, Elements),
    % TODO check that it isn't an adjective?
    syntax_symbol(Syntax, Symbol).
lexEntry(adj, [symbol:Symbol, syntax:Syntax, vType:Type]) :-
    concept(Concept, constructed:Elements),
    property(_Type, _, Concept),
    addType(Symbol, Type),
    member(Syntax, Elements),
    syntax_symbol(Syntax, Symbol).
lexEntry(adj, [symbol:Symbol, syntax:Syntax, vType:adj(Type)]) :-
    member(Syntax, [[first], [second], [third], [fourth], [fifth], [sixth]]),
    property(_Type, _, int),
    addType(Symbol, adj(Type)),
    syntax_symbol(Syntax, Symbol).
% TODO: need other wordforms?
lexEntry(tv, [symbol:Symbol, syntax:Syntax, inf:fin, num:sg, vType:pred(SubjType, ObjType)]) :-
    % relation(SubjType, ObjType, Syntax, []),
    relation(_SubjType, _ObjType, Syntax, []),
    addType(Symbol, pred(SubjType, ObjType)),
    syntax_symbol(Syntax, Symbol).
lexEntry(ivpp, [symbol:Symbol, syntax:Syntax, pp:PP, inf:fin, num:sg, vType:pred(SubjType, ObjType)]) :-
    %% relation(SubjType, ObjType, Syntax, PP),
    relation(_SubjType, _ObjType, Syntax, PP),
    PP \= [],
    addType(Symbol, pred(SubjType, ObjType)),
    append(Syntax, PP, WordForm),
    syntax_symbol(WordForm, Symbol).
lexEntry(prep, [symbol:Symbol, syntax:PP, vType:null]) :-
    relation(_, _, _, PP),
    PP \= [],
    syntax_symbol(PP, Symbol).
lexEntry(iv, [symbol:Symbol, syntax:Syntax, inf:fin, num:sg, vType:Type]) :-
    relation(_Type, Syntax),
    addType(Symbol, Type),
    syntax_symbol(Syntax, Symbol).

lexEntry(prep, [symbol:Symbol, syntax:PP, vType:fun(SubjType, ObjType)]) :-
    actor(_, _, SyntaxNoun, PP, _),
    append(SyntaxNoun, PP, Syntax),
    addType(Symbol, fun(SubjType, ObjType)),
    syntax_symbol(Syntax, Symbol).
lexEntry(noun, [symbol:Symbol, num:sg, syntax:Syntax, vType:Type]) :-
    actor(_, _, Syntax, _, _),
    addType(Symbol, Type),
    syntax_symbol(Syntax, Symbol).

syntax_symbol(Syntax, Symbol) :-
    atomic_list_concat(Syntax, '_', Symbol).
symbol_syntax(Symbol, Syntax) :-
    split_string(Symbol, '_', '', L),
    maplist(atom_chars, Syntax, L).

:- discontiguous myLexicon:concept/2.
:- discontiguous myLexicon:property/3.
:- discontiguous myLexicon:relation/2.
:- discontiguous myLexicon:relation/4.
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

relation(house, house, [is, next], [to]).
relation(person, house, [lives], [in]).
relation(person, animal, [keeps], []).
relation(person, drink, [drinks], []).
relation(person, cigarette, [smokes], []).

actor(person, cigarette, [smoker], [of], [smokes]).

/*========================================================================
    Determiners
========================================================================*/

lexEntry(det, [syntax:[every], mood:decl, num:sg, type:uni]).
lexEntry(det, [syntax:[a], mood:decl, num:sg, type:indef]).
lexEntry(det, [syntax:[an], mood:decl, num:sg, type:indef]).
lexEntry(det, [syntax:[one], mood:decl, num:sg, type:indef]).
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
    Copula
========================================================================*/
lexEntry(cop, [pol:pos, syntax:[is], inf:fin, num:sg]).
lexEntry(cop, [pol:neg, syntax:[is, not], inf:fin, num:sg]).
lexEntry(cop, [pol:pos, syntax:[are], inf:fin, num:pl]).
lexEntry(cop, [pol:neg, syntax:[are, not], inf:fin, num:pl]).

/*========================================================================
    Relative Pronouns
========================================================================*/
lexEntry(relpro, [syntax:[who]]).
lexEntry(relpro, [syntax:[which]]).
lexEntry(relpro, [syntax:[that]]).

/*========================================================================
    Coordinations
========================================================================*/
lexEntry(coord, [syntax:[and], type:conj]).
lexEntry(coord, [syntax:[or], type:disj]).

/*========================================================================
    Auxiliary Verbs
========================================================================*/
lexEntry(av, [syntax:[does], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[does, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[do], inf:fin, num:pl, pol:pos]).
lexEntry(av, [syntax:[do, not], inf:fin, num:pl, pol:neg]).
lexEntry(av, [syntax:[did], inf:fin, num:sg, pol:pos]).
lexEntry(av, [syntax:[did, not], inf:fin, num:sg, pol:neg]).
lexEntry(av, [syntax:[did], inf:fin, num:pl, pol:pos]).
lexEntry(av, [syntax:[did, not], inf:fin, num:pl, pol:neg]).

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
