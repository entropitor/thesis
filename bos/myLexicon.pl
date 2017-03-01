/*************************************************************************

     File: englishLexicon.pl
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

%% lexEntry(iv, [symbol:dance, syntax:[dance], inf:inf, num:sg, vType:person]).
%% lexEntry(iv, [symbol:dance, syntax:[dances], inf:fin, num:sg, vType:Type]) :-
%%     addType(dance, Type).
%% lexEntry(iv, [symbol:dance, syntax:[dance], inf:fin, num:pl, vType:Type]) :-
%%     addType(dance, Type).
%% lexEntry(pn, [symbol:english_wizard, syntax:[the, englishman], vType:Type]) :-
%%     addType(english_wizard, Type).

/*========================================================================
    Puzzle: general predicates
========================================================================*/
lexEntry(det, [syntax:[the], mood:decl, num:sg, type:uni]).

lexEntry(noun, [symbol:Symbol, num:sg, syntax:Syntax, vType:Symbol]) :-
    concept(Symbol, _),
    symbol_syntax(Symbol, Syntax).
lexEntry(pn, [symbol:Symbol, syntax:Syntax, vType:Type]) :-
    concept(_Type, constructed:Elements),
    addType(Symbol, Type),
    member(Syntax, Elements),
    % TODO check that it isn't an adjective?
    syntax_symbol(Syntax, Symbol).
lexEntry(adj, [symbol:Symbol, syntax:Syntax, vType:Type]) :-
    concept(Concept, constructed:Elements),
    property(Type, _, Concept),
    member(Syntax, Elements),
    syntax_symbol(Syntax, Symbol).
lexEntry(adj, [symbol:Symbol, syntax:Syntax, vType:Type]) :-
    member(Syntax, [[first], [second], [third], [fourth], [fifth], [sixth]]),
    property(Type, _, int),
    syntax_symbol(Syntax, Symbol).
% TODO: need other wordforms?
lexEntry(tv, [symbol:Symbol, syntax:Syntax, inf:fin, num:sg, vType:pred(SubjType, ObjType)]) :-
    relation(SubjType, ObjType, Syntax, []),
    %addType(Symbol, pred(SubjType, ObjType)),
    syntax_symbol(Syntax, Symbol).
lexEntry(ivpp, [symbol:Symbol, syntax:Syntax, pp:PP, inf:fin, num:sg, vType:pred(SubjType, ObjType)]) :-
    relation(SubjType, ObjType, Syntax, PP),
    PP \= [],
    append(Syntax, PP, WordForm),
    syntax_symbol(WordForm, Symbol).
lexEntry(iv, [symbol:Symbol, syntax:Syntax, inf:fin, num:sg, vType:Type]) :-
    relation(Type, Syntax),
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

%% lexEntry(det, [syntax:[the], mood:decl, num:sg, type:uni]).

%% lexEntry(noun, [symbol:person, num:sg, syntax:[person]]).
%% lexEntry(noun, [symbol:house, num:sg, syntax:[house]]).
%% lexEntry(noun, [symbol:animal, num:sg, syntax:[animal]]).
%% %% lexEntry(noun, [symbol:color, num:sg, syntax:[color]]).
%% %% lexEntry(noun, [symbol:position, num:sg, syntax:[position]]).

%% lexEntry(noun, [symbol:englishman, num:sg, syntax:[englishman]]).
%% lexEntry(noun, [symbol:spaniard, num:sg, syntax:[spaniard]]).
%% lexEntry(noun, [symbol:ukrainian, num:sg, syntax:[ukrainian]]).
%% lexEntry(noun, [symbol:japanese, num:sg, syntax:[japanese]]).
%% lexEntry(noun, [symbol:norwegian, num:sg, syntax:[norwegian]]).

%% lexEntry(noun, [symbol:dog, num:sg, syntax:[dog]]).
%% lexEntry(noun, [symbol:zebra, num:sg, syntax:[zebra]]).
%% lexEntry(noun, [symbol:snail, num:sg, syntax:[snail]]).
%% lexEntry(noun, [symbol:fox, num:sg, syntax:[fox]]).
%% lexEntry(noun, [symbol:horse, num:sg, syntax:[horse]]).

%% lexEntry(pn, [symbol:chesterfields, syntax:[chesterfields]]).
%% lexEntry(pn, [symbol:kools, syntax:[kools]]).
%% lexEntry(pn, [symbol:parliaments, syntax:[parliaments]]).
%% lexEntry(pn, [symbol:old_gold, syntax:[old, gold]]).
%% lexEntry(pn, [symbol:lucky_strike, syntax:[lucky, strike]]).

%% lexEntry(pn, [symbol:coffee, syntax:[coffee]]).
%% lexEntry(pn, [symbol:tea, syntax:[tea]]).
%% lexEntry(pn, [symbol:milk, syntax:[milk]]).
%% lexEntry(pn, [symbol:water, syntax:[water]]).
%% lexEntry(pn, [symbol:orange_juice, syntax:[orange, juice]]).

%% lexEntry(adj, [symbol:red, syntax:[red]]).
%% lexEntry(adj, [symbol:green, syntax:[green]]).
%% lexEntry(adj, [symbol:ivory, syntax:[ivory]]).
%% lexEntry(adj, [symbol:yellow, syntax:[yellow]]).
%% lexEntry(adj, [symbol:blue, syntax:[blue]]).

%% lexEntry(adj, [symbol:first, syntax:[first]]).
%% lexEntry(adj, [symbol:second, syntax:[second]]).
%% lexEntry(adj, [symbol:third, syntax:[third]]).
%% lexEntry(adj, [symbol:fourth, syntax:[fourth]]).
%% lexEntry(adj, [symbol:fifth, syntax:[fifth]]).

%% lexEntry(tv, [symbol:Symbol, syntax:Syntax, inf:inf, num:sg]) :-
%%     tv(Syntax, Symbol).
%% lexEntry(tv, [symbol:Symbol, syntax:[Hs|T], inf:fin, num:sg]) :-
%%     tv([H|T], Symbol),
%%     atom_concat(H, s, Hs).
%% lexEntry(tv, [symbol:Symbol, syntax:Syntax, inf:fin, num:pl]) :-
%%     tv(Syntax, Symbol).
%% tv([keep], keep).
%% tv([drink], drink).
%% tv([smoke], smoke).

%% lexEntry(tv, [symbol:is_next_to, syntax:[be, next, to], inf:inf, num:sg]).
%% lexEntry(tv, [symbol:is_next_to, syntax:[is, next, to], inf:fin, num:sg]).
%% lexEntry(tv, [symbol:is_next_to, syntax:[are, next, to], inf:fin, num:pl]).

%% lexEntry(ivpp, [symbol:live_in, syntax:[live], pp:[in], inf:inf, num:sg]).
%% lexEntry(ivpp, [symbol:live_in, syntax:[lives], pp:[in], inf:fin, num:sg]).
%% lexEntry(ivpp, [symbol:live_in, syntax:[live], pp:[in], inf:fin, num:pl]).


/*========================================================================
    Puzzle: Translators
========================================================================*/

%% concept(translator, constructed:[[the, spaniard], [the, englishman], [the, frenchman], [the, german], [the, russian]]).
%% concept(language, constructed:[[spanish], [english], [french], [german], [russian]]).
%% concept(native_language, constructed:[[spanish], [english], [french], [german], [russian]]). % inherits language!

%% property(translator, [native, language], native_language).

%% relation(translator, language, [speaks], []).

/*========================================================================
    Puzzle: Thieves
========================================================================*/

%% concept(thief, constructed:[[albert], [bob], [charley], [damian], [ernest]]).
%% concept(role, constructed:[[the, hacker], [the, overlooker], [the, driver], [the, driller], [the, accessory, after, the, fact]]).

%% property(translator, [native, language], native_language).

%% relation(thief, role, [plays], []).
%% relation(thief, role, [knows], []).
%% relation(thief, [attends, to, the, meeting]).

/*========================================================================
    Puzzle: Swimming Suits
========================================================================*/

%% concept(contestant, nominal).
%% concept(first_name, constructed:[[rachel], [melony], [amelia], [julia], [sarah]]).
%% concept(last_name, constructed:[[travers], [james], [west], [couch], [sanford]]).
%% concept(bathing_suit, nominal).
%% concept(nb_pieces, constructed:[['1-piece'], ['2-piece']]).
%% concept(color, constructed:[[red], [white], [yellow], [blue], [black]]).
%% concept(place, inherits:int).

%% property(contestant, [first, name], first_name).
%% property(contestant, [last, name], last_name).
%% property(contestant, [position], place).
%% property(bathing_suit, [type, of], nb_pieces).
%% property(bathing_suit, [color], color).

%% relation(contestant, bathing_suit, [wears], []).

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
lexEntry(det, [syntax:[his], mood:decl, num:sg, type:poss(male)]).
lexEntry(det, [syntax:[her], mood:decl, num:sg, type:poss(female)]).
lexEntry(det, [syntax:[its], mood:decl, num:sg, type:poss(neuter)]).

lexEntry(det, [syntax:[some], mood:decl, num:pl, type:atleast(one)]).
lexEntry(det, [syntax:[several], mood:decl, num:pl, type:atleast(two)]).
lexEntry(det, [syntax:[two], mood:decl, num:pl, type:atleast(two)]).
lexEntry(det, [syntax:[three], mood:decl, num:pl, type:atleast(three)]).
lexEntry(det, [syntax:[four], mood:decl, num:pl, type:atleast(four)]).
lexEntry(det, [syntax:[five], mood:decl, num:pl, type:atleast(five)]).
lexEntry(det, [syntax:[no], mood:decl, num:pl, type:neg]).
lexEntry(det, [syntax:[all], mood:decl, num:pl, type:uni]).
lexEntry(det, [syntax:[most], mood:decl, num:pl, type:most]).
lexEntry(det, [syntax:[the], mood:decl, num:pl, type:def]).



/*========================================================================
    Nouns
========================================================================*/

lexEntry(noun, [symbol:animal, num:sg, syntax:[animal]]).
lexEntry(noun, [symbol:animal, num:pl, syntax:[animals]]).
lexEntry(noun, [symbol:beverage, num:sg, syntax:[beverage]]).
lexEntry(noun, [symbol:beverage, num:pl, syntax:[beverages]]).
lexEntry(noun, [symbol:building, num:sg, syntax:[building]]).
lexEntry(noun, [symbol:building, num:pl, syntax:[buildings]]).
lexEntry(noun, [symbol:cup, num:sg, syntax:[cup]]).
lexEntry(noun, [symbol:cup, num:pl, syntax:[cups]]).
lexEntry(noun, [symbol:burger, num:sg, syntax:[burger]]).
lexEntry(noun, [symbol:burger, num:pl, syntax:[burgers]]).
lexEntry(noun, [symbol:boxer, num:sg, syntax:[boxer]]).
lexEntry(noun, [symbol:boxer, num:pl, syntax:[boxers]]).
lexEntry(noun, [symbol:boss, num:sg, syntax:[boss]]).
lexEntry(noun, [symbol:boss, num:pl, syntax:[bosses]]).
lexEntry(noun, [symbol:car, num:sg, syntax:[car]]).
lexEntry(noun, [symbol:car, num:pl, syntax:[cars]]).
lexEntry(noun, [symbol:chainsaw, num:sg, syntax:[chainsaw]]).
lexEntry(noun, [symbol:chainsaw, num:pl, syntax:[chainsaws]]).
lexEntry(noun, [symbol:criminal, num:sg, syntax:[criminal]]).
lexEntry(noun, [symbol:criminal, num:pl, syntax:[criminals]]).
lexEntry(noun, [symbol:customer, num:sg, syntax:[customer]]).
lexEntry(noun, [symbol:customer, num:pl, syntax:[customers]]).
lexEntry(noun, [symbol:drug, num:sg, syntax:[drug]]).
lexEntry(noun, [symbol:drug, num:pl, syntax:[drugs]]).
lexEntry(noun, [symbol:episode, num:sg, syntax:[episode]]).
lexEntry(noun, [symbol:episode, num:pl, syntax:[episodes]]).
lexEntry(noun, [symbol:fdshake, num:sg, syntax:[five, dollar, shake]]).
lexEntry(noun, [symbol:fdshake, num:pl, syntax:[five, dollar, shakes]]).
lexEntry(noun, [symbol:footmassage, num:sg, syntax:[foot, massage]]).
lexEntry(noun, [symbol:footmassage, num:pl, syntax:[foot, massages]]).
lexEntry(noun, [symbol:gimp, num:sg, syntax:[gimp]]).
lexEntry(noun, [symbol:gimp, num:pl, syntax:[gimps]]).
lexEntry(noun, [symbol:glass, num:sg, syntax:[glass]]).
lexEntry(noun, [symbol:glass, num:pl, syntax:[glasses]]).
lexEntry(noun, [symbol:gun, num:sg, syntax:[gun]]).
lexEntry(noun, [symbol:gun, num:pl, syntax:[guns]]).
lexEntry(noun, [symbol:hammer, num:sg, syntax:[hammer]]).
lexEntry(noun, [symbol:hammer, num:pl, syntax:[hammers]]).
lexEntry(noun, [symbol:hashbar, num:sg, syntax:[hash, bar]]).
lexEntry(noun, [symbol:hashbar, num:pl, syntax:[hash, bars]]).
lexEntry(noun, [symbol:person, num:sg, syntax:[person]]).
lexEntry(noun, [symbol:person, num:pl, syntax:[persons]]).
lexEntry(noun, [symbol:husband, num:sg, syntax:[husband]]).
lexEntry(noun, [symbol:husband, num:pl, syntax:[husbands]]).
lexEntry(noun, [symbol:joke, num:sg, syntax:[joke]]).
lexEntry(noun, [symbol:joke, num:pl, syntax:[jokes]]).
lexEntry(noun, [symbol:man, num:sg, syntax:[man]]).
lexEntry(noun, [symbol:man, num:pl, syntax:[men]]).
lexEntry(noun, [symbol:needle, num:sg, syntax:[needle]]).
lexEntry(noun, [symbol:needle, num:pl, syntax:[needles]]).
lexEntry(noun, [symbol:owner, num:sg, syntax:[owner]]).
lexEntry(noun, [symbol:owner, num:pl, syntax:[owners]]).
lexEntry(noun, [symbol:piercing, num:sg, syntax:[piercing]]).
lexEntry(noun, [symbol:piercing, num:pl, syntax:[piercings]]).
lexEntry(noun, [symbol:plant, num:sg, syntax:[plant]]).
lexEntry(noun, [symbol:plant, num:pl, syntax:[plants]]).
lexEntry(noun, [symbol:qpwc, num:sg, syntax:[quarter, pounder, with, cheese]]).
lexEntry(noun, [symbol:qpwc, num:pl, syntax:[quarter, pounders, with, cheese]]).
lexEntry(noun, [symbol:radio, num:sg, syntax:[radio]]).
lexEntry(noun, [symbol:radio, num:pl, syntax:[radioes]]).
lexEntry(noun, [symbol:restaurant, num:sg, syntax:[restaurant]]).
lexEntry(noun, [symbol:restaurant, num:pl, syntax:[restaurants]]).
lexEntry(noun, [symbol:robber, num:sg, syntax:[robber]]).
lexEntry(noun, [symbol:robber, num:pl, syntax:[robbers]]).
lexEntry(noun, [symbol:suitcase, num:sg, syntax:[suitcase]]).
lexEntry(noun, [symbol:suitcase, num:pl, syntax:[suitcases]]).
lexEntry(noun, [symbol:shotgun, num:sg, syntax:[shotgun]]).
lexEntry(noun, [symbol:shotgun, num:pl, syntax:[shotguns]]).
lexEntry(noun, [symbol:sword, num:sg, syntax:[sword]]).
lexEntry(noun, [symbol:sword, num:pl, syntax:[swords]]).
lexEntry(noun, [symbol:vehicle, num:sg, syntax:[vehicle]]).
lexEntry(noun, [symbol:vehicle, num:pl, syntax:[vehicles]]).
lexEntry(noun, [symbol:weapon, num:sg, syntax:[weapon]]).
lexEntry(noun, [symbol:weapon, num:pl, syntax:[weapons]]).
lexEntry(noun, [symbol:wife, num:sg, syntax:[wife]]).
lexEntry(noun, [symbol:wife, num:pl, syntax:[wifes]]).
lexEntry(noun, [symbol:woman, num:sg, syntax:[woman]]).
lexEntry(noun, [symbol:woman, num:pl, syntax:[women]]).


/*========================================================================
    Proper Names
========================================================================*/

lexEntry(pn, [symbol:butch, syntax:[butch]]).
lexEntry(pn, [symbol:esmarelda, syntax:[esmarelda]]).
lexEntry(pn, [symbol:honey_bunny, syntax:[honey, bunny]]).
lexEntry(pn, [symbol:jimmy, syntax:[jimmy]]).
lexEntry(pn, [symbol:jody, syntax:[jody]]).
lexEntry(pn, [symbol:jules, syntax:[jules]]).
lexEntry(pn, [symbol:lance, syntax:[lance]]).
lexEntry(pn, [symbol:marsellus, syntax:[marsellus]]).
lexEntry(pn, [symbol:marsellus, syntax:[marsellus, wallace]]).
lexEntry(pn, [symbol:marvin, syntax:[marvin]]).
lexEntry(pn, [symbol:mia, syntax:[mia]]).
lexEntry(pn, [symbol:mia, syntax:[mia, wallace]]).
lexEntry(pn, [symbol:pumpkin, syntax:[pumpkin]]).
lexEntry(pn, [symbol:thewolf, syntax:[the, wolf]]).
lexEntry(pn, [symbol:vincent, syntax:[vincent]]).
lexEntry(pn, [symbol:vincent, syntax:[vincent, vega]]).
lexEntry(pn, [symbol:yolanda, syntax:[yolanda]]).


/*========================================================================
    Quantified Noun Phrases
========================================================================*/

lexEntry(qnp, [symbol:person, syntax:[who], mood:int, type:wh]).
lexEntry(qnp, [symbol:thing, syntax:[what], mood:int, type:wh]).


/*========================================================================
    Intransitive Verbs
========================================================================*/

lexEntry(iv, [symbol:collapse, syntax:[collapse], inf:inf, num:sg]).
lexEntry(iv, [symbol:collapse, syntax:[collapses], inf:fin, num:sg]).
lexEntry(iv, [symbol:collapse, syntax:[collapse], inf:fin, num:pl]).

lexEntry(iv, [symbol:dance, syntax:[dance], inf:inf, num:sg]).
lexEntry(iv, [symbol:dance, syntax:[dances], inf:fin, num:sg]).
lexEntry(iv, [symbol:dance, syntax:[dance], inf:fin, num:pl]).

lexEntry(iv, [symbol:die, syntax:[die], inf:inf, num:_]).
lexEntry(iv, [symbol:die, syntax:[dies], inf:fin, num:sg]).
lexEntry(iv, [symbol:die, syntax:[die], inf:fin, num:pl]).

lexEntry(iv, [symbol:growl, syntax:[growl], inf:inf, num:sg]).
lexEntry(iv, [symbol:growl, syntax:[growls], inf:fin, num:sg]).
lexEntry(iv, [symbol:growl, syntax:[growl], inf:fin, num:pl]).

lexEntry(iv, [symbol:playairguitar, syntax:[play, air, guitar], inf:inf, num:sg]).
lexEntry(iv, [symbol:playairguitar, syntax:[plays, air, guitar], inf:fin, num:sg]).
lexEntry(iv, [symbol:playairguitar, syntax:[play, air, guitar], inf:fin, num:pl]).

lexEntry(iv, [symbol:smoke, syntax:[smoke], inf:inf, num:sg]).
lexEntry(iv, [symbol:smoke, syntax:[smokes], inf:fin, num:sg]).
lexEntry(iv, [symbol:smoke, syntax:[smoke], inf:fin, num:pl]).

lexEntry(iv, [symbol:snort, syntax:[snort], inf:inf, num:sg]).
lexEntry(iv, [symbol:snort, syntax:[snorts], inf:fin, num:sg]).
lexEntry(iv, [symbol:snort, syntax:[snort], inf:fin, num:pl]).

lexEntry(iv, [symbol:shriek, syntax:[shriek], inf:inf, num:sg]).
lexEntry(iv, [symbol:shriek, syntax:[shrieks], inf:fin, num:sg]).
lexEntry(iv, [symbol:shriek, syntax:[shriek], inf:fin, num:pl]).

lexEntry(iv, [symbol:walk, syntax:[walk], inf:inf, num:sg]).
lexEntry(iv, [symbol:walk, syntax:[walks], inf:fin, num:sg]).
lexEntry(iv, [symbol:walk, syntax:[walk], inf:fin, num:pl]).


/*========================================================================
    Transitive Verbs
========================================================================*/

lexEntry(tv, [symbol:clean, syntax:[clean], inf:inf, num:sg]).
lexEntry(tv, [symbol:clean, syntax:[cleans], inf:fin, num:sg]).
lexEntry(tv, [symbol:clean, syntax:[clean], inf:fin, num:pl]).

lexEntry(tv, [symbol:drink, syntax:[drink], inf:inf, num:sg]).
lexEntry(tv, [symbol:drink, syntax:[drinks], inf:fin, num:sg]).
lexEntry(tv, [symbol:drink, syntax:[drink], inf:fin, num:pl]).

lexEntry(tv, [symbol:date, syntax:[date], inf:inf, num:sg]).
lexEntry(tv, [symbol:date, syntax:[dates], inf:fin, num:sg]).
lexEntry(tv, [symbol:date, syntax:[date], inf:fin, num:pl]).

lexEntry(tv, [symbol:discard, syntax:[discard], inf:inf, num:sg]).
lexEntry(tv, [symbol:discard, syntax:[discards], inf:fin, num:sg]).
lexEntry(tv, [symbol:discard, syntax:[discard], inf:fin, num:pl]).

lexEntry(tv, [symbol:eat, syntax:[eat], inf:inf, num:sg]).
lexEntry(tv, [symbol:eat, syntax:[eats], inf:fin, num:sg]).
lexEntry(tv, [symbol:eat, syntax:[eat], inf:fin, num:pl]).

lexEntry(tv, [symbol:enjoy, syntax:[enjoy], inf:inf, num:sg]).
lexEntry(tv, [symbol:enjoy, syntax:[enjoys], inf:fin, num:sg]).
lexEntry(tv, [symbol:enjoy, syntax:[enjoy], inf:fin, num:pl]).

lexEntry(tv, [symbol:hate, syntax:[hate], inf:inf, num:sg]).
lexEntry(tv, [symbol:hate, syntax:[hates], inf:fin, num:sg]).
lexEntry(tv, [symbol:hate, syntax:[hate], inf:fin, num:pl]).

lexEntry(tv, [symbol:have, syntax:[have], inf:inf, num:sg]).
lexEntry(tv, [symbol:have, syntax:[has], inf:fin, num:sg]).
lexEntry(tv, [symbol:have, syntax:[have], inf:fin, num:pl]).

lexEntry(tv, [symbol:kill, syntax:[kill], inf:inf, num:sg]).
lexEntry(tv, [symbol:kill, syntax:[kills], inf:fin, num:sg]).
lexEntry(tv, [symbol:kill, syntax:[kill], inf:fin, num:pl]).

lexEntry(tv, [symbol:know, syntax:[know], inf:inf, num:sg]).
lexEntry(tv, [symbol:know, syntax:[knows], inf:fin, num:sg]).
lexEntry(tv, [symbol:know, syntax:[know], inf:fin, num:pl]).

lexEntry(tv, [symbol:like, syntax:[like], inf:inf, num:sg]).
lexEntry(tv, [symbol:like, syntax:[likes], inf:fin, num:sg]).
lexEntry(tv, [symbol:like, syntax:[like], inf:fin, num:pl]).

lexEntry(tv, [symbol:love, syntax:[love], inf:inf, num:sg]).
lexEntry(tv, [symbol:love, syntax:[loves], inf:fin, num:sg]).
lexEntry(tv, [symbol:love, syntax:[love], inf:fin, num:pl]).

lexEntry(tv, [symbol:pickup, syntax:[pick, up], inf:inf, num:sg]).
lexEntry(tv, [symbol:pickup, syntax:[picks, up], inf:fin, num:sg]).
lexEntry(tv, [symbol:pickup, syntax:[pick, up], inf:fin, num:pl]).

lexEntry(tv, [symbol:shoot, syntax:[shot], inf:inf, num:sg]).
lexEntry(tv, [symbol:shoot, syntax:[shot], inf:fin, num:sg]).
lexEntry(tv, [symbol:shoot, syntax:[shoots], inf:fin, num:sg]).
lexEntry(tv, [symbol:shoot, syntax:[shoot], inf:fin, num:pl]).


/*========================================================================
    Copula
========================================================================*/

lexEntry(cop, [pol:pos, syntax:[is], inf:fin, num:sg]).
lexEntry(cop, [pol:neg, syntax:[is, not], inf:fin, num:sg]).
lexEntry(cop, [pol:pos, syntax:[are], inf:fin, num:pl]).
lexEntry(cop, [pol:neg, syntax:[are, not], inf:fin, num:pl]).


/*========================================================================
    Prepositions
========================================================================*/

lexEntry(prep, [symbol:about, syntax:[about]]).
lexEntry(prep, [symbol:in, syntax:[in]]).
lexEntry(prep, [symbol:of, syntax:[of]]).
lexEntry(prep, [symbol:with, syntax:[with]]).


/*========================================================================
    Adjectives
========================================================================*/

lexEntry(adj, [symbol:big, syntax:[big]]).
lexEntry(adj, [symbol:blue, syntax:[blue]]).
lexEntry(adj, [symbol:female, syntax:[female]]).
lexEntry(adj, [symbol:happy, syntax:[happy]]).
lexEntry(adj, [symbol:kahuna, syntax:[kahuna]]).
lexEntry(adj, [symbol:male, syntax:[male]]).
lexEntry(adj, [symbol:married, syntax:[married]]).
lexEntry(adj, [symbol:red, syntax:[red]]).
lexEntry(adj, [symbol:sad, syntax:[sad]]).
lexEntry(adj, [symbol:small, syntax:[small]]).
lexEntry(adj, [symbol:tall, syntax:[tall]]).


/*========================================================================
    Adverbs
========================================================================*/

lexEntry(adv, [symbol:quick, syntax:[quickly]]).
lexEntry(adv, [symbol:slow, syntax:[slowly]]).


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
    Pronouns (third person)
========================================================================*/

lexEntry(pro, [symbol:male,  ref:no, syntax:[he]]).
lexEntry(pro, [symbol:male,  ref:no, syntax:[him]]).
lexEntry(pro, [symbol:male,  ref:yes, syntax:[himself]]).
lexEntry(pro, [symbol:female, ref:no, syntax:[she]]).
lexEntry(pro, [symbol:female, ref:yes, syntax:[herself]]).
lexEntry(pro, [symbol:female, ref:no, syntax:[her]]).
lexEntry(pro, [symbol:neuter, ref:no, syntax:[it]]).
lexEntry(pro, [symbol:neuter, ref:yes, syntax:[itself]]).
