/*************************************************************************

     File: englishGrammar.pl
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

:- module(myGrammar, [t/3]).

:- use_module(comsemPredicates, [memberList/2]).
:- use_module(problemLexiconRules, [lexEntry/2]).
:- use_module(myLexiconSemantics, [semLex/2]).
:- use_module(myGrammarSemantics, [combine/2]).

/*========================================================================
    Texts
========================================================================*/

t([sem:T])-->
    { memberList(C, [yes, no, question]) },
    s([coord:C, sem:S]),
    { combine(t:T, [s:S]) }.

%% t([sem:T])-->
%%     { memberList(C, [yes, no, question]) },
%%     s([coord:C, sem:S1]),
%%     t([sem:S2]),
%%     { combine(t:T, [s:S1, t:S2]) }.

/*========================================================================
    Sentences
========================================================================*/

s([coord:no, sem:Sem])-->
    np([coord:_, num:Num, gap:[], ref:no, sem:NP, vType:SubjType]),
    vp([coord:_, inf:fin, num:Num, gap:[], sem:VP, vType:SubjType]),
    { combine(s:Sem, [np:NP, vp:VP]) }.

s([coord:no, sem:Sem])-->
    [there, is],
    det([mood:decl, type:indef, num:sg, sem:Det, vType:Type]),
    n([coord:_, num:sg, sem:N, vType:Type]),
    { combine(s:Sem, [det:Det, n:N]) }.

s([coord:yes, sem:Sem])-->
    s([coord:ant, sem:S1]),
    s([coord:con, sem:S2]),
    { combine(s:Sem, [s:S1, s:S2]) }.

s([coord:yes, sem:Sem])-->
    s([coord:either, sem:S1]),
    s([coord:or, sem:S2]),
    { combine(s:Sem, [s:S1, s:S2]) }.

s([coord:ant, sem:Sem])-->
    [if],
    s([coord:no, sem:S]),
    { combine(s:Sem, [if:S]) }.

s([coord:either, sem:Sem])-->
    [either],
    s([coord:no, sem:S]),
    { combine(s:Sem, [either:S]) }.

s([coord:con, sem:Sem])-->
    [then],
    s([coord:no, sem:S]),
    { combine(s:Sem, [then:S]) }.

s([coord:con, sem:Sem])-->
    s([coord:no, sem:S]),
    { combine(s:Sem, [then:S]) }.

s([coord:or, sem:Sem])-->
    [or],
    s([coord:no, sem:S]),
    { combine(s:Sem, [or:S]) }.

s([coord:question, sem:Sem])-->
    q([sem:Q]),
    { combine(s:Sem, [question:Q]) }.

%s([coord:C, sem:Sem])-->
%    [it, is, not, the, case, that],
%    s([coord:C, sem:S]),
%    { combine(s:Sem, [not:S]) }.

sinv([gap:G, sem:S])-->
    av([inf:fin, num:Num, sem:Sem]),
    np([coord:_, num:Num, gap:[], ref:no, sem:NP, vType:SubjType]),
    vp([coord:_, inf:inf, num:Num, gap:G, sem:VP, vType:SubjType]),
    { combine(sinv:S, [av:Sem, np:NP, vp:VP]) }.

/*========================================================================
    Questions
========================================================================*/

q([sem:Sem])-->
    whnp([num:Num, sem:NP, vType:Type]),
    vp([coord:_, inf:fin, num:Num, gap:[], sem:VP, vType:Type]),
    { combine(q:Sem, [whnp:NP, vp:VP]) }.

q([sem:Sem])-->
    whnp([num:_, sem:NP, vType:Type]),
    sinv([gap:[np:NP-Type], sem:S]),
    { combine(q:Sem, [sinv:S]) }.


/*========================================================================
    Noun Phrases
========================================================================*/

np([coord:no, num:sg, gap:[np:NP-Type], ref:no, sem:NP, vType:Type])--> [].

np([coord:yes, num:pl, gap:[], ref:Ref, sem:NP, vType:Type])-->
    np([coord:no, num:sg, gap:[], ref:Ref, sem:NP1, vType:Type]),
    coord([type:conj, sem:C]),
    np([coord:_, num:_, gap:[], ref:Ref, sem:NP2, vType:Type]),
    { combine(np:NP, [np:NP1, coord:C, np:NP2]) }.

np([coord:yes, num:sg, gap:[], ref:Ref, sem:NP, vType:Type])-->
    np([coord:no, num:sg, gap:[], ref:Ref, sem:NP1, vType:Type]),
    coord([type:disj, sem:C]),
    np([coord:_, num:sg, gap:[], ref:Ref, sem:NP2, vType:Type]),
    { combine(np:NP, [np:NP1, coord:C, np:NP2]) }.

np([coord:no, num:Num, gap:[], ref:no, sem:NP, vType:Type])-->
    det([mood:decl, type:_, num:Num, sem:Det, vType:Type]),
    n([coord:_, num:Num, sem:N, vType:Type]),
    { combine(np:NP, [det:Det, n:N]) }.

%TODO: fix plural vs singular
np([coord:no, num:Num, gap:[], ref:no, sem:NP, vType:countable(Type)])-->
    number([sem:Number, vType:Type]),
    n([coord:_, num:Num, sem:N, vType:countable(Type)]),
    { combine(np:NP, [number:Number, n:N]) }.

np([coord:yes, num:Num, gap:[], ref:no, sem:NP, vType:countable(Type)])-->
    np([coord:no, num:Num, gap:[], ref:no, sem:NP1, vType:countable(Type)]),
    comp([sem:Comp, vType:Type]),
    np([coord:no, num:_, gap:[], ref:no, sem:NP2, vType:countable(Type)]),
    { combine(np:NP, [np:NP1, comp:Comp, np:NP2]) }.

np([coord:no, num:sg, gap:[], ref:no, sem:NP, vType:Type])-->
    pn([sem:PN, vType:Type]),
    { combine(np:NP, [pn:PN]) }.

%np([coord:no, num:sg, gap:[], ref:Ref, sem:NP])-->
%    pro([ref:Ref, sem:PN]),
%    { combine(np:NP, [pn:PN]) }.

%np([coord:no, num:sg, gap:[], ref:no, sem:NP])-->
%    qnp([mood:decl, sem:QNP]),
%    { combine(np:NP, [qnp:QNP]) }.


/*========================================================================
    WH Noun Phrases
========================================================================*/

whnp([num:sg, sem:NP, vType:Type])-->
   qnp([mood:int, sem:QNP, vType:Type]),
   { combine(whnp:NP, [qnp:QNP]) }.

%whnp([num:sg, sem:NP])-->
%    det([mood:int, type:_, num:_, sem:Det]),
%    n([coord:_, num:_, sem:N]),
%    { combine(whnp:NP, [det:Det, n:N]) }.


/*========================================================================
    Nouns
========================================================================*/

n([coord:yes, num:Num, sem:N, vType:Type])-->
    n([coord:no, num:Num, sem:N1, vType:Type]),
    coord([type:_, sem:C]),
    n([coord:_, num:Num, sem:N2, vType:Type]),
    { combine(n:N, [n:N1, coord:C, n:N2]) }.

n([coord:C, num:Num, sem:Sem, vType:Type])-->
    adj([sem:A, vType:adj(Type)]),
    n([coord:C, num:Num, sem:N, vType:Type]),
    { combine(n:Sem, [adj:A, n:N]) }.

n([coord:no, num:Num, sem:N, vType:Type])-->
    noun([num:Num, sem:Noun, vType:Type]),
    { combine(n:N, [noun:Noun]) }.

%% n([coord:no, num:_, sem:N, vType:Type])-->
%%     cn([sem:CN, vType:Type]),
%%     { combine(n:N, [cn:CN]) }.

n([coord:no, num:Num, sem:Sem, vType:Type])-->
    noun([num:Num, sem:N, vType:Type]),
    nmod([num:Num, sem:PP, vType:Type]),
    { combine(n:Sem, [noun:N, nmod:PP]) }.

nmod([num:_, sem:N, vType:Type])-->
    pp([type:n, sem:PP, vType:Type]),
    { combine(nmod:N, [pp:PP]) }.

nmod([num:Num, sem:N, vType:Type])-->
    rc([num:Num, sem:RC, vType:Type]),
    { combine(nmod:N, [rc:RC]) }.

%% nmod([num:Num, sem:Sem, vType:Type])-->
%%     pp([type:n, sem:PP, vType:Type]),
%%     nmod([num:Num, sem:NMod, vType:Type]),
%%     { combine(nmod:Sem, [pp:PP, nmod:NMod]) }.

/*========================================================================
    Verb Phrases
========================================================================*/

vp([coord:yes, inf:Inf, num:Num, gap:[], sem:VP, vType:Type])-->
    vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP1, vType:Type]),
    coord([type:_, sem:C]),
    vp([coord:_, inf:Inf, num:Num, gap:[], sem:VP2, vType:Type]),
    { combine(vp:VP, [vp:VP1, coord:C, vp:VP2]) }.

vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP, vType:Type])-->
    av([inf:Inf, num:Num, sem:Mod]),
    vp([coord:_, inf:inf, num:_, gap:[], sem:V2, vType:Type]),
    { combine(vp:VP, [av:Mod, vp:V2]) }.

%% vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP, vType:Type])-->
%%     cop([type:np, inf:Inf, num:Num, sem:Cop]),
%%     np([coord:_, num:_, gap:[], ref:_, sem:NP, vType:Type]),
%%     { combine(vp:VP, [cop:Cop, np:NP]) }.

vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP, vType:Type])-->
    cop([type:adj, inf:Inf, num:Num, sem:Cop]),
    adj([sem:Adj, vType:adj(Type)]),
    { combine(vp:VP, [cop:Cop, adj:Adj]) }.

vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP, vType:Type])-->
    iv([inf:Inf, num:Num, sem:IV, vType:Type]),
    { combine(vp:VP, [iv:IV]) }.

%vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP])-->
%    iv([inf:Inf, num:Num, sem:IV]),
%    adv([sem:ADV]),
%    { combine(vp:VP, [iv:IV, adv:ADV]) }.

%vp([coord:no, inf:Inf, num:Num, gap:[], sem:VP])-->
%    iv([inf:Inf, num:Num, sem:IV]),
%    pp([type:vp, sem:Adv]),
%    { combine(vp:VP, [iv:IV, adv:Adv]) }.

vp([coord:no, inf:I, num:Num, gap:[], sem:VP, vType:TypeObj])-->
    np([coord:_, num:_, gap:[], ref:Ref, sem:NP, vType:TypeSubj]),
    tv([inf:I, num:Num, ref:Ref, sem:TV, vType:pred(TypeSubj, TypeObj)]),
    { combine(vp:VP, [np:NP, tv:TV]) }.

vp([coord:no, inf:I, num:Num, gap:G, sem:VP, vType:TypeSubj])-->
  tv([inf:I, num:Num, ref:Ref, sem:TV, vType:pred(TypeSubj, TypeObj)]),
  np([coord:_, num:_, gap:G, ref:Ref, sem:NP, vType:TypeObj]),
  { combine(vp:VP, [tv:TV, np:NP]) }.

vp([coord:no, inf:I, num:Num, gap:G, sem:VP, vType:TypeSubj])-->
    ivpp([inf:I, num:Num, pp:PP, sem:IVPP, vType:pred(TypeSubj, TypeObj)]),
    PP,
    np([coord:_, num:_, gap:G, ref:_, sem:NP, vType:TypeObj]),
    { combine(vp:VP, [tv:IVPP, np:NP]) }.

vp([coord:no, inf:I, num:Num, gap:[pp:PP], sem:VP, vType:TypeObj])-->
    np([coord:_, num:_, gap:_, ref:_, sem:NP, vType:TypeSubj]),
    ivpp([inf:I, num:Num, pp:PP, sem:IVPP, vType:pred(TypeSubj, TypeObj)]),
    { combine(vp:VP, [tv:IVPP, npSubj:NP]) }.

/*========================================================================
    Prepositional Phrases
========================================================================*/

pp([type:Type, sem:PP, vType:SubjType])-->
    prep([type:Type, syntax:_, sem:Prep, vType:fun(SubjType, ObjType)]),
    np([coord:_, num:_, gap:[], ref:no, sem:NP, vType:ObjType]),
    { combine(pp:PP, [prep:Prep, np:NP]) }.


/*========================================================================
    Relative Clauses
========================================================================*/

rc([num:Num, sem:RC, vType:Type])-->
    relpro([sem:RP]),
    vp([coord:_, inf:fin, num:Num, gap:[], sem:VP, vType:Type]),
    { combine(rc:RC, [relpro:RP, vp:VP]) }.

rc([num:Num, sem:RC, vType:Type])-->
    prep([type:n, syntax:PP, sem:_, vType:_]),
    relpro([sem:RP]),
    vp([coord:_, inf:fin, num:Num, gap:[pp:PP], sem:VP, vType:Type]),
    { combine(rc:RC, [relpro:RP, vp:VP]) }.

/*========================================================================
    Lexical Rules
========================================================================*/

iv([inf:Inf, num:Num, sem:Sem, vType:Type])-->
    { lexEntry(iv, [symbol:Sym, syntax:Word, inf:Inf, num:Num, vType:Type]) },
    Word,
    { semLex(iv, [symbol:Sym, sem:Sem]) }.

ivpp([inf:Inf, num:Num, pp:PP, sem:Sem, vType:Type])-->
    { lexEntry(ivpp, [symbol:Sym, syntax:Word, pp:PP, inf:Inf, num:Num, vType:Type]) },
    Word,
    { semLex(ivpp, [symbol:Sym, sem:Sem]) }.

tv([inf:Inf, num:Num, ref:Ref, sem:Sem, vType:Type])-->
    { lexEntry(tv, [symbol:Sym, syntax:Word, inf:Inf, num:Num, vType:Type]) },
    Word,
    { semLex(tv, [symbol:Sym, ref:Ref, sem:Sem]) }.

cop([type:Type, inf:Inf, num:Num, sem:Sem])-->
    { lexEntry(cop, [pol:Pol, syntax:Word, inf:Inf, num:Num]) },
    Word,
    { semLex(cop, [pol:Pol, type:Type, sem:Sem]) }.

det([mood:M, type:Type, num:Num, sem:Det, vType:VType])-->
    { lexEntry(det, [syntax:Word, mood:M, num:Num, type:Type]) },
    Word,
    { semLex(det, [type:Type, num:Num, sem:Det, vType:VType]) }.

%% cn([sem:Sem, vType:Type])-->
%%     { lexEntry(cn, [symbol:Sym, syntax:Word, vType:Type]) },
%%     Word,
%%     { semLex(cn, [symbol:Sym, sem:Sem, vType:Type]) }.

number([sem:Sem, vType:Type], [Number|T], T) :-
    integer(Number),
    semLex(number, [number:Number, sem:Sem, vType:Type]).

pn([sem:Sem, vType:Type])-->
    { lexEntry(pn, [symbol:Sym, syntax:Word, vType:Type]) },
    Word,
    { semLex(pn, [symbol:Sym, sem:Sem]) }.

%pro([ref:Ref, sem:Sem])-->
%    { lexEntry(pro, [symbol:Sym, ref:Ref, syntax:Word]) },
%    Word,
%    { semLex(pro, [symbol:Sym, sem:Sem]) }.

relpro([sem:Sem])-->
    { lexEntry(relpro, [syntax:Word]) },
    Word,
    { semLex(relpro, [sem:Sem]) }.

prep([type:Type, syntax:Word, sem:Sem, vType:VType])-->
    { lexEntry(prep, [symbol:Sym, syntax:Word, vType:VType]) },
    Word,
    { semLex(prep, [symbol:Sym, type:Type, sem:Sem]) }.

adj([sem:Sem, vType:Type])-->
    { lexEntry(adj, [symbol:Sym, syntax:Word, vType:Type]) },
    Word,
    { semLex(adj, [symbol:Sym, sem:Sem]) }.

%adv([sem:Sem])-->
%    { lexEntry(adv, [symbol:Sym, syntax:Word]) },
%    Word,
%    { semLex(adv, [symbol:Sym, sem:Sem]) }.

av([inf:Inf, num:Num, sem:Sem])-->
    { lexEntry(av, [syntax:Word, inf:Inf, num:Num, pol:Pol]) },
    Word,
    { semLex(av, [pol:Pol, sem:Sem]) }.

coord([type:Type, sem:Sem])-->
    { lexEntry(coord, [syntax:Word, type:Type]) },
    Word,
    { semLex(coord, [type:Type, sem:Sem]) }.

qnp([mood:M, sem:NP, vType:_VType])-->
    { lexEntry(qnp, [symbol:Symbol, syntax:Word, mood:M, type:Type]) },
    Word,
    { semLex(qnp, [type:Type, symbol:Symbol, sem:NP]) }.

noun([num:Num, sem:Sem, vType:Type])-->
    { lexEntry(noun, [symbol:Sym, num:Num, syntax:Word, vType:Type]) },
    Word,
    { semLex(noun, [symbol:Sym, sem:Sem]) }.

comp([sem:Sem, vType:VType])-->
    { lexEntry(comp, [type:Type, syntax:Word]) },
    Word,
    { semLex(comp, [type:Type, sem:Sem, vType:VType]) }.

