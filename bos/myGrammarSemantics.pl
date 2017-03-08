/*************************************************************************

     File: semRulesDRT.pl
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

:- module(myGrammarSemantics, [combine/2]).

/*========================================================================
    Semantic Rules
========================================================================*/

combine(t:Sem, [s:Sem]).
combine(t:merge(S, T), [s:S, t:T]).
combine(t:Sem, [q:Sem]).

combine(s:app(B, A), [np:A, vp:B]).
combine(s:app(app(A, B), lam(_, drs([], []))), [det:A, n:B]).
combine(s:app(A, B), [s:A, s:B]).
combine(s:lam(B, drs([], [imp(S, B)])), [if:S]).
combine(s:lam(B, drs([], [or(S, B)])), [either:S]).
combine(s:S, [then:S]).
combine(s:S, [or:S]).
combine(s:drs([], [not(S)]), [not:S]).
combine(s:S, [question:S]).

combine(sinv:app(app(A, C), B), [av:A, np:B, vp:C]).

combine(q:app(B, A), [whnp:A, vp:B]).
combine(q:A, [sinv:A]).

combine(np:app(app(B, A), C), [np:A, coord:B, np:C]).
combine(np:app(A, B), [det:A, n:B]).
combine(np:A, [pn:A]).
combine(np:A, [qnp:A]).

combine(whnp:app(A, B), [det:A, n:B]).
combine(whnp:A, [qnp:A]).

combine(n:app(app(B, A), C), [n:A, coord:B, n:C]).
combine(n:app(A, B), [adj:A, n:B]).
combine(n:A, [noun:A]).
combine(n:app(B, A), [noun:A, nmod:B]).

combine(nmod:A, [pp:A]).
combine(nmod:A, [rc:A]).
combine(nmod:lam(P, app(A, app(B, P))), [pp:A, nmod:B]).

combine(vp:app(app(B, A), C), [vp:A, coord:B, vp:C]).
combine(vp:app(A, B), [av:A, vp:B]).
combine(vp:app(A, B), [cop:A, np:B]).
combine(vp:app(A, B), [cop:A, adj:B]).
combine(vp:A, [iv:A]).
combine(vp:app(PP, A), [iv:A, adv:PP]).
combine(vp:app(A, B), [tv:A, np:B]).
combine(vp:lam(Obj, app(app(A, Obj), B)), [tv:A, npSubj:B]).

combine(pp:app(A, B), [prep:A, np:B]).

combine(rc:app(A, B), [relpro:A, vp:B]).
