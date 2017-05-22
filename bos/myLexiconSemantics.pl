/*************************************************************************

     File: semLexLambdaDRT.pl
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

:- module(myLexiconSemantics, [semLex/2]).

:- use_module(types, [addTypeAttribute/2]).

semLex(det, M) :-
    M = [type:uni,
         num:sg,
         sem:lam(U, lam(V, drs([], [imp(merge(drs([variable(X, Type, decl)], []), app(U, X)), app(V, X))]))),
         vType:Type].

%TODO: If the NP is qualified further, does indef really translate to existential?
% E.g. "An animal that dies is not a building" => Every animal that dies is not a building
semLex(det, M) :-
    M = [type:indef,
	       num:sg,
         sem:lam(U, lam(V, merge(merge(drs([variable(X, Type, decl)], []), app(U, X)), app(V, X)))),
         vType:Type].

semLex(det, M) :-
    M = [type:neg,
	       num:sg,
         sem:lam(U, lam(V, drs([], [not(merge(merge(drs([variable(X, Type, decl)], []), app(U, X)), app(V, X)))]))),
         vType:Type].

semLex(number, M) :-
    M = [number:Num,
         sem:lam(U, lam(V, merge(app(U, Num), app(V, Num)))),
         vType:_Type].

semLex(pn, M) :-
    M = [symbol:Sym,
         sem:lam(P, app(P, Sym))].

semLex(noun, M) :-
    M = [symbol:_Sym,
         sem:lam(_X, drs([], []))].

%% semLex(iv, M) :-
%%     M = [symbol:Sym,
%%          sem:lam(N, app(N, lam(X, drs([], [pred(Sym, X)]))))].

semLex(tv, M) :-
    M = [symbol:Sym,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [rel(Sym, X, Y)])))))))].

semLex(ivpp, M) :-
    M = [symbol:Sym,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [rel(Sym, X, Y)])))))))].

semLex(cop, M) :-
    M = [pol:pos,
         type:np,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [eq(Y, X)]))))))),
         symbol:_];
    M = [pol:neg,
         type:np,
         sem:lam(N1, lam(N2, app(N2, lam(X, drs([], [not(app(N1, lam(Y, drs([], [eq(Y, X)]))))]))))),
         symbol:_];
    M = [pol:pos,
         type:adj,
         sem:lam(Adj, lam(N, app(N, lam(X, app(app(Adj, lam(_, drs([], []))), X))))),
         symbol:_];
    M = [pol:neg,
         type:adj,
         sem:lam(Adj, lam(N, app(N, lam(X, drs([], [not(app(app(Adj, lam(_, drs([], []))), X))]))))),
         symbol:_];
    M = [pol:neg,
         type:tv,
         sem:lam(N1, lam(N2, app(N2, lam(X, drs([], [not(app(N1, lam(Y, drs([], [rel(Sym, X, Y)]))))]))))),
         symbol:Sym];
    M = [pol:pos,
         type:tv,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [rel(Sym, X, Y)]))))))),
         symbol:Sym].

semLex(relpro, M) :-
    M = [sem:lam(P, lam(Q, lam(X, merge(app(Q, X), app(P, lam(R, app(R, X)))))))].

semLex(prep, M) :-
    M = [symbol:Sym,
         type:n,
         sem:lam(K, lam(P, lam(Y, merge(app(P, Y), app(K, lam(X, drs([], [rel(Sym, Y, X)])))))))].
    %% M = [symbol:Sym,
    %%      type:vp,
    %%      sem:lam(K, lam(V, lam(N, lam(E, app(app(V, N), lam(X, merge(app(K, lam(Y, drs([], [rel(Sym, X, Y)]))), app(E, X))))))))].

%% semLex(adj, M) :-
%%     M = [symbol:Sym,
%%          sem:lam(P, lam(X, merge(drs([], [pred(Sym, X)]), app(P, X))))].

%% semLex(adv, M) :-
%%     M = [symbol:Sym,
%%          sem:lam(V, lam(N, lam(E, app(app(V, N), lam(X, merge(drs([], [pred(Sym, X)]), app(E, X)))))))].

semLex(av, M) :-
    M = [pol:neg,
         %% sem:lam(P, lam(N, app(N, lam(X, drs([], [not(app(P, lam(Y, app(Y, X))))])))))];
          sem:lam(P, lam(N, app(N, lam(X, app(P, lam(Y, drs([], [not(app(Y, X))])))))))];
    M = [pol:pos,
         sem:lam(P, P)].

% We use distributive reading
% Mia and Vincent do love a building. Distributive vs collective reading -> Different building or the same!!!
semLex(coord, M) :-
    M = [type:conj,
         sem:lam(X, lam(Y, lam(P, merge(app(X, P), app(Y, P)))))];
    M = [type:disj,
         sem:lam(X, lam(Y, lam(P, drs([], [or(app(X, P), app(Y, P))]))))];
    M = [type:neg,
        sem:lam(X, lam(Y, lam(P, merge(drs([], [not(app(X, P))]), drs([], [not(app(Y, P))])))))].

%% semLex(qnp,M) :-
%%     M = [type:wh,
%%          symbol:_Sym,
%%          sem:lam(Q, merge(drs([variable(X, _Type, int)], []), app(Q,X)))].


semLex(comp, M) :-
    M = [type:lower,
         sem:lam(N1, lam(N2, lam(V, app(N1, lam(Y, app(N2, lam(Z, merge(drs([variable(X, Type, decl)], [eq(X, Z-Y)]), app(V, X))))))))),
         vType:Type];
    M = [type:higher,
         sem:lam(N1, lam(N2, lam(V, app(N1, lam(Y, app(N2, lam(Z, merge(drs([variable(X, Type, decl)], [eq(X, Z+Y)]), app(V, X))))))))),
         vType:Type].

semLex(some, M) :-
    addTypeAttribute(Type1, derivedCountable(Type)),
    M = [sem:lam(N, merge(drs([variable(X, Type1, decl)], [eq(X > 0)]), app(N, X))),
         vType:Type].
