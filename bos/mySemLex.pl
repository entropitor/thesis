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

:- module(mySemLex, [semLex/2]).

semLex(det, M) :-
    M = [type:uni,
         num:sg,
         sem:lam(U, lam(V, drs([], [imp(merge(drs([var(X, Type)], []), app(U, X)), app(V, X))]))),
         vType:Type].

%TODO: If the NP is qualified further, does indef really translate to existential?
% E.g. "An animal that dies is not a building" => Every animal that dies is not a building
semLex(det, M) :-
    M = [type:indef,
	       num:sg,
         sem:lam(U, lam(V, merge(merge(drs([var(X, Type)], []), app(U, X)), app(V, X)))),
         vType:Type].

semLex(det, M) :-
    M = [type:neg,
	       num:sg,
         sem:lam(U, lam(V, drs([], [not(merge(merge(drs([var(X, Type)], []), app(U, X)), app(V, X)))]))),
         vType:Type].

semLex(pn, M) :-
    M = [symbol:Sym,
         sem:lam(P, merge(drs([], []), app(P, Sym)))].

semLex(noun, M) :-
    M = [symbol:Sym,
         sem:lam(X, drs([], [pred(Sym, X)]))].

semLex(iv, M) :-
    M = [symbol:Sym,
         sem:lam(N, app(N, lam(X, drs([], [pred(Sym, X)]))))].

semLex(tv, M) :-
    M = [symbol:Sym,
         _,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [rel(Sym, X, Y)])))))))].

semLex(ivpp, M) :-
    M = [symbol:Sym,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [rel(Sym, X, Y)])))))))].

semLex(cop, M) :-
    M = [pol:pos,
         type:np,
         sem:lam(N1, lam(N2, app(N2, lam(X, app(N1, lam(Y, drs([], [eq(Y, X)])))))))];
    M = [pol:neg,
         type:np,
         sem:lam(N1, lam(N2, app(N2, lam(X, drs([], [not(app(N1, lam(Y, drs([], [eq(Y, X)]))))])))))];
    M = [pol:pos,
         type:adj,
         sem:lam(Adj, lam(N, app(N, lam(X, app(app(Adj, lam(_, drs([], []))), X)))))];
    M = [pol:neg,
         type:adj,
         sem:lam(Adj, lam(N, app(N, lam(X, drs([], [not(app(app(Adj, lam(_, drs([], []))), X))])))))].

semLex(relpro, M) :-
    M = [sem:lam(P, lam(Q, lam(X, merge(app(P, lam(R, app(R, X))), app(Q, X)))))].

semLex(prep, M) :-
    M = [symbol:Sym,
         type:n,
         sem:lam(K, lam(P, lam(Y, merge(app(K, lam(X, drs([], [rel(Sym, Y, X)]))), app(P, Y)))))].
    %% M = [symbol:Sym,
    %%      type:vp,
    %%      sem:lam(K, lam(V, lam(N, lam(E, app(app(V, N), lam(X, merge(app(K, lam(Y, drs([], [rel(Sym, X, Y)]))), app(E, X))))))))].

semLex(adj, M) :-
    M = [symbol:Sym,
         sem:lam(P, lam(X, merge(drs([], [pred(Sym, X)]), app(P, X))))].

%% semLex(adv, M) :-
%%     M = [symbol:Sym,
%%          sem:lam(V, lam(N, lam(E, app(app(V, N), lam(X, merge(drs([], [pred(Sym, X)]), app(E, X)))))))].

semLex(av, M) :-
    M = [pol:neg,
         sem:lam(P, lam(X, drs([], [not(app(P, X))])))];
    M = [pol:pos,
         sem:lam(P, lam(X, app(P, X)))].

% TODO: Mia and Vincent do love a building. Distributive vs collective reading -> Different building or the same!!!
semLex(coord, M) :-
    M = [type:conj,
         sem:lam(X, lam(Y, lam(P, merge(app(X, P), app(Y, P)))))];
    M = [type:disj,
         sem:lam(X, lam(Y, lam(P, drs([], [or(app(X, P), app(Y, P))]))))].
