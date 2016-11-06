:- module(dcg_macro, [
              op(1200, xfx, (--->)),
              op(600, xfy, (::))
          ]).
:- op(1200, xfx, (--->)).
:- op(600, xfy, (::)).

% The :: operators
% optional
to_yprime(optional :: Y, Yprimes, Ns) :-
    to_yprime((Y ; []), Yprimes, Ns).
% option
to_yprime(option::[], [], []).
to_yprime(option::[H | T], Yprimes, Ns) :-
    H = [_|_],
    !,
    to_yprime((H ; option::T), Yprimes, Ns).
to_yprime(option::[H | T], Yprimes, Ns) :-
    to_yprime(([H] ; option::T), Yprimes, Ns).

to_yprime((Y, Ys), Yprimes3, Ns3) :-
    !,
    to_yprime(Y, Yprimes1, Ns1),
    to_yprime(Ys, Yprimes2, Ns2),
    combine(Yprimes1, Yprimes2, Yprimes3, and),
    combine(Ns1, Ns2, Ns3, list).
to_yprime((Y; Ys), Yprimes2, Ns3) :-
    !,
    to_yprime(Y, Yprime, Ns1),
    to_yprime(Ys, Yprimes, Ns2),
    append(Yprime, Yprimes, Yprimes2),
    append(Ns1, Ns2, Ns3).
to_yprime(L, [L], [L]) :-
    L = [],
    !.
to_yprime(L, [L], [L]) :-
    L = [_|_],
    !.
to_yprime(L, [L], [[]]) :-
    L =.. ['{}' | _],
    !.
to_yprime(Y, [Yprime], [[N]]) :-
    callable(Y),
    !,
    Y =.. [Functor | Arg],
    Yprime =.. [Functor, N | Arg].
to_yprime(Y, [Y], nil).

user:term_expansion(X ---> Y, Rules) :-
    X =.. [Functor | Arg],
    to_yprime(Y, Yprimes, Ns),
    maplist(to_term(Functor, Arg), Yprimes, Ns, Terms),
    % expand DCG rules
    maplist(expand_term, Terms, Rules).

to_term(Functor, Arg, Yprime, N, (Head --> Yprime)) :-
    Capture =.. [Functor | N],
    Head =.. [Functor, Capture | Arg].

combine([], _, [], _) :- !.
combine(_, [], [], _) :- !.
combine([X | Xs], Ys, Zs, F) :-
    combine1(X, Ys, Zs1, F),
    combine(Xs, Ys, Zs2, F),
    append(Zs1, Zs2, Zs).

combine1(_, [], [], _).
combine1(X, [Y | Ys], [Z | Zs], F) :-
    combine2(X, Y, Z, F),
    combine1(X, Ys, Zs, F).

combine2(X, Y, (X, Y), and).
combine2(X, Y, Z, list) :-
    append(X, Y, Z).
