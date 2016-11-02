:- module(dcg_tree_expansion, [
              op(1200, xfx, (--->))
          ]).
:- op(1200, xfx, (--->)).

to_yprime((Y, Ys), (Yprime, Yprimes), Ns2) :-
    !,
    to_yprime(Y, Yprime, N),
    to_yprime(Ys, Yprimes, Ns),
    append(N, Ns, Ns2).
to_yprime((Y; Ys), (Yprime; Yprimes), N) :-
    !,
    to_yprime(Y, Yprime, N),
    to_yprime(Ys, Yprimes, _),
    write(' !!! using ; in DCG\' is discouraged').
to_yprime(L, L, [L]) :-
    L = [],
    !.
to_yprime(L, L, [L]) :-
    L = [_|_],
    !.
to_yprime(L, L, []) :-
    L =.. ['{}' | _],
    !.
to_yprime(Y, Yprime, [N]) :-
    callable(Y),
    !,
    Y =.. [Functor | Arg],
    Yprime =.. [Functor, N | Arg].
to_yprime(Y, Y, nil).
user:term_expansion(X ---> Y, (Head --> Yprime)) :-
    X =.. [Functor | Arg],
    to_yprime(Y, Yprime, N),
    Capture =.. [Functor | N],
    Head =.. [Functor, Capture | Arg].
