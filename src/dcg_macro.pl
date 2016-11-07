:- module(dcg_macro, [
              op(1200, xfx, (--->)),
              op(600, xfy, (::)),
              expand/2
          ]).
:- op(1200, xfx, (--->)).
:- op(600, xfy, (::)).

:- discontiguous expand/2.
to(Term, X, Term::X).
to(Term, Extra, X, Term::Extra-X).

expand(verb::X, verb(X) ---> [X]).
expand(verb_list::X, Rules) :-
    maplist(to(verb), X, Rules).

expand(named_entity::X, entity(literal(X, Name), Name) ---> [X]).
expand(named_entity_list::X, Rules) :-
    maplist(to(named_entity), X, Rules).

expand(entity::Type, entity(var(Name, Type), Name) ---> [Type]).
expand(entity_list::X, Rules) :-
    maplist(to(entity), X, Rules).

expand(property::X, property(X) ---> [X]).
expand(property_list::X, Rules) :-
    maplist(to(property), X, Rules).

expand(property_value::Property-X, property_value(Property, literal(X)) ---> [X]).
expand(property_value_list::Property-X, Rules) :-
    maplist(to(property_value, Property), X, Rules).

% The :: operators
% optional: the next goal is optional
to_yprime(optional::Y, Yprimes, BodyNames) :-
    to_yprime((Y ; []), Yprimes, BodyNames).
% one_of: choose a literal from the list, multi-words are supported as nested list
to_yprime(one_of::[], [], []).
to_yprime(one_of::[H | T], Yprimes, BodyNames) :-
    H = [_|_],
    !,
    to_yprime((H ; one_of::T), Yprimes, BodyNames).
to_yprime(one_of::[H | T], Yprimes, BodyNames) :-
    to_yprime(([H] ; one_of::T), Yprimes, BodyNames).

to_yprime((Y, Ys), Yprimes3, BodyNames3) :-
    !,
    to_yprime(Y, Yprimes1, BodyNames1),
    to_yprime(Ys, Yprimes2, BodyNames2),
    combine(Yprimes1, Yprimes2, Yprimes3, and),
    combine(BodyNames1, BodyNames2, BodyNames3, list).
to_yprime((Y; Ys), Yprimes2, BodyNames3) :-
    !,
    to_yprime(Y, Yprime, BodyNames1),
    to_yprime(Ys, Yprimes, BodyNames2),
    append(Yprime, Yprimes, Yprimes2),
    append(BodyNames1, BodyNames2, BodyNames3).
to_yprime(L, [L], [L]) :-
    L = [],
    !.
to_yprime(L, [Lprime], [Lprime]) :-
    L = [_|_],
    flatten(L, Lprime),
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

to_term(Functor, Arg, Yprime, BodyNames, (Head --> Yprime)) :-
    Capture =.. [Functor | BodyNames],
    Head =.. [Functor, Capture | Arg].

expand(X ---> Y, Rules) :-
    X =.. [Functor | Arg],
    to_yprime(Y, Yprimes, BodyNames),
    maplist(to_term(Functor, Arg), Yprimes, BodyNames, Rules).
user:term_expansion(In, Out) :-
    expand(In, Out1),
    (
        Out1 = [_|_]
     ->
        maplist(expand_term, Out1, Out)
     ;
        expand_term(Out1, Out)
    ).

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
