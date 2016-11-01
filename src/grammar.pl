:- op(1200, xfx, (->>)).
%phrase(s(X), [the, age, of, an, employee, is, less, than, 3]).

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
term_expansion(X ->> Y, (Head --> Yprime)) :-
    X =.. [Functor | Arg],
    to_yprime(Y, Yprime, N),
    Capture =.. [Functor | N],
    Head =.. [Functor, Capture | Arg].

s ->> [if], cond, [then], expr.
s ->> expr, [if], cond.
s ->> cond.

expr ->> property_phrase, [of], entity_phrase, [equals], amount.
expr ->> entity_phrase, verb, object.

cond ->> cond1.
cond ->> cond1, [and], cond.
cond ->> cond1, [or], cond.

cond1 ->> property_phrase, [of], entity_phrase, [is], comparison.
cond1 ->> entity_phrase, verb, object.
cond1 ->> entity_phrase, [does, not], verb, object.
cond1 ->> entity_phrase, [has, a, number, of], property_phrase, [equal, to], amount.

property_phrase ->> ([X], {member(X, [the, a, an])} ; []), property.

entity_phrase ->> [X], {member(X, [each, every, a, an, the])}, entity.
entity_phrase ->> [he].

comparison ->> comparison_function, amount.
comparison ->> comparison_function, amount, [or], comparison.
comparison ->> comparison_function, amount, [and], comparison.

comparison_function ->> [less, than].
comparison_function ->> [at, least].
comparison_function ->> [more, than].
comparison_function ->> [greather, than].
comparison_function ->> [between], amount, [and].

amount ->> amount1.
amount ->> amount1, [plus, the, sum, of], property_phrase, entity_phrase, verb.

amount1 ->> [X], {number(X)}.
amount1 ->> [X], {atom(X), atom_number(X, _)}.


property ->> [years, of, service].
property ->> [age].
property ->> [extra, days].
property ->> [vacation, days].
entity ->> [employee].
verb ->> [receives].
verb ->> [receive].

object ->> amount, property.

%optional(X) ->> X.
%optional(_) ->> [].

parse(S, X, Z) :-
    string_lower(S, SLower),
    atomics_to_string(Z, ' ', SLower),
    phrase(s(X), Z).

all_parses(S, Zs) :-
    findall(Z, parse(S, Z), Zs).

parse_vakantiedagen(Zs) :-
    maplist(all_parses, 
            [
              "If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days",
              "If the years of service of an employee is at least 30 then he receives 5 extra days",
              "If years of service of an employee is at least 30 then he receives 3 extra days",
              "If the years of service of an employee is between 15 and 30 and he does not receive 5 extra days, then he receives 2 extra days",
              "If the age of an employee is at least 45 and he does not receive 5 extra days then he receives 2 extra days",
              "Each employee has a number of vacation days equal to 22 plus the sum of the extra days he receives"
            ], Zs).

parse_tree(S) :-
    string_lower(S, SLower),
    atomics_to_string(As, ' ', SLower),
    phrase(s(T), As),
    display_tree(vertical, T).

display_tree(Direction, Tree) :-
    sformat(A, 'Display tree ~w', [Direction]),
    new(D, window(A)),
    send(D, size, size(350,200)),
    draw_tree(Direction, Tree, T),
    send(T, direction, Direction),
    send(D, display, T),
    send(D, open).

draw_tree(_, Node, T) :-
    atom(Node),
    !,
    draw_node(T, Node).
draw_tree(_, L, T) :-
    L = [_ | _],
    !,
    atomics_to_string(L, ' ', S),
    draw_node(S, T).
draw_tree(Direction, Tree, T) :-
    Tree =.. [Node | Children],
    new(T, tree(text(Node))),
    send(T, neighbour_gap, 10),
    send(T, direction, Direction),
    maplist(draw_tree(Direction), Children, SubTrees),
    send_list(T, son, SubTrees).

draw_node(S, T) :-
    new(T, node(text(S))).

%?- parse_tree("If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days").
