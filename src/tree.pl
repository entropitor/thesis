:- module(tree, [parse_tree/2, display_tree/2]).
:- use_module(parser).

parse_tree(S, Symbol) :-
    input_atoms(S, Atoms),
    Symbol =.. [Functor | Args],
    Symbol2 =.. [Functor, T | Args],
    phrase(Symbol2, Atoms),
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
    draw_node(Node, T).
draw_tree(_, L, T) :-
    L = [_ | _],
    !,
    atomics_to_string(L, ' ', S),
    draw_node(S, T).
draw_tree(Direction, Tree, T) :-
    Tree =.. [Node | Children],
    new(T, tree(text(Node))),
    send(T, neighbour_gap, 10),
    send(T, level_gap, 30),
    send(T, direction, Direction),
    maplist(draw_tree(Direction), Children, SubTrees),
    send_list(T, son, SubTrees).

draw_node(S, T) :-
    new(T, node(text(S))).
