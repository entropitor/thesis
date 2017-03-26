:- module(printTable, [printTable/1]).

printTable(Rows) :-
    maplist(maplist(atom_length), Rows, Lengths),
    maxPerColumn(Lengths, MaxLengths),
    printHeader(MaxLengths),
    maplist(printRow(MaxLengths), Rows),
    printFooter(MaxLengths),
    nl.

maxPerColumn(Rows, Maxs) :-
    Rows = [Row | _],
    length(Row, L),
    length(Zeros, L),
    include(=(0), Zeros, Zeros),
    foldl(maplist(max), Rows, Zeros, Maxs).

maxList(List, Max) :-
    foldl(max, List, 0, Max).
max(X, Y, Z) :-
    Z is max(X, Y).

printRow(Widths, List) :-
    write('│ '),
    maplist(printCell, Widths, List),
    nl.
printCell(Width, Atom) :-
    format('~|~t~p~t~*+ │ ', [Atom, Width]).

printHeader(Widths) :-
    write('┌─'),
    printHeaderCells(Widths),
    nl.
printHeaderCells([Width]) :-
    !,
    format('~|~`─t~*+─┐', [Width]).
printHeaderCells([Width | Widths]) :-
    format('~|~`─t~*+─┬─', [Width]),
    printHeaderCells(Widths).

printFooter(Widths) :-
    write('└─'),
    printFooterCells(Widths),
    nl.
printFooterCells([Width]) :-
    !,
    format('~|~`─t~*+─┘', [Width]).
printFooterCells([Width | Widths]) :-
    format('~|~`─t~*+─┴─', [Width]),
    printFooterCells(Widths).
