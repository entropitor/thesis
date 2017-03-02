:- module(combineTypes, [combineTypes/2]).

combineTypes([], []) :-
    !.
combineTypes([type(Var, Type) | In], Out) :-
    selectchk(type(Var, Type2), In, RestIn),
    !,
    matchType(Type, Type2),
    combineTypes([type(Var, Type) | RestIn], Out).
combineTypes([type(Var, Type) | In], [type(Var, Type) | Out]) :-
    !,
    combineTypes(In, Out).


%% matchType(Type1, Type2) :-
%%     ground(Type1),
%%     ground(Type2),
%%     Type1 == Type2,
%%     !.
%% matchType(Type1, Type2) :-
%%     nonvar(Type1),
%%     var(Type2),
%%     !,
%%     Type1 = Type2.
%% matchType(Type1, Type2) :-
%%     var(Type1),
%%     nonvar(Type2),
%%     !,
%%     Type1 = Type2.
%% matchType(Type1, Type2) :-
%%     var(Type1),
%%     var(Type2),
%%     !,
%%     Type1 = Type2.
matchType(Type1, Type2) :-
    Type1 = Type2,
    !.
matchType(Type1, Type2) :-
    format("Error matchingType: ~p doesn't match with ~p~n", [Type1, Type2]),
    fail.
