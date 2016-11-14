:- module(parser, [input_atoms/2, parse/2, parse_string/3, all_parses/2, problem/2, parse_problem_using/2, parse_using/3, test/0]).
:- use_module(grammar).
:- use_module(print).

test :-
    %parse_problem(vakantiedagen, X),
    parse_problem(zebra, Y),
    %X == nil,
    Y == nil.

input_atoms(I, A) :-
    % to lower case
    string_lower(I, SLower),
    % make ',' its own atom by surrounding it with spaces
    % split_string(SLower, ",", " ", Splitted),
    % join_string(" , ", Splitted, Joined),
    % remove ','
    split_string(SLower, ",", " ", Splitted),
    join_string(" ", Splitted, Joined),
    % split on spaces
    atomics_to_string(A, ' ', Joined).

join_string(_, [], "").
join_string(_, [String], String) :- !.
join_string(JoinChar, [String | Strings], Result) :-
    join_string(JoinChar, Strings, Joined),
    string_concat(String, JoinChar, Temp),
    string_concat(Temp, Joined, Result).

parse(Atoms, Tree) :-
    phrase(grammar:s(Tree, Facts), Atoms),
    show_rules(Atoms, Facts).
parse_string(Sentence, Tree, Atoms) :-
    input_atoms(Sentence, Atoms),
    parse(Atoms, Tree).

all_parses(Atoms, Parses) :-
    findall(Tree, parse(Atoms, Tree), Parses).

parse_problem(Name, FirstFailedSentence) :-
    problem(Name, Sentences),
    maplist(input_atoms, Sentences, Atoms),
    maplist(all_parses, Atoms, Parses),
    maplist(length, Parses, NbMatches),
    pairs_keys_values(Pairs, Atoms, NbMatches),
    exclude(success, Pairs, FailedPairs),
    (
        pairs_keys(FailedPairs, [FirstFailedSentence | _])
     ->
         writeln(FirstFailedSentence),
         retractall(problem_sentence(_)),
         assert(problem_sentence(FirstFailedSentence))
     ;
         FirstFailedSentence = nil
    ),
    writeln(NbMatches),
    nl.

success(_-1).

problem(vakantiedagen, [
            "If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days",
            "If the years of service of an employee is at least 30, then he receives 5 extra days",
            "If years of service of an employee is at least 30 then he receives 3 extra days",
            "If age of an employee is at least 60 then he receives 3 extra days",
            "If the years of service of an employee is between 15 and 30 and he does not receive 5 extra days then he receives 2 extra days",
            "If the age of an employee is at least 45 and he does not receive 5 extra days then he receives 2 extra days",
            "Each employee has a number of vacation days equal to 22 plus the sum of the extra days he receives"
        ]).


%?- parse_tree("If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days", s).

problem(zebra, [
            "The Englishman lives in the red house",
            "The Spaniard keeps the dog",
            "There is a person who lives in the green house and drinks coffee",
            "The Ukrainian drinks tea",
            "The position of the green house is equal to the position of the ivory house plus 1",
            "There is a person who smokes Old Gold and keeps the snail",
            "There is a person who lives in a yellow house and smokes Kools",
            "The person who lives in the third house, drinks milk",
            "The Norwegian lives in the first house",
            "A house A is next to a house B if the absolute value of the difference between the position of house A and the position of house B is equal to 1",
            "The house in which the person who smokes Chesterfields lives, is next to the house in which the person who keeps the fox lives",
            "The house in which the person who smokes Kools lives, is next to the house in which the person who keeps a horse lives",
            "The person who drinks Lucky Strike, drinks Orange Juice",
            "The Japanese smokes Parliaments",
            "The Norwegian lives in a house that is next to the blue house",
            "Every animal is kept by exactly 1 Person"
      ]).

parse_problem_using(Rest, Terms) :-
    problem_sentence(X),
    parse_using(X, Rest, Terms).

parse_using(Atoms, Atoms, []).
parse_using(Atoms, Rest, [[] | Terms]) :-
    !,
    parse_using(Atoms, Rest, Terms).
parse_using([H | Rest], Rest2, [[H | T] | Terms]) :-
    !,
    parse_using(Rest, Rest2, [T | Terms]).
parse_using(Atoms, Rest2, [Term | Terms]) :-
    Term =.. [Functor | Args],
    Functor \= '[|]',
    Term2 =.. [Functor, _ | Args],
    phrase(grammar:Term2, Atoms, Rest),
    parse_using(Rest, Rest2, Terms).
