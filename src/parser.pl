:- module(parser, [parse/2,
                   parse_string/3,
                   all_parses/2,
                   parse_using/3,
                   parse_problem/2,
                   parse_sentences/2]).

:- use_module(grammar).
:- use_module(print).
:- use_module(problems).
:- use_module(processor).

parse(Atoms, Tree) :-
    phrase(grammar:s(Tree, Facts), Atoms),
    process(Atoms, Facts, Tree, _Theory).
    %show_rules(Atoms, Facts, Tree).
parse_string(Sentence, Tree, Atoms) :-
    input_atoms(Sentence, Atoms),
    parse(Atoms, Tree).

% find all parses for the list of atoms
all_parses(Atoms, Parses) :-
    findall(Tree, parse(Atoms, Tree), Parses).

% parse_using([atoms], [remaining_atoms], [grammar_symbols])
% parses the list of atoms using the list of grammar_symbols leaving the list of remaining atoms
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

% parse a problem with the given name
parse_problem(Name, FirstFailedSentence) :-
    problem(Name, Sentences),
    parse_sentences(Sentences, FirstFailedSentence).
% parse a list of sentences
parse_sentences(Sentences, FirstFailedSentence) :-
    maplist(input_atoms, Sentences, Atoms),
    maplist(all_parses, Atoms, Parses),
    maplist(length, Parses, NbMatches),
    pairs_keys_values(Pairs, Atoms, NbMatches),
    exclude(success, Pairs, FailedPairs),
    (
        pairs_keys(FailedPairs, [FirstFailedSentence | _])
     ->
         writeln(FirstFailedSentence)
     ;
     FirstFailedSentence = nil
    ),
    writeln(NbMatches),
    nl.
% ---------------------------------------------------------------------

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
success(_-1).

