:- module(questions, [
              askQuestion/3
          ]).

askQuestion(Question, _, Answer) :-
    loadFile('output/cachedAnswers.pl'),
    answer(Question, Answer),
    !.
askQuestion(Question, Str, Answer) :-
    askQuestionToUser(Str, Answer),
    assertz(answer(Question, Answer)),
    tell('output/cachedAnswers.pl'),
    listing(answer/2),
    told.

askQuestionToUser(Question, Answer) :-
    writeln(Question),
    read(Answer).

loadFile(FileName) :-
    exists_file(FileName),
    !,
    consult(FileName).
loadFile(FileName) :-
    tell(FileName),
    writeln(':- dynamic answer/2.'),
    told,
    consult(FileName).

