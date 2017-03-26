:- module(questions, [
              askQuestion/3,
              setQuestionTopic/1,
              clearQuestionTopic/0
          ]).

:- dynamic questionTopic/1.

setQuestionTopic(X) :-
    clearQuestionTopic,
    assertz(questionTopic(X)).
clearQuestionTopic :-
    retractall(questionTopic(_)).
:- clearQuestionTopic.

askQuestion(Question, _, Answer) :-
    questionTopic(Topic),
    loadFile('output/cachedAnswers.pl'),
    answer(Topic, Question, Answer),
    !.
askQuestion(Question, Str, Answer) :-
    questionTopic(Topic),
    askQuestionToUser(Str, Answer),
    assertz(answer(Topic, Question, Answer)),
    tell('output/cachedAnswers.pl'),
    listing(answer/3),
    told.
askQuestion(_, _, _) :-
    \+ questionTopic(_),
    error('no question topic found').


askQuestionToUser(Question, Answer) :-
    writeln(Question),
    read(Answer).

loadFile(FileName) :-
    exists_file(FileName),
    !,
    consult(FileName).
loadFile(FileName) :-
    tell(FileName),
    writeln(':- dynamic answer/3.'),
    told,
    consult(FileName).

