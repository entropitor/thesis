:- module(questions, [
              askQuestion/3,
              setQuestionTopic/1,
              clearQuestionTopic/0
          ]).

:- dynamic questionTopic/1.

increaseNbQuestions(1) :-
    \+ nb_current(nbQuestions, _),
    nb_setval(nbQuestions, 1),
    !.
increaseNbQuestions(N1) :-
    b_getval(nbQuestions, N),
    N1 is N + 1,
    b_setval(nbQuestions, N1).
clearNbQuestions :-
    b_setval(nbQuestions, 0).

setQuestionTopic(X) :-
    clearQuestionTopic,
    assertz(questionTopic(X)).
clearQuestionTopic :-
    clearNbQuestions,
    retractall(questionTopic(_)).
:- clearQuestionTopic.

askQuestion(Question, Str, Answer) :-
    questionTopic(Topic),
    loadFile('output/cachedAnswers.pl'),
    answer(Topic, Question, Answer),
    simulationQuestionAskedToUser(Str, Answer),
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
    increaseNbQuestions(QuestionNumber),
    format("Question ~w: ~w~n", [QuestionNumber, Question]),
    read(Answer).

simulationQuestionAskedToUser(Str, Answer) :-
    increaseNbQuestions(QuestionNumber),
    format("Question ~w: ~w~n|: ~p.~n", [QuestionNumber, Str, Answer]).

loadFile(FileName) :-
    exists_file(FileName),
    !,
    consult(FileName).
loadFile(FileName) :-
    tell(FileName),
    writeln(':- dynamic answer/3.'),
    told,
    consult(FileName).

