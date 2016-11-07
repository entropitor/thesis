
:- op(1200, xfx, (:=)).
:- op(600, fx, (|)).
:- op(600, fx, (...)).

statement := fact(predicate, [...arguments]) | and(fact, fact) | or(fact, fact).


% The Englishman lives in the red house
% ? x: lives(Englishman, x) & color(x, red).
% exists(var(X, house), and(predicate(lives, literal(Englishman), var(X, house)), predicate(color, var(X, house), literal(red))))
