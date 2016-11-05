:- module(grammar, [
              s/3
          ]).
:- use_module(dcg_tree_expansion).

:- discontiguous entity/3.
:- discontiguous verb/3.
:- discontiguous property/3.

s ---> [if], cond, optional(','), [then], expr.
s ---> expr, [if], cond.
s ---> cond.

optional(X) ---> [X].
optional(_) ---> [].

expr ---> property_phrase, [of], entity_phrase, [equals], amount.
expr ---> entity_phrase, verb, object.
expr ---> entity_phrase, verb, place.

cond ---> cond1.
cond ---> cond1, [and], cond.
cond ---> cond1, [or], cond.

cond1 ---> property_phrase, [of], entity_phrase, [is], comparison.
cond1 ---> entity_phrase, verb, object.
cond1 ---> entity_phrase, verb, place.
cond1 ---> entity_phrase, [does, not], verb, object.
cond1 ---> entity_phrase, [has, a, number, of], property_phrase, [equal, to], amount.
cond1 ---> [there, is], entity_phrase, subsentence.

property_phrase ---> [X], {member(X, [the, a, an])}, property.
property_phrase ---> property.

entity_phrase ---> [X], {member(X, [each, every, a, an, the])}, entity.
entity_phrase ---> [X], {member(X, [each, every, a, an, the])}, property_value, entity.
entity_phrase ---> [he].

comparison ---> comparison_function, amount.
comparison ---> comparison_function, amount, [or], comparison.
comparison ---> comparison_function, amount, [and], comparison.

comparison_function ---> [less, than].
comparison_function ---> [at, least].
comparison_function ---> [more, than].
comparison_function ---> [greather, than].
comparison_function ---> [between], amount, [and].

amount ---> amount1.
amount ---> amount1, [plus, the, sum, of], property_phrase, entity_phrase, verb.

amount1 ---> [X], {number(X)}.
amount1 ---> [X], {atom(X), atom_number(X, _)}.

object ---> amount, property.
object ---> opt_determiner, property_value.

place ---> [X], {member(X, [in, at])}, entity_phrase.

subsentence ---> [X], {member(X, [who, that])}, verb, place, [and], verb, object.


opt_determiner ---> [].
opt_determiner ---> determiner.

determiner ---> [X], {member(X, [each, every, a, an, the])}.

% Vakantiedagen!
property ---> [years, of, service].
property ---> [age].
property ---> [extra, days].
property ---> [vacation, days].
entity ---> [employee].
verb ---> [receives].
verb ---> [receive].

% Zebra
entity ---> [person].
entity ---> [X], {member(X, [englishman, spaniard, ukrainian, japanese, norwegian])}.
% property ---> [animal].
% property ---> [drink].
% property ---> [cigarette].
property_value ---> [X], {member(X, [red, green, ivory, yellow, blue])}.
property_value ---> [X], {member(X, [dog, zebra, snail, fox, horse])}.
property_value ---> [X], {member(X, [coffee, tea, milk, water])}.
property_value ---> [orange, juice].
property_value ---> [cigarette].
property_value ---> [X], {member(X, [chesterfields, kools, parliaments])}.
property_value ---> [old, gold].
property_value ---> [lucky, strike].

entity ---> [house].
property ---> [position].
property ---> [color].

verb ---> [lives].
verb ---> [be].
verb ---> [keeps].
verb ---> [drinks].
verb ---> [smokes].
