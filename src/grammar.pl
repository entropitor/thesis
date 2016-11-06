:- module(grammar, [
              s/3
          ]).
:- use_module(dcg_macro).

:- discontiguous entity/3.
:- discontiguous verb/3.
:- discontiguous property/3.
:- discontiguous property_value/3.

s ---> [if], cond, [then], expr.
s ---> expr, [if], cond.
s ---> cond.

expr ---> property_phrase, [of], entity_phrase, [equals], amount.
expr ---> entity_phrase, verb_phrase.

cond ---> cond1.
cond ---> cond1, [and], cond.
cond ---> cond1, [or], cond.

cond1 ---> property_phrase, [of], entity_phrase, [is], comparison.
cond1 ---> entity_phrase, verb_phrase.
cond1 ---> entity_phrase, [is], verb, [by], verb_attachment.
cond1 ---> entity_phrase, [does, not], verb_phrase.
cond1 ---> entity_phrase, [has, a, number, of], property_phrase, [equal, to], amount.
cond1 ---> [there, is], entity_phrase.

verb_phrase ---> verb_phrase1.
verb_phrase ---> verb_phrase1, [and], verb_phrase.

verb_phrase1 ---> verb, verb_attachment.
verb_phrase1 ---> verb_attachment, verb.

verb_attachment ---> object.
verb_attachment ---> place.
verb_attachment ---> entity_phrase.

property_phrase ---> one_of::[the, a, an], property.
property_phrase ---> property.
property_phrase ---> [the, absolute, value, of], property_phrase.
property_phrase ---> [the, difference, between], property_phrase, [and], property_phrase.
property_phrase ---> one_of::[the, a, an], property, [of], entity_phrase.
property_phrase ---> property, [of], entity_phrase.

entity_phrase ---> entity_phrase1.
entity_phrase ---> entity_phrase1, subsentence.

entity_phrase1 ---> determiner, entity.
entity_phrase1 ---> determiner, entity, variable_name.
entity_phrase1 ---> determiner, property_value, entity.
entity_phrase1 ---> entity, variable_name.
entity_phrase1 ---> variable_name.
entity_phrase1 ---> [he].

variable_name ---> one_of::[a, b, c, d, e, f, g, h, i, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z].

comparison ---> comparison_function, amount.
comparison ---> [between], amount, [and], amount.
comparison ---> comparison_function, amount, [or], comparison.
comparison ---> comparison_function, amount, [and], comparison.

comparison_function ---> one_of::[[less, than], [at, least], [more, than],
                                  [greather, than], [equal, to]].

amount ---> [exactly], amount1.
amount ---> amount1.
amount ---> amount1, [plus], amount.

amount1 ---> amount_literal.
amount1 ---> property_phrase, [of], entity_phrase.
amount1 ---> [the, sum, of], property_phrase, entity_phrase, verb.

amount_literal ---> [X], {number(X)}.
amount_literal ---> [X], {atom(X), atom_number(X, _)}.

object ---> amount, property.
object ---> amount, entity.
object ---> optional::determiner, property_value.

place ---> one_of::[in, at], entity_phrase.

subsentence ---> one_of::[who, that, [in, which]], verb_phrase.

determiner ---> one_of::[each, every, a, an, the].

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
entity ---> one_of::[englishman, spaniard, ukrainian, japanese, norwegian].
property_value ---> one_of::[red, green, ivory, yellow, blue].
entity ---> [animal].
property_value ---> one_of::[dog, zebra, snail, fox, horse].
property_value ---> one_of::[coffee, tea, milk, water, [orange, juice]].
entity ---> [cigarette].
property_value ---> one_of::[chesterfields, kools, parliaments, [old, gold], [lucky, strike]].

entity ---> [house].
property ---> [position].
property_value ---> one_of::[first, second, third, fourth, fifth].
property ---> [color].

verb ---> [lives].
verb ---> [be].
verb ---> [keeps].
verb ---> [kept].
verb ---> [drinks].
verb ---> [smokes].
verb ---> [is, next, to].
