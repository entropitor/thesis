:- module(grammar, [
              s/4
          ]).
:- use_module(dcg_macro).

:- discontiguous entity/5.
:- discontiguous verb/4.
:- discontiguous property/4.
:- discontiguous property_value/5.

:- discontiguous cond1/3.
:- discontiguous cond1/5.
:- discontiguous entity_phrase1/6.
:- discontiguous entity_phrase1/3.
:- discontiguous amount1/3.
:- discontiguous amount/3.
:- discontiguous property_phrase/3.
:- discontiguous property_phrase/6.

s(if(Cond, Expr)) ---> [if], cond([], Cond), [then], expr([], Expr).
s(if(Cond, Expr)) ---> expr([], Expr), [if], cond([], Cond).
s(COut) ---> cond([], COut).

expr(CIn, [equal(property(Property, Entity), Amount) | COut3]) ---> property_phrase(Property, CIn, COut1), [of], entity_phrase(Entity, COut1, COut2), [equals], amount(Amount, COut2, COut3).
expr(CIn, COut2) ---> entity_phrase(Entity, CIn, COut1), verb_phrase(Entity, COut1, COut2).

cond(CIn, COut) ---> cond1(CIn, COut).
cond(CIn, [and(COut1, COut2) | CIn]) ---> cond1([], COut1), [and], cond([], COut2).
cond(CIn, [or(COut1, COut2) | CIn]) ---> cond1([], COut1), [or], cond([], COut2).

cond1(CIn, COut) ---> property_phrase(Property, CIn, COut1), [is], comparison(Property, COut1, COut).
cond1(CIn, COut) ---> entity_phrase(Entity, CIn, COut1), verb_phrase(Entity, COut1, COut).
cond1 ---> entity_phrase, [is], verb, [by], verb_attachment.
cond1 ---> entity_phrase, [does, not], verb_phrase.
cond1 ---> entity_phrase, [has, a, number, of], property_phrase, [equal, to], amount.
cond1(CIn, [exists(Entity, COut)]) ---> [there, is], entity_phrase(Entity, CIn, COut).

verb_phrase(Entity, CIn, COut) ---> verb_phrase1(Entity, CIn, COut).
verb_phrase(Entity, CIn, [and(COut1, COut2) | CIn]) ---> verb_phrase1(Entity, [], COut1), [and], verb_phrase(Entity, [], COut2).

verb_phrase1(Entity, CIn, COut) ---> verb(Verb), verb_attachment(Entity, Verb, CIn, COut).
verb_phrase1 ---> verb_attachment, verb.

verb_attachment(Entity, Verb, CIn, COut) ---> object(Entity, Verb, CIn, COut).
verb_attachment(Entity, Verb, CIn, COut) ---> place(Entity, Verb, CIn, COut).
verb_attachment(Entity1, Verb, CIn, [predicate(Verb, Entity1, Entity2) | COut1]) ---> entity_phrase(Entity2, CIn, COut1).

property_phrase(Property, CIn, CIn) ---> one_of::[the, a, an], property(Property).
property_phrase ---> property.
property_phrase(abs(Property), CIn, COut) ---> [the, absolute, value, of], property_phrase(Property, CIn, COut).
property_phrase(-(Property1, Property2), CIn, COut) ---> [the, difference, between], property_phrase(Property1, CIn, COut1), [and], property_phrase(Property2, COut1, COut).
property_phrase(property(Property, Entity), CIn, COut) ---> one_of::[the, a, an], property(Property), [of], entity_phrase(Entity, CIn, COut).
property_phrase ---> property, [of], entity_phrase.

entity_phrase(Entity, CIn, COut) ---> entity_phrase1(Entity, CIn, COut).
entity_phrase(Entity, CIn, COut) ---> entity_phrase1(Entity, CIn, COut1), subsentence(Entity, COut1, COut).

entity_phrase1(Entity, C, C) ---> determiner, entity(Entity, unnamed(_)).
entity_phrase1(Entity, C, C) ---> optional::determiner, entity(Entity, named(Name)), variable_name(Name).
entity_phrase1(Entity, CIn, [predicate(PropertyName, Entity, PropertyValue) | CIn]) ---> determiner, property_value(PropertyName, PropertyValue), entity(Entity, unnamed(_)).
entity_phrase1 ---> variable_name.
entity_phrase1 ---> [he].

variable_name(Name) ---> [Name], {member(Name, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z])}.

comparison(LeftHand, CIn, [Cond | COut]) ---> comparison_function(Cond, LeftHand, RightHand), amount(RightHand, CIn, COut).
comparison ---> [between], amount, [and], amount.
comparison ---> comparison_function, amount, [or], comparison.
comparison ---> comparison_function, amount, [and], comparison.

comparison_function(=(A, B), A, B) ---> [equal, to].
comparison_function(<(A, B), A, B) ---> [less, than].
comparison_function ---> one_of::[[at, least], [more, than], [greather, than]].

amount ---> [exactly], amount1.
amount(Amount, CIn, COut) ---> amount1(Amount, CIn, COut).
amount(+(Amount1, Amount2), CIn, COut) ---> amount1(Amount1, CIn, COut1), [plus], amount(Amount2, COut1, COut).

amount1(literal(X), CIn, CIn) ---> amount_literal(X).
amount1(property(Property, Entity), CIn, COut) ---> property_phrase(Property, CIn, COut1), [of], entity_phrase(Entity, COut1, COut).
amount1 ---> [the, sum, of], property_phrase, entity_phrase, verb.

amount_literal(X) ---> [X], {number(X)}.
amount_literal(X) ---> [X], {atom(X), atom_number(X, _)}.

object ---> amount, property.
object ---> amount, entity.
object(Entity, Verb, CIn, [predicate(Verb, Entity, Value) | CIn]) ---> optional::determiner, property_value(_PropertyName, Value). % TODO: type check PropertyName with Verb

place(Entity, Verb, CIn, [predicate(Verb, Entity, Place) | COut1]) ---> one_of::[in, at], entity_phrase(Place, CIn, COut1).


subsentence(Entity, CIn, COut) ---> one_of::[who, that], verb_phrase(Entity, CIn, COut).
subsentence ---> [in, which], verb_phrase.

determiner ---> one_of::[each, every, a, an, the].

% Vakantiedagen!
property_list::[[years, of, service], [age], [extra, days], [vacation, days]].
entity::[employee].
verb_list::[receives, receive].

% Zebra
entity_list::[person, animal, cigarette].
named_entity_list::[englishman, spaniard, ukrainian, japanese, norwegian].
property_value_list::color-[red, green, ivory, yellow, blue].
property_value_list::animal-[dog, zebra, snail, fox, horse].
property_value_list::drink-[coffee, tea, milk, water, [orange, juice]].
property_value_list::cigarette-[chesterfields, kools, parliaments, [old, gold], [lucky, strike]].

entity::house.
property_list::[position, color].
property_value_list::position-[first, second, third, fourth, fifth].

verb_list::[lives, be, keeps, kept, drinks, smokes, [is, next, to]].
