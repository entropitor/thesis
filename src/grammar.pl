:- module(grammar, [
              s/4
          ]).
:- use_module(dcg_macro).

:- discontiguous entity/5.
:- discontiguous verb/4.
:- discontiguous property/4.
:- discontiguous property_value/5.

:- discontiguous cond1/4.
:- discontiguous property_phrase/3.
:- discontiguous property_phrase/5.

s(if(Cond, Expr)) ---> [if], cond([]-Cond), [then], expr([]-Expr).
s(if(Cond, Expr)) ---> expr([]-Expr), [if], cond([]-Cond).
s(Out) ---> cond([]-Out).

expr(In-[equal(property(Property, Entity), Amount) | Out3]) ---> property_phrase(Property, In-Out1), [of], entity_phrase(Entity, Out1-Out2), [equals], amount(Amount, Out2-Out3).
expr(In-Out2) ---> entity_phrase(Entity, In-Out1), verb_phrase(Entity, Out1-Out2).

cond(In-Out) ---> cond1(In-Out).
cond(In-[and(Out1, Out2) | In]) ---> cond1([]-Out1), [and], cond([]-Out2).
cond(In-[or(Out1, Out2) | In]) ---> cond1([]-Out1), [or], cond([]-Out2).

cond1(In-Out) ---> property_phrase(Property, In-Out1), [is], comparison(Property, Out1-Out).
cond1(In-Out) ---> entity_phrase(Entity, In-Out1), verb_phrase(Entity, Out1-Out).
cond1(In-[predicate(passive:Verb, Attachment, Entity) | Out]) ---> entity_phrase(Entity, In-Out1), [is], verb(Verb), [by], verb_attachment(Attachment, Out1-Out).
cond1 ---> entity_phrase, [does, not], verb_phrase.
cond1 ---> entity_phrase, [has, a, number, of], property_phrase, [equal, to], amount.
cond1(In-exists(Entity, Out)) ---> [there, is], entity_phrase(Entity, In-Out).

verb_phrase(Entity, In-Out) ---> verb_phrase1(Entity, In-Out).
verb_phrase(Entity, In-[and(Out1, Out2) | In]) ---> verb_phrase1(Entity, []-Out1), [and], verb_phrase(Entity, []-Out2).

verb_phrase1(Entity, In-[predicate(Verb, Entity, Attachment) | Out]) ---> verb(Verb), verb_attachment(Attachment, In-Out).
verb_phrase1(Entity, In-[predicate(Verb, Attachment, Entity) | Out]) ---> verb_attachment(Attachment, In-Out), verb(Verb).

verb_attachment(Object, In-Out) ---> object(Object, In-Out).
verb_attachment(Place, In-Out) ---> place(Place, In-Out).
verb_attachment(Entity, In-Out) ---> entity_phrase(Entity, In-Out).

object ---> amount, property.
object(Entity, In-[quantified(Amount, Entity) | Out1]) ---> amount(Amount, In-Out1), entity(Entity, unnamed(_)).
% TODO: type check PropertyName with Verb
object(Value, In-In) ---> optional::determiner(_Quantor), property_value(_PropertyName, Value).

place(Place, In-Out1) ---> one_of::[in, at], entity_phrase(Place, In-Out1).

property_phrase(Property, In-In) ---> one_of::[the, a, an], property(Property).
property_phrase ---> property.
property_phrase(abs(Property), In-Out) ---> [the, absolute, value, of], property_phrase(Property, In-Out).
property_phrase(-(Property1, Property2), In-Out) ---> [the, difference, between], property_phrase(Property1, In-Out1), [and], property_phrase(Property2, Out1-Out).
property_phrase(property(Property, Entity), In-Out) ---> one_of::[the, a, an], property(Property), [of], entity_phrase(Entity, In-Out).
property_phrase ---> property, [of], entity_phrase.

entity_phrase(Entity, In-Out) ---> entity_phrase1(Entity, In-Out).
entity_phrase(Entity, In-Out) ---> entity_phrase1(Entity, In-Out1), subsentence(Entity, Out1-Out).

entity_phrase1(Entity, In-[quantor(Quantor, Entity) | In]) ---> determiner(Quantor), entity(Entity, unnamed(_)).
entity_phrase1(Entity, In-[quantor(Quantor, Entity) | In]) ---> determiner(Quantor), entity(Entity, named(Name)), variable_name(Name).
entity_phrase1(Entity, In-In) ---> entity(Entity, named(Name)), variable_name(Name).
entity_phrase1(Entity, In-[predicate(PropertyName, Entity, PropertyValue), quantor(Quantor, Entity) | In]) ---> determiner(Quantor), property_value(PropertyName, PropertyValue), entity(Entity, unnamed(_)).
entity_phrase1 ---> variable_name.
entity_phrase1 ---> [he].

variable_name(Name) ---> [Name], {member(Name, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z])}.

comparison(LeftHand, In-[Cond | Out]) ---> comparison_function(Cond, LeftHand, RightHand), amount(RightHand, In-Out).
comparison ---> [between], amount, [and], amount.
comparison ---> comparison_function, amount, [or], comparison.
comparison ---> comparison_function, amount, [and], comparison.

comparison_function(=(A, B), A, B) ---> [equal, to].
comparison_function(<(A, B), A, B) ---> [less, than].
comparison_function ---> one_of::[[at, least], [more, than], [greather, than]].

amount(exactly(Amount), In-Out) ---> [exactly], amount1(Amount, In-Out).
amount(Amount, In-Out) ---> amount1(Amount, In-Out).
amount(+(Amount1, Amount2), In-Out) ---> amount1(Amount1, In-Out1), [plus], amount(Amount2, Out1-Out).

amount1(literal(X), In-In) ---> amount_literal(X).
amount1(property(Property, Entity), In-Out) ---> property_phrase(Property, In-Out1), [of], entity_phrase(Entity, Out1-Out).
amount1 ---> [the, sum, of], property_phrase, entity_phrase, verb.

amount_literal(X) ---> [X], {number(X)}.
amount_literal(X) ---> [X], {atom(X), atom_number(X, _)}.

subsentence(Entity, In-Out) ---> one_of::[who, that], verb_phrase(Entity, In-Out).
% TODO: fix the rev() thingy
subsentence(Entity, In-Out) ---> [in, which], verb_phrase(rev(Entity), In-Out).

% TODO: fix quantors
determiner(exists) ---> one_of::[a, an].
determiner(forall) ---> one_of::[each, every, the].

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



%% _.default() := grammar{
%%              conditions: [],
%%              quantors: []
%%          }.
%% _.from_condition(Cond) := _.default().add_condition(Cond).

%% In.add_condition(Cond) := In.put([conditions = [Cond | In.conditions]]).
%% In.add_quantor(Quantor) := In.put([quantors = [Quantor | In.quantors]]).

%% In.add_condition(Cond, Out1, Out2) := In.add_condition(Cond).put([quantors = Qs]) :-
%%     append(In.quantors, Out1.quantors, Qs1),
%%     append(Qs1, Out2.quantors, Qs).
