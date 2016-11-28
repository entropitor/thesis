:- type conditions :=
        grammar{
            conditions: [condition],
            quantors: [quantified_variable]
        }.
:- type condition :=
        equal(property, amount)
        | and(conditions, conditions)
        | or(conditions, conditions)
        | predicate(verb, entity, verb_attachment)
        | predicate(verb, verb_attachment, entity)
        | predicate(property_name, entity, property_value)
        | compare(comparison_function, property, amount).

:- type quantified_variable :=
        quantor(quantor, entity).
:- type quantor :=
        quantified(amount)
        | exists
        | forall.

:- type amount :=
        exactly(amount_value)
        | +(amount_value, amount_value)
        | amount_value.
:- type amount_value :=
        literal(number)
        | property(property, entity).
:- type comparison_function :=
        '='
        | '<'
        | '>'
        | '>='
        | '<='.

:- type property :=
        atom(property).
:- type property_value :=
        atom(property_value).
:- type verb :=
        atom(verb).
:- type entity :=
        literal(atom(named_entity), variable)
        | var(variable, atom(entity)).
:- type verb_attachment :=
        entity
        | property_value.

:- type variable :=
        unnamed(atom(unnamed_variable))
        | named(atom(named_variable)).
