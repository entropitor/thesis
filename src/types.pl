% -------------------------------------------------------------------------------
% Grammar Types
:- type conditions :=
        grammar{
            conditions: [condition],
            quantors: [quantified_variable]
        }.
:- type condition :=
        equal(property, amount)
        | and(condition, condition)
        | or(condition, condition)
        | if(conditions, conditions)
        | predicate(verb, entity, verb_attachment)
        | predicate(verb, verb_attachment, entity)
        | predicate(property_name, entity, property_value)
        | compare(comparison_function, property, amount).

:- type quantified_variable :=
        quantor(quantor, entity),
        | and([quantified_variable], [quantified_variable]).
        | or([quantified_variable], [quantified_variable]).
:- type quantor :=
        quantified(amount)
        | exists
        | forall.

:- type amount :=
        exactly(amount_value)
        | +(amount, amount_value)
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
        literal(property_value)
        | atom(property).
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
        atom(variable_name).
% -------------------------------------------------------------------------------
% Simplified types

:- type quantified_variable :=
        quantor:variable.
        | and([quantified_variable], [quantified_variable]).
        | or([quantified_variable], [quantified_variable]).
:- type variable :=
        atom(variable).
:- type quantor :=
        quantified(<comparison>(number))
        | exists
        | forall.
