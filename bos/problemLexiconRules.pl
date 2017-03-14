:- module(problemLexiconRules, [lexEntry/2]).

:- use_module(myLexicon, [lexEntry/2 as defaultLexicon]).
:- use_module(problemLexicon, [concept/2, property/3, relation/5, actor/5]).
:- use_module(types, [addType/2]).

lexEntry(noun, [symbol:contestant, num:pl, syntax:[contestants], vType:_]).

lexEntry(X, Y) :-
    defaultLexicon(X, Y).

lexEntry(noun, [symbol:Symbol, num:sg, syntax:Syntax, vType:Type]) :-
    concept(Symbol, _),
    addType(Symbol, Type),
    symbol_syntax(Symbol, Syntax).
lexEntry(pn, [symbol:Symbol, syntax:Syntax, vType:qualified(Type)]) :-
    concept(_Type, constructed:Elements),
    addType(Symbol, qualified(Type)),
    member(Syntax, Elements),
    % TODO check that it isn't an adjective?
    syntax_symbol(Syntax, Symbol).
lexEntry(adj, [symbol:Symbol, syntax:Syntax, vType:Type]) :-
    concept(Concept, constructed:Elements),
    property(_Type, _, Concept),
    addType(Symbol, Type),
    member(Syntax, Elements),
    syntax_symbol(Syntax, Symbol).
lexEntry(adj, [symbol:Symbol, syntax:Syntax, vType:adj(Type)]) :-
    member(Syntax, [[first], [second], [third], [fourth], [fifth], [sixth]]),
    property(_Type, _, int),
    addType(Symbol, adj(Type)),
    syntax_symbol(Syntax, Symbol).
% TODO: need other wordforms?
lexEntry(tv, [symbol:Symbol, syntax:Syntax, inf:fin, num:sg, vType:pred(SubjType, ObjType)]) :-
    relation(_SubjType, _ObjType, Syntax, [], SyntaxInf),
    addType(Symbol, pred(SubjType, ObjType)),
    syntax_symbol(SyntaxInf, Symbol).
lexEntry(tv, [symbol:Symbol, syntax:Syntax, inf:inf, num:sg, vType:pred(SubjType, ObjType)]) :-
    relation(_SubjType, _ObjType, _, [], Syntax),
    addType(Symbol, pred(SubjType, ObjType)),
    syntax_symbol(Syntax, Symbol).
lexEntry(ivpp, [symbol:Symbol, syntax:Syntax, pp:PP, inf:fin, num:sg, vType:pred(SubjType, ObjType)]) :-
    relation(_SubjType, _ObjType, Syntax, PP, SyntaxInf),
    PP \= [],
    addType(Symbol, pred(SubjType, ObjType)),
    append(SyntaxInf, PP, WordForm),
    syntax_symbol(WordForm, Symbol).
lexEntry(ivpp, [symbol:Symbol, syntax:Syntax, pp:PP, inf:inf, num:sg, vType:pred(SubjType, ObjType)]) :-
    relation(_SubjType, _ObjType, _, PP, Syntax),
    PP \= [],
    addType(Symbol, pred(SubjType, ObjType)),
    append(Syntax, PP, WordForm),
    syntax_symbol(WordForm, Symbol).
lexEntry(prep, [symbol:Symbol, syntax:PP, vType:null]) :-
    relation(_, _, _, PP, _),
    PP \= [],
    syntax_symbol(PP, Symbol).
%% lexEntry(iv, [symbol:Symbol, syntax:Syntax, inf:fin, num:sg, vType:Type]) :-
%%     relation(_Type, Syntax),
%%     addType(Symbol, Type),
%%     syntax_symbol(Syntax, Symbol).

lexEntry(prep, [symbol:Symbol, syntax:PP, vType:fun(SubjType, ObjType)]) :-
    actor(_, _, SyntaxNoun, PP, _),
    append(SyntaxNoun, PP, Syntax),
    addType(Symbol, fun(SubjType, ObjType)),
    syntax_symbol(Syntax, Symbol).
lexEntry(noun, [symbol:Symbol, num:sg, syntax:Syntax, vType:Type]) :-
    actor(_, _, Syntax, _, _),
    addType(Symbol, Type),
    syntax_symbol(Syntax, Symbol).

%TODO: fix plural vs singular
lexEntry(noun, [symbol:Symbol, num:_, syntax:Syntax, vType:Type]) :-
    concept(_Type, countable:Syntax),
    addType(Symbol, Type),
    syntax_symbol(Syntax, Symbol).

syntax_symbol(Syntax, Symbol) :-
    atomic_list_concat(Syntax, '_', Symbol).
symbol_syntax(Symbol, Syntax) :-
    split_string(Symbol, '_', '', L),
    maplist(atom_chars, Syntax, L).
