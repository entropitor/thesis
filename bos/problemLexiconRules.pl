:- module(problemLexiconRules, [
              lexEntry/2,
              useLexicon/1
          ]).

:- use_module(myLexicon, [lexEntry/2 as defaultLexicon]).
:- use_module(types, [addType/2, addTypeAttribute/2]).

:- dynamic pLexEntry/2.

useLexicon(Rules) :-
    retractall(pLexEntry(_, _)),
    maplist(addRule, Rules).

lexEntry(X, Y) :-
    defaultLexicon(X, Y).
lexEntry(X, Y) :-
    pLexEntry(X, Y).

syntax_symbol(Syntax, Symbol) :-
    atomic_list_concat(Syntax, '_', Symbol).
symbol_syntax(Symbol, Syntax) :-
    split_string(Symbol, '_', '', L),
    maplist(atom_chars, Syntax, L).

/*========================================================================
    Rules
========================================================================*/
addRule(noun(_Type, SyntaxSg, SyntaxPl)) :-
    syntax_symbol(SyntaxSg, Symbol),
    assertz(pLexEntry(noun, [symbol:Symbol, num:sg, syntax:SyntaxSg, vType:Type]) :- addType(noun-Symbol, Type)),
    assertz(pLexEntry(noun, [symbol:Symbol, num:pl, syntax:SyntaxPl, vType:Type]) :- addType(noun-Symbol, Type)).
addRule(pn(_Type, Syntax)) :-
    syntax_symbol(Syntax, Symbol),
    assertz((pLexEntry(pn, [symbol:Symbol, syntax:Syntax, num:sg, vType:Type]) :- addType(pn-Symbol, Type), addTypeAttribute(Type, qualified))).
addRule(ppn(_Type, Syntax)) :-
    syntax_symbol(Syntax, Symbol),
    assertz((pLexEntry(pn, [symbol:Symbol, syntax:Syntax, num:_, vType:Type]) :- addType(pn-Symbol, Type), addTypeAttribute(Type, qualified))).
addRule(ivpp(_Type, SyntaxSg, PP, SyntaxInf)) :-
    Type = pred(SubjType, ObjType),
    append(SyntaxSg, PP, WordForm),
    syntax_symbol(WordForm, Symbol),
    assertz(pLexEntry(ivpp, [symbol:Symbol, syntax:SyntaxSg, pp:PP, inf:fin, num:sg, vType:Type]) :- addType(ivpp-Symbol, Type)),
    assertz(pLexEntry(ivpp, [symbol:Symbol, syntax:SyntaxInf, pp:PP, inf:inf, num:sg, vType:Type]) :- addType(ivpp-Symbol, Type)),
    assertz(pLexEntry(prep, [symbol:Symbol, syntax:PP, vType:pred(SubjType, ObjType)])).
addRule(tv(_Type, SyntaxSg, SyntaxInf)) :-
    syntax_symbol(SyntaxSg, Symbol),
    assertz(pLexEntry(tv, [symbol:Symbol, syntax:SyntaxSg, inf:fin, num:sg, vType:Type]) :- addType(tv-Symbol, Type)),
    assertz(pLexEntry(tv, [symbol:Symbol, syntax:SyntaxInf, inf:inf, num:sg, vType:Type]) :- addType(tv-Symbol, Type)).
addRule(tvgap(_Type, SyntaxSg, Gap, SyntaxInf)) :-
    append(SyntaxSg, Gap, WordForm),
    syntax_symbol(WordForm, Symbol),
    assertz(pLexEntry(tvgap, [symbol:Symbol, syntax:SyntaxSg, gap:Gap, inf:fin, num:sg, vType:Type]) :- addType(tvgap-Symbol, Type)),
    assertz(pLexEntry(tvgap, [symbol:Symbol, syntax:SyntaxInf, gap:Gap, inf:inf, num:sg, vType:Type]) :- addType(tvgap-Symbol, Type)).
addRule(prep(_Type, Syntax)) :-
    syntax_symbol(Syntax, Symbol),
    assertz(pLexEntry(prep, [symbol:Symbol, syntax:Syntax, vType:Type]) :- addType(prep-Symbol, Type)).
addRule(comp(Type, Syntax)) :-
    assertz(pLexEntry(comp, [type:Type, syntax:Syntax])).
%% addRule(pnn(_Type, Syntax, _Number)) :-
%%     addRule(pn(_, Syntax)).



addRule(pn(Syntax)) :-
    syntax_symbol(Syntax, Symbol),
    assertz((pLexEntry(pn, [symbol:Symbol, syntax:Syntax, num:sg, vType:Type]) :- addType(pn-Symbol, Type), addTypeAttribute(Type, qualified))).
addRule(pnn(Syntax, Number)) :-
    syntax_symbol(Syntax, Symbol),
    assertz((pLexEntry(pn, [symbol:Number, syntax:Syntax, num:sg, vType:Type]) :- addType(pn-Symbol, Type), addTypeAttribute(Type, countable))).
addRule(noun(SyntaxSg, SyntaxPl)) :-
    syntax_symbol(SyntaxSg, Symbol),
    assertz(pLexEntry(noun, [symbol:Symbol, num:sg, syntax:SyntaxSg, vType:Type]) :- addType(noun-Symbol, Type)),
    assertz(pLexEntry(noun, [symbol:Symbol, num:pl, syntax:SyntaxPl, vType:Type]) :- addType(noun-Symbol, Type)).
addRule(tvPrep(SyntaxSg, PP, SyntaxInf, SyntaxPart)) :-
    Type = pred(SubjType, ObjType),
    append(SyntaxSg, PP, WordForm),
    syntax_symbol(WordForm, Symbol),
    assertz(pLexEntry(ivpp, [symbol:Symbol, syntax:SyntaxSg, pp:PP, inf:fin, num:sg, vType:Type]) :- addType(ivpp-Symbol, Type)),
    assertz(pLexEntry(ivpp, [symbol:Symbol, syntax:SyntaxInf, pp:PP, inf:inf, num:sg, vType:Type]) :- addType(ivpp-Symbol, Type)),
    assertz(pLexEntry(ivpp, [symbol:Symbol, syntax:SyntaxPart, pp:PP, inf:part, num:sg, vType:Type]) :- addType(ivpp-Symbol, Type)),
    assertz(pLexEntry(prep, [symbol:Symbol, syntax:PP, vType:pred(SubjType, ObjType)])).
addRule(prep(Syntax)) :-
    syntax_symbol(Syntax, Symbol),
    assertz(pLexEntry(prep, [symbol:Symbol, syntax:Syntax, vType:Type]) :- addType(prep-Symbol, Type)).
