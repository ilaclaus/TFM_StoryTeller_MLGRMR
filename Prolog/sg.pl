:- use_module(library(lists)).
:- use_module(library(http/json)).


:- use_module(grammar).

:- use_module(kb).

%:- dynamic wv/4.
%:- dynamic domain/2.
%:- dynamic created_domain/3.
%:- discontiguous possible/4.
:- discontiguous domain/4.
%:- discontiguous consequence/4.
:- discontiguous domain/5.

%domain(D, A, B) :- created_domain(D, A, B).

%mn([[isa(tobby, dog), doesfor(tobby, run, fun), -capableof(tobby, ride_horse)], [hasproperty(tobby, old), -capableof(tobby, run)], [learnto(tobby, ride_horse), capableof(tobby, ride_horse), doesfor(tobby, ride_horse, fun)]]).

%Fact
%json([tag='Fact',_factArguments=[json([tag='VariableArgument',contents=json([_variableName= ?x])]),json([tag='VariableArgument',contents=json([_variableName= $animal])])],_factId=isa])

%Cause
%json([tag='Given',contents=[]])

%ValenceReason
%json([tag='ByDefinition',contents=[]])

% FIXME: this is not complete: 2nd order things won't work
goodarguments(json([tag='VariableArgument',contents=json(['_variableName'= VarName])]), VarName).
goodarguments(json([tag='IdArgument',contents=VarName]), VarName).

%[json([tag='VariableArgument',contents=json([_variableName= ?x])]),json([tag='VariableArgument',contents=json([_variableName= $animal])])]

%goodfact(json([tag='Not','_negatedFact'=json([tag='Fact','_factArguments'=,_factId=Id])])
%goodfact(json([tag='Fact','_factArguments'=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,contents=json([_variableName= $activity2])])],'_factId'=Id]), -Fact) :-
    %goodfact(json([tag='Fact','_factArguments'=ArgumentsIn ,'_factId'=Id])

%goodfact(json([tag='Not','_negatedFact'=json([tag='Fact','_factArguments'=[json([tag='VariableArgument',contents=json(['_variableName'= ?x])]),json([tag='VariableArgument',contents=json(['_variableName'= $activity2])])],'_factId'=capableof])]), -Fact) :-
goodfact(json([tag='Not','_negatedFact'=Ficty]), -Fact) :-
    goodfact(Ficty, Fact).

%goodfact(json([tag='Fact','_factArguments'=ArgumentsIn ,'_factId'=Id]), Fact) :-
goodfact(json([tag='Fact','_factArguments'=ArgumentsIn ,'_factId'=Id]), Fact) :-
    maplist(goodarguments, ArgumentsIn, Args),
    Fact =.. [Id | Args].

goodcause(json([tag='Given',contents=[]]), given).
goodcause(json([tag='InferredBy',contents=[What]]), inferredby(What)).
goodcause(json([tag='CausedBy',contents=[What]]), inferredby(What)).
goodvalencereason(json([tag='ByDefinition',contents=[]]), bydefinition).
goodvalencereason(json([tag='Assumed',contents=[]]), assumed).
goodvalencereason(json([tag='ValenceInferredBy',contents=[What]]), inferredby(What)).

goodnp((NP, Type, Id, FactIn, CauseIn, Valence, ValenceReasonIn), (NP, Type, Id, Fact, Cause, Valence, ValenceReason)) :-
    goodfact(FactIn, Fact),
    goodcause(CauseIn, Cause),
    goodvalencereason(ValenceReasonIn, ValenceReason).

%domainize((D, V)) :-
    %maplist(dom(D), V).

%dom(K, D, V) :-  % true.
    %assert(created_domain(K, D, V)).

%factize((_ , _, _, Fact, _, _, _), Fact).

parsemn(json(['_elements'=JSON]), MN) :-
    %findall((VN, L), member(json([tag='GlobalVariable',contents=[json(['_variableName'= VN]),L]]), JSON), GVS),
    %writeln(GVS),
    %maplist(domainize, GVS),
    findall((NP, Type, Id, Fact, Cause, Valence, ValenceReason), member( json([tag='NarrativeStatement','_statementValenceReason'=ValenceReason,'_statementCause'=Cause,'_statementValence'=Valence,'_statementId'=Id,'_statementType'=Type,'_statementFact'=Fact,'_narrativePoint'=NP]), JSON), NPS),
    maplist(goodnp, NPS, NPSGOOD),
    %aggregate(set(NP), member(NP, NPS), OnlyNPs),
    findall(NPu, member((NPu, _, _, _, _, _, _), NPS), OnlyNPs1),
    list_to_set(OnlyNPs1, OnlyNPs),
    maplist(takefacts(NPSGOOD), OnlyNPs, MN).
    %maplist(factize, MNAll, MN).

takefacts(NPS, NP, List) :-
    findall(F, member((NP, _, _, F, _, _, _), NPS), List).

one(WV, MN, St, S, C) :-
    phrase(story(WV, MN, [], [], St, C), S).

printst(S) :-
    flatten(S, Sf),
    maplist(writeln, Sf),
    writeln('-----------------------------').

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [V | R], [V | O]) :-
    N2 is N - 1,
    take(N2, R, O).

all(WV, MN, Alls) :-
    %findall((S, St), one(MN, St, S), AllsAlmost),
    %findall((S, C), (one(WV, MN, _, S, C), writeln(S)), AllsAlmost),
    %findall((S, C), (one(WV, MN, _, S, C), retract_all), AllsAlmost),
    findall((S, C), (one(WV, MN, _, S, C)), AllsAlmost),
    list_to_set(AllsAlmost, Alls).
    %length(Alls, L),
    %write('you got '), write(L), write(' stories'),
    %take(10, Alls, Printable),
    %maplist(printst, Printable),
    %retractall(created_domain).

getjson(stdin, JSON) :-
    json_read(current_input, JSON).

getjson(file, JSON) :-
    open('output.json', read, In),
    %read(In, S),
    %writeln(S),
    json_read(In, JSON),
    close(In).

generate_test :-
    %getjson(stdin, JSON),
    getjson(file, JSON),
    JSON = json(['_elements'=Out1]),
    member(json([tag='Rules', contents=Out]), Out1),
    tmp_file_stream(text, File, Tmp),
    write(Tmp, Out),
    close(Tmp),
    consult(File),
    delete_file(File),
    parsemn(JSON, MN), !,
    WV = disney,
    all(WV, MN, StoriesA),
    maplist(lispifystory, StoriesA, Stories),
    maplist(dejsonfy, Stories, StoriesIvan),
    maplist(writeln, StoriesIvan).

generate_2 :-
    getjson(file, JSON),
    parsemn(JSON, MN), !,
    WV = disney,
    all(WV, MN, StoriesA),
    maplist(lispifystory, StoriesA, Stories),
    maplist(dejsonfy, Stories, StoriesIvan),
    maplist(writeln, StoriesIvan).


dejsonfy(json(['_story'=S, '_plausibility'=P]), (S, P)).

    %writeln(json(['_elements'=Out1, 'stories'=Stories])).
    %json_write(current_output, json(['_elements'=Out1, 'stories'=Stories])).
    %mn(MN),
    %generate(MN).

generate :-
    getjson(stdin, JSON),
    %getjson(file, JSON),
    JSON = json(['_elements'=Out1]),
    member(json([tag='Rules', contents=Out]), Out1),
    tmp_file_stream(text, File, Tmp),
    write(Tmp, Out),
    close(Tmp),
    consult(File),
    delete_file(File),
    parsemn(JSON, MN), !,
    WV = disney,
    all(WV, MN, StoriesA),
    maplist(lispifystory, StoriesA, Stories),

    %writeln(json(['_elements'=Out1, 'stories'=Stories])).
    json_write(current_output, json(['_elements'=Out1, 'stories'=Stories])).
    %mn(MN),
    %generate(MN).

lispifystory((Story, C), json(['_story'=Story2, '_plausibility'=C])) :-
    maplist(lispify, Story, Story2).

%lispifynp(Story, Story2) :-
    %maplist(lispify, Story, Story2).

%generate(MN) :-
    %one(MN, St, Y),
    %write('final state: '), writeln(St),
    %write('story: '), writeln(Y),
    %maplist(lispify, Y, P),
    %json_write(current_output, P).

applyfact(WV, Ev, (St, C), (St2, C2)) :-
    (
        (consequence(WV, Ev, Ef, Confidence2), !)
    ;
        (Ef = [], Confidence2 = 1.0)
    ),
    append(St, Ef, St2),
    C2 is C * Confidence2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% story(WV, MN, MNO, St, StOut, C) -->
%     beginning(WV, MN, MNB, St, StB, C1),
%     development(WV, MNB, MND, StB, StD, C2),
%     outcome(WV, MND, MNO, StD, StOut, C3),
%     { C is C1 * C2 * C3 }.
%
% beginning(WV, [NP | MN], MN, St, St2, C) --> NP, {foldl(applyfact(WV), NP, (St, 1.0), (St2, C))}.
% beginning(_, MN, MN, St, St, 1.0) --> [].
% %beginning(MN, MNO, St, StO) -->
%     %[],
%     %beginning(MN, MNO, St, StO).
%
% development(_, [NP | MN], MN, St, St, 1.0) --> NP.
% development(WV, MN, MNO, St, StOut, C) -->
%     {step(WV, St, StMid, Ev, C1)}, [Ev],
%     development(WV, MN, MNO, StMid, StOut, C2),
%     {C is C1 * C2}.
%
% outcome(_, [NP | MN], MN, St, St, 1.0) --> NP.
% outcome(_, MN, MN, St, St, 1.0) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


step(WV, St, StOut, Ev, Confidence) :-
    possible(WV, St, Ev, Confidence1),
    consequence(WV, Ev, Ef, Confidence2),
    Confidence is Confidence1 * Confidence2,
    clean(St, Ef, StMid),
    %append(St, [Ev], StMid),
    append(StMid, Ef, StOut).

clean(St, [], St) :- !.
clean(St, [-X | R], StOut) :-
    !,
    member(X, St),
    delete(St, X, StMid),
    clean(StMid, R, StOut).
clean(St, [_ | R], StOut) :-
    clean(St, R, StOut).

lispify(A, A) :-
    atom(A), !.
lispify(-A, [not, B]) :-
    !,
    lispify(A, B).
lispify(A, B) :-
    A =.. O,
    maplist(lispify, O, B).

%wv(disney, St, shoot(X, Y), [dead(Y), -alive(Y)]) :-
   %member(alive(X), St),
   %member(alive(Y), St),
   %X \== Y.

%%wv(papa, _, learnto(c, d), [capableof(a, b)]) :- True.

%wv(disney, St, learnto(X, Y), [capableof(X, Y)]) :-
    %domain(animal, X),
    %(domain(activity2, Y); domain(activity1, Y)),
    %\+member(capableof(X, Y), St).

%% global variables
%domain(animal, dog).
%domain(animal, cat).
%domain(activity1, run).
%domain(activity2, ride_horse).
%domain(posword, fun).
%domain(posword, joy).
%domain(negword, old).

%domain(_, _).

%wv(disney, St, shoot(X, Y), [dead(Y), -alive(Y)]) :-
   %member(alive(X), St),
   %member(alive(Y), St),
   %X \== Y.

%wv(disney, St, learnto(X, Y), [capableof(X, Y)]) :-
    %domain(animal, X),
    %(domain(activity2, Y); domain(activity1, Y)),
    %\+member(capableof(X, Y), St).

%% global variables
%domain(animal, dog).
%domain(animal, cat).
%domain(activity1, run).
%domain(activity2, ride_horse).
%domain(posword, fun).
%domain(posword, joy).
%domain(negword, old).

%>
%
%
%
%
%json([_elements=[json([tag=GlobalVariable,contents=[json([_variableName= $animal]),[dog,cat]]]),json([tag=GlobalVariable,contents=[json([_variableN
%ame= $negword]),[old]]]),json([tag=GlobalVariable,contents=[json([_variableName= $posword]),[fun,joy]]]),json([tag=GlobalVariable,contents=[json([_var
%iableName= $activity1]),[run]]]),json([tag=GlobalVariable,contents=[json([_variableName= $activity2]),[ride_horse]]]),json([tag=NarrativeStatement,_st
%atementValenceReason=json([tag=ByDefinition,contents=[]]),_statementCause=json([tag=Given,contents=[]]),_statementValence=0.0,_statementId=id1,_statem
%entType=CH,_statementFact=json([tag=Fact,_factArguments=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,co
%ntents=json([_variableName= $animal])])],_factId=isa]),_narrativePoint=np1]),json([tag=NarrativeStatement,_statementValenceReason=json([tag=Assumed,co
%ntents=[]]),_statementCause=json([tag=InferredBy,contents=[id5]]),_statementValence=1,_statementId=id2,_statementType=CH,_statementFact=json([tag=Fact
%,_factArguments=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,contents=json([_variableName= $activity1])
%]),json([tag=VariableArgument,contents=json([_variableName= $posword])])],_factId=doesfor]),_narrativePoint=np1]),json([tag=NarrativeStatement,_statem
%entValenceReason=json([tag=Assumed,contents=[]]),_statementCause=json([tag=InferredBy,contents=[id6]]),_statementValence=0.0,_statementId=id3,_stateme
%ntType=CH,_statementFact=json([tag=Not,_negatedFact=json([tag=Fact,_factArguments=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),jso
%n([tag=VariableArgument,contents=json([_variableName= $activity2])])],_factId=capableof])]),_narrativePoint=np1]),json([tag=NarrativeStatement,_statem
%entValenceReason=json([tag=Assumed,contents=[]]),_statementCause=json([tag=Given,contents=[]]),_statementValence= -1,_statementId=id4,_statementType=C
%H,_statementFact=json([tag=Fact,_factArguments=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,contents=js
%on([_variableName= $negword])])],_factId=hasproperty]),_narrativePoint=np2]),json([tag=NarrativeStatement,_statementValenceReason=json([tag=Assumed,co
%ntents=[]]),_statementCause=json([tag=Given,contents=[]]),_statementValence= -1,_statementId=id5,_statementType=CH,_statementFact=json([tag=Not,_negat
%edFact=json([tag=Fact,_factArguments=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,contents=json([_varia
%bleName= $activity1])])],_factId=capableof])]),_narrativePoint=np2]),json([tag=NarrativeStatement,_statementValenceReason=json([tag=ValenceInferredBy,
%contents=[id7]]),_statementCause=json([tag=Given,contents=[]]),_statementValence=1,_statementId=id6,_statementType=EV,_statementFact=json([tag=Fact,_f
%actArguments=[json([tag=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,contents=json([_variableName= $activity2])])]
%,_factId=learnto]),_narrativePoint=np3]),json([tag=NarrativeStatement,_statementValenceReason=json([tag=ValenceInferredBy,contents=[id8]]),_statementC
%ause=json([tag=CausedBy,contents=[id6]]),_statementValence=1,_statementId=id7,_statementType=CH,_statementFact=json([tag=Fact,_factArguments=[json([ta
%g=VariableArgument,contents=json([_variableName= ?x])]),json([tag=VariableArgument,contents=json([_variableName= $activity2])])],_factId=capableof]),_
%narrativePoint=np3]),json([tag=NarrativeStatement,_statementValenceReason=json([tag=Assumed,contents=[]]),_statementCause=json([tag=InferredBy,content
%s=[id2]]),_statementValence=1,_statementId=id8,_statementType=CH,_statementFact=json([tag=Fact,_factArguments=[json([tag=VariableArgument,contents=jso
%n([_variableName= ?x])]),json([tag=VariableArgument,contents=json([_variableName= $activity2])]),json([tag=VariableArgument,contents=json([_variableNa
%me= $posword])])],_factId=doesfor]),_narrativePoint=np3]),json([tag=Schema,_schemaId=np1,_schemaType=Setting]),json([tag=Schema,_schemaId=np2,_schemaT
%ype=Conflict]),json([tag=Schema,_schemaId=np3,_schemaType=Resolution]),json([tag=Rules,contents=%wv(disney, St, learnto(X, Y), [capableof(X, Y)]) :-
