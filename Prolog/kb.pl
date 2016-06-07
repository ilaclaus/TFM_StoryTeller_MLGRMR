%<
:- module(kb, [frase_trivial/1, consequence/4, possible/4, happen/5,
                introduce_main_characters/2, introduce_secondary_characters/3,
                retract_all/0, retract_main/0, fetch_actions/2, choose_one_action/3]).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(date)).
:- use_module(library(apply)).

:- dynamic main_character/1.


/* Obtener acciones del servicio Cambridge */

/*
  ACTION: la acción a realizar
  SUBJECT_TYPE: empty/protagonist/something/text
  SUBJECT_TEXT: texto
  OBJECT_TYPE: empty/protagonist/something/text
  OBJECT_TEXT: texto
  PREPOSITION: texto
  IO_TYPE: empty/protagonist/text
  IO_TEXT: texto
  PROTAGONIST: texto

  Ej: "dog eats itself" -> doform(eats, protagonist, '', protagonist, '', '', empty, '', dog).
*/

fetch_actions_from_ws(A, STY, STXT, OTY, OTXT, PRP, IOTY, IOTXT, PROT, Js) :-
              string_concat('http://ccg02.doc.gold.ac.uk/eventchains3/entity_narrative/api/query/word2vec/gigaword-300-full-with-args-trans-adj/?action=', A, P1),
              string_concat(P1, '&subject_type=', P2),
              string_concat(P2, STY, P3),
              string_concat(P3, '&subject_text=', P4),
              string_concat(P4, STXT, P5),
              string_concat(P5, '&object_type=', P6),
              string_concat(P6, OTY, P7),
              string_concat(P7, '&object_text=', P8),
              string_concat(P8, OTXT, P9),
              string_concat(P9, '&preposition=', P10),
              string_concat(P10, PRP, P11),
              string_concat(P11, '&indirect_object_type=', P12),
              string_concat(P12, IOTY, P13),
              string_concat(P13, '&indirect_object_text=', P14),
              string_concat(P14, IOTXT, P15),
              string_concat(P15, '&limit=10&protagonist=', P16),
              string_concat(P16, PROT, P17),
              string_concat(P17, '&unique_predicates=0', URL),
              http_open(URL,In, []),
              json_read(In, Js),
              close(In).

/* TODO: sacar la acción pasada como argumento: action(S, O) */
fetch_actions(Ev, AcJSON) :- parse_action(Ev, Ac, Sj, Ob),
        fetch_actions_from_ws(Ac, protagonist, '', text, Ob, '', empty, '', Sj, AcJSON).

parse_action(Ev, Ac, Sj, Ob) :- format(atom(EvAtom), "~w", Ev), split_string(EvAtom, "(,", "() ", [A, S, O]),
                                atom_codes(Ac, A), atom_codes(Sj, S), atom_codes(Ob, O).

%Cada vez devolverá una de las acciones, haciendo que las posibilidades de la gramática aumenten exponencialmente.
choose_one_action(St, [Js | _], Ac) :- get_action(St, Js, Ac).
choose_one_action(St, [_ | JSR], Ac) :- choose_one_action(St, JSR, Ac).


get_action(St, json([score = Q | R]), Ac) :- A1 = score(Q), get_action(St, json(R), A2), Ac = [A1, A2].
get_action(St, json([event = Q | _]), Ac) :- substitute_characters(St, Q, ResInt), substitute_spaces(ResInt, Res),
                                              Ac = event(Res).
get_action(St, json([P = _ | R]), Ac) :- P \== 'score', P \== 'event', get_action(St, json(R), Ac).


/* Sustituye los parámetros de la acción: P = horse, "___ rides X" -> "cat rides horse" */

substitute_characters(St, Ac, Result) :- substitute_main_character(Ac, MidR),
                                              substitute_secondary_character(St, MidR, Result).

substitute_main_character(Ac, Result) :- main_character(MC),
                                                  atomic_list_concat(Words, 'X', Ac),
                                                  atomic_list_concat(Words, MC, Result).

substitute_secondary_character(St, Ac, Result) :- get_secondary_character(St, SC),
                                                      atomic_list_concat(Words, '___', Ac),
                                                      atomic_list_concat(Words, SC, Result).

substitute_secondary_character(_, Ac, Result) :- main_character(SC),
                                                      atomic_list_concat(Words, '___', Ac),
                                                      atomic_list_concat(Words, SC, Result).

substitute_spaces(Ac, Result) :- atomic_list_concat(Words, ' ', Ac), atomic_list_concat(Words, '_', Result).

/* Frase trivial */
/* Genera frases sobre el tiempo, el día/hora, el cima... */

frase_trivial(T) :- calendar_sentence(T).
frase_trivial(T) :- climate_sentence(T).
%frase_trivial(T) :- calendar_sentence(Cal), climate_sentence(Cl), append(Cal, Cl, T).



calendar_sentence(F) :- get_time(T), stamp_date_time(T, D, local), calendar(D, F).

calendar(Date, [date(Yr, Mth, Day)]) :- mn_date(Date, Yr, Mth, Day).
calendar(Date, [time(Hr, Mn)]) :- mn_time(Date, Hr, Mn).
calendar(Date, [date(Yr, Mth, Day), time(Hr, Mn)]) :- mn_date(Date, Yr, Mth, Day), mn_time(Date, Hr, Mn).

mn_date(Date, Yr, Mth, Day) :- date_time_value(date, Date, date(Y, M, D)), atom_number(Yr, Y),
                    atom_number(Mth, M), atom_number(Day, D).

mn_time(Date, Hr, Mn) :- date_time_value(time, Date, time(H, M, _)), atom_number(Hr, H), atom_number(Mn, M).




climate_sentence(F) :- sky(S), wind(W), temp(T), append(S, W, Int), append(Int, T, F).

sky([clear(sky)]).
sky([cloudy(sky)]).
sky([rainy(sky)]).

wind([windy(wind)]).
wind([-windy(wind)]).

temp([cold(temperature)]).
temp([hot(temperature)]).
temp([warm(temperature)]).
temp([nice(temperature)]).

/* No utiliza los possible/consequence de este fichero (overide en output.json) */


possible(disney, St, shoot(X, Y), 1.0) :-
   member(alive(X), St),
   member(alive(Y), St),
   X \== Y.

consequence(disney, shoot(_, Y), [dead(Y), -alive(Y)], 1.0).


possible(disney, St, learnto(X, Y), C) :-
    domain(disney, St, animal, X, C1),
    domain(disney, St, activity, Y, C2),
    C is C1 * C2,    \+member(capableof(X, Y), St).

consequence(disney, learnto(X, Y), [capableof(X, Y)], 1.0).


possible(disney, St, eat(X, Y), 1.0) :-
   member(alive(X), St),
   member(human(X), St),
   member(food(Y), St).

consequence(disney, eat(X, Y), [-hungry(X), -edible(Y)], 1.0).

consequence(disney, isa(X, animal), [capableof(X, talk)], 0.5).

/* Hechos espontaneos */
% [isa(dog,animal),doesfor(dog,run,fun),-capableof(dog,ride_horse)]

/* Idea: en cualquier momento puede ocurrir algo (p.e.: lluvia), la historia debe mostrar las consecuencias
(p.e.: tal personaje usa el paraguas)
[startsto, rain], [use(dog, umbrella)], [wet(horse)]. */

% Devuelve el efecto de lo que ocurre (para todos los personajes de los NP).
happen(WV, F, St, NP, C) :- spontaneous_fact(WV, H, C), append(St, NP, L), apply(H, L, Ef), F = [H | Ef].

apply(H, NP, Ef) :- all_characters(Pers, NP), maplist(spontaneous_effect(H), Pers, Ef).

/* Obtiene los personajes de los narrative points (TODO: Hacerlo con todos los personajes que aparecen: personajes
   introducidos previamente) */

all_characters(Pers, NP) :- findall(P, character(P, NP), Pers).

% TODO: Hacer personajes dependientes del dominio (p.e.: disney = animal, other = human, pixar = toy...)

character(Pers, [P | NP]) :- domain(_, _, animal, Pers, _), is_character(Pers, [P | NP]).
is_character(Pers, [P | NP]) :- (has_character(Pers, P), !) ; character(Pers, NP).
has_character(Pers, SNP) :- format(atom(A), "~w", SNP), sub_string(A, _, _, _, Pers).

/* Presentación de personajes */

introduce_secondary_characters(St, NP, F) :-
                      all_characters(C, NP), secondary_unintroduced_characters(St, C, Pers),
                      secondary_character_frase(St, Pers, F).


secondary_character_frase(_, [], []).
secondary_character_frase(St, [P | RP], F) :-
                  (secondary_character_frase(St, RP, Q), \+is_introduced(St, P), establish_relation(P, R),
                  F = [secondary_character(P) , R | Q]) ;
                  (secondary_character_frase(St, RP, Q), \+is_introduced(St, P), establish_relation(P, R, S),
                  F = [secondary_character(P) , R , S | Q]) ;
                  is_introduced(St, P), secondary_character_frase(St, RP, F).


secondary_unintroduced_characters(_, [], []).
secondary_unintroduced_characters(St, [P | RP], Pers) :-
                  (\+is_introduced(St, P), secondary_unintroduced_characters(St, RP, Q), Pers = [P | Q], !) ;
                  secondary_unintroduced_characters(St, RP, Pers).

get_secondary_character(St, P) :- all_characters(Pers, St), get_one(Pers, P), \+ main_character(P).

get_one([P | _], Personaje) :- Personaje = P.
get_one([_ | RP], Personaje) :- get_one(RP, Personaje).

/* TODO: Filtrar palabras con substrings de personaje dentro (p.e.: cat, stacatto) */
is_introduced([F | St], P) :- (format(atom(A), "~w", F), sub_string(A, _, _, _, P), !) ; is_introduced(St, P).



/* PROBLEMA: Al hacer backtrack no pone los personajes secundarios (ya se han introducido!!!) */

/* Introducción de personajes */
/*
introduce_characters(NP, F) :- all_characters(Pers, NP), introduce_all(Pers, F). %, character_frase(Pers, F).

introduce_all([], []).
introduce_all([P | PR], F) :- (\+ secondary_character(P), \+ main_character(P),
                            assert(secondary_character(P)), introduce_all(PR, Q), establish_relation(P, R),
                            F = [secondary_character(P) , R | Q]) ; introduce_all(PR, F).
*/

%introduce_all([_ | PR], F) :- introduce_all(PR, F).


introduce_main_characters(NP, F) :- all_characters(Pers, NP), introduce_all_main(Pers), main_character_frase(Pers, F).
introduce_all_main([]).
introduce_all_main([P | PR]) :- (\+ main_character(P), assert(main_character(P)),
                              introduce_all_main(PR), !) ; introduce_all_main(PR).


%introduce_main_characters(NP, F) :- all_characters(Pers, NP), main_character_frase(Pers, F).

main_character_frase([], []).
main_character_frase([P | RP], F) :- main_character_frase(RP, Q), F = [main_character(P) | Q].


%introduce_main_characters(NP, F) :- all_characters(Pers, NP), introduce_all_main(Pers, F).

/*
introduce_all_main([], []).
introduce_all_main([P | PR], F) :- (\+ secondary_character(P), \+ main_character(P),
                            assert(main_character(P)), introduce_all_main(PR, Q),
                            F = [main_character(P) | Q], !) ; introduce_all_main(PR, F).
*/

%introduce_all_main([_ | PR], F) :- introduce_all_main(PR, F).


/*
are_characters_introduced(NP) :- all_characters(Pers, NP), are_all_introduced(Pers).
are_all_introduced([]).
are_all_introduced([P | PR]) :- ((secondary_character(P), !) ; main_character(P)), are_all_introduced(PR).
*/

/* Eliminar personajes introducidos */

retract_all_characters([]).
retract_all_characters([P | NP]) :- main_character(P), retract(main_character(P)), retract_all_characters(NP).

retract_main :- findall(P, main_character(P), M), retract_main_characters(M).


retract_main_characters([]).
retract_main_characters([P | RP]) :- main_character(P), retract(main_character(P)), retract_main_characters(RP).

all_introduced(Pers) :- findall(P, main_character(P), Pers).

retract_all :- all_introduced(Pers), retract_all_characters(Pers).



spontaneous_fact(_, start(rain), 0.5).
spontaneous_fact(_, start(fight), 0.4).

spontaneous_fact(disney, sing(people), 0.3).

spontaneous_effect(start(rain), P, use(P, umbrella)).
spontaneous_effect(start(rain), P, get(P, wet)).

spontaneous_effect(sing(people), P, sing(P, aloud)).
spontaneous_effect(sing(people), P, sing(P, happily)).
spontaneous_effect(sing(people), P, sing(P, sadly)).

spontaneous_effect(start(fight), P, get(P, angry)).
spontaneous_effect(start(fight), P, get(P, scared)).
spontaneous_effect(start(fight), P, preparefor(P, fight)).

/* Relaciones (con otros personajes o entre personajes) */
/* Idea: los personajes se relacionan entre sí (hermanos/amigos/enemigos), con consecuencias
   parent(P1, P2) -> love(P1, P2)
   Idea: propiedades de los personajes (male, female, single, married)
   Idea: introducir personajes para establecer una relación, en lugar de hacerlo aleatoriamente
   Idea: start(fight) -> enemy(P1, P2), single(P) -> looksfor(lover) */


establish_relation(P2, F) :- main_character(P1), P1 \= P2, relation_fact(P1, P2, F).
establish_relation(P2, F1, F2) :- main_character(P1), P1 \= P2, relation_fact(P1, P2, F1),
                              relation_effect(F1, F2).


relation_fact(P1, P2, brother(P1, P2)).
relation_fact(P1, P2, friend(P1, P2)).
relation_fact(P1, P2, enemy(P1, P2)).
relation_fact(P1, P2, parent(P1, P2)).

relation_effect(brother(P1, P2), love(P1, P2)).
relation_effect(brother(P1, P2), hate(P1, P2)).
relation_effect(friend(P1, P2), hangout(P1, P2)).
relation_effect(enemy(P1, P2), hate(P1, P2)).
relation_effect(parent(P1, P2), love(P1, P2)).




%domain(disney, animal, dog).
%domain(kafka, animal, dog).
domain(_, _, animal, dog, 1.0).
domain(_, _, animal, cat, 1.0).
domain(_, _, animal, horse, 1.0).
domain(_, _, activity, run, 1.0).
domain(_, _, activity, ride_horse, 1.0).
domain(_, _, posword, fun, 1.0).
domain(_, _, posword, joy, 1.0).
domain(_, _, negword, old, 1.0).


% ontology

domain(D, St, alive, X, V) :- domain(D, St, animal, X, V).
domain(D, St, animal, X, V) :- domain(D, St, human, X, V).

%>
