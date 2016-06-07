:- module(grammar, [story/8]).

:- use_module(kb).



story(WV, MN, MNO, St, StOut, C) -->
    restart,
    beginning(WV, MN, MNB, St, StB, C1),
    development(WV, MNB, MND, StB, StD, C2),
    outcome(WV, MND, MNO, StD, StOut, C3),
    { C is C1 * C2 * C3 }.


/* Beggining -------------------------------------------- */

beginning(WV, [NP | MN], MN, St, StOut, C) --> NP, {foldl(applyfact(WV), NP, (St, 1.0), (StM, C)),
                                              introduce_main_characters(NP, CF)}, CF, {append(StM, CF, StOut)}.

/* La llamada al servicio alarga demasiado la espera cuando hay muchas posibilidades. */

beginning(WV, [NP | MN], MN, St, StOut, C) -->
                                {frase_trivial(F)}, F, NP, {foldl(applyfact(WV), NP, (St, 1.0), (StMid, C)),
                                introduce_main_characters(NP, CF)}, CF, {append(F, CF, Mid), append(Mid, StMid, StOut)}.


%beginning(_, [NP | MN], MN, St, St, StoryI, 1.0) --> {introduce_main_characters(NP, CF)}, {append(NP, CF, StoryI)}.



/* Development -------------------------------------------- */

development(_, [NP | MN], MN, St, StOut, 1.0) -->
                      NP, {introduce_secondary_characters(St, NP, CF), append(St, CF, StOut)}.


development(WV, [NP | MN], MN, St, StOut, C) -->
                      NP, {happen(WV, Ef, St, NP, C), introduce_secondary_characters(St, NP, CF)},
                      Ef, {append(St, Ef, Mid), append(Mid, CF, StOut)}.


/* TODO: Permitir que varias acciones se produzcan seguidas */
/*
  TODO: cómo rellenar los parámetros de la acción en cada momento
  - Posibilidad: Tener acciones en step, y utilizar el servicio para sacar las
    acciones consecutivas.
  - Posibilidad: Una regla para cada tipo de acción

*/

development(WV, [NP | MN], MN, St, StOut, C) -->
    NP, {step(WV, St, StMid, Ev1, C1), fetch_actions(Ev1, AcJSON), choose_one_action(StMid, AcJSON, [score(S), Ev2]),
    introduce_secondary_characters(St, [Ev1, Ev2], CF)}, [Ev1], [Ev2], CF,
    {append(St, [Ev1], Mid), append(Mid, [Ev2], Mid2), append(Mid2, CF, StOut)},
    {C is C1 * S}.

/*
development(WV, [NP | MN], MNO, St, StOut, C) -->
    NP, {step(WV, St, StMid, Ev, C1)}, [Ev],
    development(WV, MN, MNO, StMid, StOut, C2),
    {C is C1 * C2}.
*/

/* Outcome -------------------------------------------- */

outcome(_, [NP | MN], MN, St, St, 1.0) --> NP.
%outcome(_, MN, MN, St, St, 1.0) --> [].

restart --> {retract_all}.

%<
