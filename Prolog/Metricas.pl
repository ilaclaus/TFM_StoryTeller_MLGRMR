/* GENERA LOS CONJUNTOS PERTINENTES */
/* ---------------------------------------------------------------------------- */
generateSets(FileName) :- setFeatures(FileName), generateTrainSet(FileName, 10), bowizeSet(FileName).
/* ---------------------------------------------------------------------------- */
/* CALCULA LAS FEATURES DE UN CONJUNTO DE HISTORIAS (researchgate.net: para referencias) */
/* Se proporciona el nombre del archivo de entrada sin la extensión (debe ser .txt) */
/* ---------------------------------------------------------------------------- */
setFeatures(FileName) :-
    prologize(FileName, TmpFile),
    readf(TmpFile, Stories),
    compute_features(Stories, F),
    writef_ARFF(FileName, F),
    delete_file(TmpFile).
/* ---------------------------------------------------------------------------- */
/* GENERA UN CONJUNTO DE ENTRENAMIENTO DE N ELEMENTOS */
/* A partir de este conjunto, obtenemos también el conjunto de test */
/* ---------------------------------------------------------------------------- */
generateTrainSet(FileName, N) :-
    prologize(FileName, TmpFile),
    readf(TmpFile, Stories),
    takeEndOfFile(Stories, SEnd),
    random_permutation(SEnd, PermSEnd), !,
    getFirstNElements(PermSEnd, N, TrainSt),
    atom_concat(FileName, '_train_set.txt', File),
    open(File, write, Str),
    maplist(writeln(Str), TrainSt),
    close(Str),
    delete_file(TmpFile).

bowizeSet(FileName) :-
    prologize(FileName, TmpFile),
    readf(TmpFile, Stories),
    write_bow_ARFF(FileName, Stories),
    delete_file(TmpFile).

write_bow_ARFF(FileName, Stories) :-
    atom_concat(FileName, '_bow.arff', FeatureFile),
    open(FeatureFile, write, Stream),
    write(Stream, '@RELATION stories'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE story STRING'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE pr NUMERIC'),
    nl(Stream),
    write(Stream, '@DATA'),
    nl(Stream),
    write_bow_stories(Stream, Stories),
    close(Stream).

/* ¡¡¡¡¡¡¡¡¡¡¡¡Hay que cambiar que las comillas acaben antes de la probabilidad de la historia!!!!!!!!!!! */
write_bow_stories(_, [end_of_file]).
write_bow_stories(Stream, [(S, P) | RS]) :-
    write(Stream, '\"'),
    bow_story(Stream, (S, P)),
    write(Stream, '\",'),
    write(Stream, P),
    nl(Stream),
    write_bow_stories(Stream, RS).

bow_story(Stream, (S, _)) :- flatten2(S, FS), write_list(Stream, FS).

write_list(Stream, [F]) :- write(Stream, F).
write_list(Stream, [F | RF]) :-
    write(Stream, F),
    write(Stream, ','),
    write_list(Stream, RF).

flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

/* ---------------------------------------------------------------------------- */
/* GENERA TEXTO PARA LAS HISTORIAS */
/* ---------------------------------------------------------------------------- */
textualizeStories(FileName) :-
    prologize(FileName, TmpFile),
    readf(TmpFile, Stories),
    takeEndOfFile(Stories, SEnd),
    stories2text(SEnd, TxtSt), !,
    atom_concat(FileName, '_textualized.txt', File),
    open(File, write, Str),
    maplist(writeln(Str), TxtSt),
    close(Str),
    delete_file(TmpFile).
/* ---------------------------------------------------------------------------- */
/* PLANTILLAS */
/* ---------------------------------------------------------------------------- */
stories2text([(St, _)], [Txt]) :- convert_to_text(St, Txt).
stories2text([(St, _) | RS], Frs) :- convert_to_text(St, Txt), stories2text(RS, RFStr), append([Txt], RFStr, Frs).

convert_to_text([F], Txt) :- textualize(F, StTxt), atom_concat(StTxt, 'The end.', Txt).
convert_to_text([[X, sky], [Y, wind], [Z, temperature] | RF], Frs) :-
                                  text_trivial_phrase([[X, sky], [Y, wind], [Z, temperature]], Txt),
                                  convert_to_text(RF, RFrs),
                                  atom_concat(Txt, '. ', Int), atom_concat(Int, RFrs, Frs), !.
convert_to_text([[X, sky], [not, [Y, wind]], [Z, temperature] | RF], Frs) :-
                                  text_trivial_phrase([[X, sky], [not, [Y, wind]], [Z, temperature]], Txt),
                                  convert_to_text(RF, RFrs),
                                  atom_concat(Txt, '. ', Int), atom_concat(Int, RFrs, Frs), !.
convert_to_text([[main_character, X], [main_character, Y] | RF], Frs) :-
                      text_main_characters([[main_character, X], [main_character, Y]], Txt),
                      convert_to_text(RF, RFrs),
                      atom_concat(Txt, '. ', Int5), atom_concat(Int5, RFrs, Frs), !.

convert_to_text([[Rel, X, Y], [Eff, Z, W] | RF], Frs) :-
                      is_relation(Rel), is_effect(Eff),
                      textualize([Rel, X, Y], TxtR), textualize([Eff, Z, W], TxtEf),
                      atom_concat(TxtR, ', so ', Int), atom_concat(Int, TxtEf, Txt),
                      convert_to_text(RF, RFrs),
                      atom_concat(Txt, '. ', Int5), atom_concat(Int5, RFrs, Frs), !.

convert_to_text([[capableof, dog, ride_horse], [doesfor, dog, ride_horse, fun]], Frs) :-
                      textualize([capableof, dog, ride_horse], TxtP), textualize([doesfor, dog, ride_horse, fun], TxtEf),
                      downcase_atom(TxtEf, LCTxt),
                      atom_concat(TxtP, ', so, instead, ', Int), atom_concat(Int, LCTxt, Str),
                      atom_concat(Str, '. The end.', Frs), !.

convert_to_text([[hasproperty, dog, old], [not, [capableof, dog, run]] | RF], Frs) :-
                      textualize([hasproperty, dog, old], TxtP), textualize([not, [capableof, dog, run]], TxtEf),
                      atom_concat(TxtP, ', so he ', Int),
                      atom_concat('the dog ', TxtEfD, TxtEf),
                      atom_concat(Int, TxtEfD, Txt),
                      convert_to_text(RF, RFrs),
                      atom_concat(Txt, '. ', Int5), atom_concat(Int5, RFrs, Frs), !.

convert_to_text([[date | RD], [time | RT] | RF], Frs) :-
                      textualize([date | RD], TxtDate), textualize([time | RT], TxtTime),
                      downcase_atom(TxtTime, LCTime),
                      atom_concat(TxtDate, ', and ', Int), atom_concat(Int, LCTime, Txt),
                      convert_to_text(RF, RFrs),
                      atom_concat(Txt, '. ', Int5), atom_concat(Int5, RFrs, Frs), !.

convert_to_text([[isa | R], [doesfor, dog, run, fun] | RF], Frs) :-
                      textualize([isa | R], TxtDog), textualize([doesfor, dog, run, fun], TxtDoes),
                      atom_concat('The dog ', TxtAct, TxtDoes),
                      atom_concat(TxtDog, ', and he ', Int), atom_concat(Int, TxtAct, Txt),
                      convert_to_text(RF, RFrs),
                      atom_concat(Txt, '. ', Int5), atom_concat(Int5, RFrs, Frs), !.

convert_to_text([F | RF], Frs) :- textualize(F, Txt), convert_to_text(RF, RFrs),
                                  atom_concat(Txt, '. ', Int), atom_concat(Int, RFrs, Frs).


is_relation(brother).
is_relation(friend).
is_relation(enemy).
is_relation(parent).

is_effect(love).
is_effect(hate).
is_effect(hangout).

day(1, 'Monday').
day(2, 'Tuesday').
day(3, 'Wednesday').
day(4, 'Thursday').
day(5, 'Friday').
day(6, 'Saturday').
day(7, 'Sunday').

month(1, 'January').
month(2, 'February').
month(3, 'March').
month(4, 'April').
month(5, 'May').
month(6, 'June').
month(7, 'July').
month(8, 'August').
month(9, 'September').
month(10,'October').
month(11, 'November').
month(12, 'December').

text_trivial_phrase([Sky, Wind, Tmp], Txt) :-
    textualize(Sky, TxtSky),
    textualize(Wind, TxtW),
    textualize(Tmp, TxtTemp),
    atom_concat(TxtSky, ', ', Int1),
    atom_concat(Int1, TxtW, Int2),
    atom_concat(Int2, ' and ', Int3),
    atom_concat(Int3, TxtTemp, Txt).

text_main_characters([[main_character, X], [main_character, Y]], Txt) :-
    atom_concat('The ', X, Int),
    atom_concat(Int, ' and ', Int2),
    atom_concat(Int2, 'the ', Int3),
    atom_concat(Int3, Y, Int4),
    atom_concat(Int4, ' were the main characters', Txt).

textualize([isa, X, Y], N) :- atom_concat('The ', X, Int), atom_concat(Int, ' was an ', In), atom_concat(In, Y, N), !.
textualize([doesfor, X, Y, Z], N) :- atom_concat('The ', X, In), atom_concat(In, ' ', Inner),
                                     (translate(do, Y, T), atom_concat(Inner, T, In2) ; atom_concat(Inner, Y, Int), atom_concat(Int, 's', In2)),
                                     atom_concat(In2, ' for ', In3), atom_concat(In3, Z, N), !.

textualize([capableof, X, ride_horse], N) :- atom_concat('The ', X, Int),
                                   atom_concat(Int, ' was capable of ', In),
                                   atom_concat(In, 'riding a horse', N), !.

textualize([capableof, X, Y], N) :- atom_concat('The ', X, Int),
                                    atom_concat(Int, ' was capable of ', In),
                                    atom_concat(In, Y, N), !.

textualize([main_character, X], N) :- atom_concat('The ', X, Int),
                                      atom_concat(Int, ' was a main character', N), !.
textualize([hasproperty, dog, old], N) :- N = 'The dog was old', !.
textualize([hasproperty, X, Y], N) :- atom_concat(X, ' had the property of ', In), atom_concat(In, Y, N), !.

textualize([learnto, X, Y], N) :- atom_concat('The ', X, Int),
                                  atom_concat(Int, ' learned to ', In),
                                  (translate(learn, Y, T), atom_concat(In, T, N) ; atom_concat(In, Y, N)), !.
textualize([event, X], N) :- atom(X), atom_concat('The event ', X, In), atom_concat(In, ' occurred', N), !.
textualize([event, X], N) :- format(atom(A), "~w", X), atom_concat('The event ', A, In),
                             atom_concat(In, ' occurred', N), !.
textualize([secondary_character, X], N) :- atom_concat('The ', X, Int),
                                           atom_concat(Int, ' was a secondary character', N), !.
textualize([brother, X, Y], N) :- atom_concat('The ', Y, Int),
                                  atom_concat(Int, ' was ', In), atom_concat(In, X, In2),
                                  atom_concat(In2, 's brother', N), !.
textualize([friend, X, Y], N) :- atom_concat('The ', Y, Int),
                                 atom_concat(Int, ' was ', In), atom_concat(In, X, In2),
                                 atom_concat(In2, 's friend', N), !.
textualize([enemy, X, Y], N) :- atom_concat('The ', Y, Int),
                                atom_concat(Int, ' was ', In), atom_concat(In, X, In2),
                                atom_concat(In2, 's enemy', N), !.
textualize([parent, X, Y], N) :- atom_concat('The ', Y, Int),
                                 atom_concat(Int, ' was ', In), atom_concat(In, X, In2),
                                 atom_concat(In2, 's parent', N), !.
textualize([love, X, Y], N) :- atom_concat('the ', X, Int), atom_concat(Int, ' loved the ', In), atom_concat(In, Y, N), !.
textualize([hate, X, Y], N) :- atom_concat('the ', X, Int), atom_concat(Int, ' hated the ', In), atom_concat(In, Y, N), !.
textualize([hangout, X, Y], N) :- atom_concat('the ', X, Int),
                                  atom_concat(Int, ' hanged out with the ', In), atom_concat(In, Y, N), !.
textualize([start, rain], 'It stareds to rain').
textualize([start, fight], 'A fight started').
textualize([sing, people], 'Everybody started to sing').
textualize([get, X, Y], N) :- atom_concat('The ', X, Int), atom_concat(Int, ' got ', In), atom_concat(In, Y, N), !.
textualize([use, X, Y], N) :- atom_concat('The ', X, Int), atom_concat(Int, ' used ', In), atom_concat(In, Y, N), !.
textualize([sing, X, Y], N) :- atom_concat('The ', X, Int), atom_concat(Int, ' sang ', In), atom_concat(In, Y, N), !.
textualize([preparefor, X, Y], N) :- atom_concat('The ', X, Int), atom_concat(Int, ' prepared for ', In), atom_concat(In, Y, N), !.
textualize([X, sky], N) :- atom_concat('The sky was ', X, N), !.
textualize([X, wind], N) :- atom_concat('the wind was ', X, N), !.
textualize([X, temperature], N) :- atom_concat('the temperature was ', X, N), !.
textualize([date, X, Y, Z], N) :- day_of_the_week(date(X, Y, Z), DayNum),
                                  day(DayNum, Day),
                                  atom_concat('It was ', Day, Int1),
                                  atom_concat(Int1, ', the ', Int2),
                                  atom_concat(Int2, Z, Int3),
                                  atom_concat(Int3, ' of ', Int4),
                                  month(Y, Month),
                                  atom_concat(Int4, Month, Int5),
                                  atom_concat(Int5, ', ', Int6),
                                  atom_concat(Int6, X, N), !.

textualize([time, X, Y], N) :- atom_concat('The time was ', X, In),
                                atom_concat(In, ':', In3),
                                atom_concat(In3, Y, N), !.
/* ---------------------------------------------------------------------------- */
/* NOT */
/* ---------------------------------------------------------------------------- */
textualize([not, [X, wind]], N) :- atom_concat('the wind was not ', X, N), !.
textualize([not, [capableof, X, ride_horse]], N) :- atom_concat('The ', X, Int),
                                   atom_concat(Int, ' was not capable of ', In),
                                   atom_concat(In, 'riding a horse', N), !.
textualize([not, [capableof, X, Y]], N) :- atom_concat('the ', X, Int),
                                           atom_concat(Int, ' was not capable of ', In),
                                           (translate(capable, Y, T), atom_concat(In, T, N) ; atom_concat(In, Y, N)), !.


textualize([not, X], N) :- textualize(X, F), atom_concat('not ', F, N), !.
/* ---------------------------------------------------------------------------- */
translate(learn, ride_horse, 'ride a horse').
translate(capable, ride_horse, 'riding a horse').
translate(do, ride_horse, 'rode a horse').

translate(do, run, 'ran').
translate(capable, run, 'running').


getFirstNElements(Src, N, L) :- findall(E, (nth1(I,Src,E), I =< N), L).
/* ---------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------- */



takeEndOfFile([end_of_file], []).
takeEndOfFile([X | Y], L) :- takeEndOfFile(Y, L2), append([X], L2, L), !.

prologize(FileName, TmpFile) :-
    atom_concat(FileName, '.txt', File),
    open(File, read, Str),
    read_lines(Str, Lines),
    close(Str),
    tmp_file_stream(text, TmpFile, TmpStr),
    add_final_point(TmpStr, Lines),
    close(TmpStr).

read_lines(Str, []) :-
    at_end_of_stream(Str), !.

read_lines(Str, [Line | Ls]) :-
    \+ at_end_of_stream(Str),
    read_one_line(Str, Line),
    read_lines(Str, Ls).

read_one_line(Str, Line) :-
    read_line_to_codes(Str, C),
    atom_codes(Line, C).

add_final_point(_, []).
add_final_point(Str, [S | RS]) :-
    write(Str, S),
    write(Str, '.'),
    nl(Str),
    add_final_point(Str, RS).

readf(File, Stories) :-
     open(File, read, Str),
     read_stories(Str, Stories),
     close(Str).

read_stories(Stream,[]) :-
     at_end_of_stream(Stream).

read_stories(Stream,[Story | RestStories]) :-
     \+ at_end_of_stream(Stream),
     read(Stream, Story),
     read_stories(Stream, RestStories).


writef_CSV(FileName, F) :-
    atom_concat(FileName, '.txt', File),
    open(File, write, Stream),
    write(Stream, 'total_characters,main_characters,secondary_characters,ratio_characters,length,total_facts,ratio_characters_facts,evolution,main_character_actions,total_actions,ratio_actions,love,hate,drama,fun,plausibility,has_date,has_time,has_weather'),
    nl(Stream),
    write_stories(Stream, F),
    close(Stream).

writef_ARFF(FileName, F) :-
    atom_concat(FileName, '_features.arff', FeatureFile),
    open(FeatureFile, write, Stream),
    write(Stream, '@RELATION stories'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE total_characters  NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE main_characters   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE secondary_characters  NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE ratio_characters   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE length   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE total_facts   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE ratio_characters_facts   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE evolution   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE main_character_actions   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE total_actions   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE ratio_actions   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE love_level   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE hate_level   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE drama_level   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE fun_level   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE plausibility   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE has_date   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE has_time   NUMERIC'),
    nl(Stream),
    write(Stream, '@ATTRIBUTE has_weather   NUMERIC'),
    nl(Stream),
    write(Stream, '@DATA'),
    nl(Stream),
    write_stories(Stream, F),
    close(Stream).

write_stories(_, []).
write_stories(Stream, [S | RS]) :- write_story_features(Stream, S), write_stories(Stream, RS).

write_story_features(Stream, [F]) :- write(Stream, F), nl(Stream).
write_story_features(Stream, [F | RF]) :- write(Stream, F), write(Stream, ','), write_story_features(Stream, RF).


compute_features([end_of_file], []).
compute_features([S | R], [F | RF]) :- compute_story_features(S, F), compute_features(R, RF).

compute_story_features(S, [TC, NMC, NSC, RC, SL, NF, RCF, E, NMCA, NA, RA, LL, HL, DL, FL, PL, HD, HT, HW]) :-
    numberOfCharacters(S, TC),
    numberOfMainCharacters(S, NMC),
    numberOfSecondaryCharacters(S, NSC),
    ratioCharacters(S, RC),
    storyLength(S, SL),
    numberOfFacts(S, NF),
    ratioCharactersFacts(S, RCF),
    evolution(S, E),
    numberOfMainCharacterActions(S, NMCA),
    numberOfActions(S, NA),
    ratioActions(S, RA),
    loveLevel(S, LL),
    hateLevel(S, HL),
    dramaLevel(S, DL),
    funLevel(S, FL),
    plausibility(S, PL),
    hasDate(S, HD),
    hasTime(S, HT),
    hasWeather(S, HW).


/* NÚMERO DE PERSONAJES (total y por tipos) */
/* ---------------------------------------------------------------------------- */
numberOfCharacters((S, _), T) :- count_characters(S, T).
/* ---------------------------------------------------------------------------- */

count_characters(S, T) :- count_main_characters(S, NM), count_secondary_characters(S, NS), T is NM + NS.

count_main_characters([], 0).
count_main_characters([F | S], T) :- (is_main_character(F), count_main_characters(S, ST), T is ST + 1, !) ;
                                     count_main_characters(S, T).

is_main_character([main_character | _]).

count_secondary_characters([], 0).
count_secondary_characters([F | S], T) :- (is_secondary_character(F), count_secondary_characters(S, ST), T is ST + 1, !) ;
                                     count_secondary_characters(S, T).

is_secondary_character([secondary_character | _]).

/* ---------------------------------------------------------------------------- */
numberOfMainCharacters((S, _), T) :- count_main_characters(S, T).
numberOfSecondaryCharacters((S, _), T) :- count_secondary_characters(S, T).
/* ---------------------------------------------------------------------------- */

/* RATIO PROTAGONISTAS/TOTAL */
/* ---------------------------------------------------------------------------- */
ratioCharacters(S, T) :- numberOfMainCharacters(S, MC), numberOfCharacters(S, C), T is MC / C.
/* ---------------------------------------------------------------------------- */

/* LONGITUD DE LA HISTORIA */
/* ---------------------------------------------------------------------------- */
storyLength((S, _), T) :- length(S, T).
/* ---------------------------------------------------------------------------- */

/* NÚMERO DE HECHOS */
/* ---------------------------------------------------------------------------- */
numberOfFacts((S, _), T) :- count_facts(S, T).
/* ---------------------------------------------------------------------------- */

count_facts([], 0).
count_facts([F | S], T) :- (is_fact(F), count_facts(S, ST), T is ST + 1, !) ;
                                     count_facts(S, T).

is_fact([start(_)]).
is_fact([sing(_)]).
is_fact([isa | _]).
is_fact([doesfor | _]).
is_fact([hasproperty | _]).

/* RATIO CHARACTERS/FACTS */
/* ---------------------------------------------------------------------------- */
ratioCharactersFacts(Story, T) :- numberOfCharacters(Story, NC), numberOfFacts(Story, NF), T is NC / NF.
/* ---------------------------------------------------------------------------- */

/* EVOLUCIÓN */
/* ---------------------------------------------------------------------------- */
evolution((S, _), T) :- count_character_changes(S, T).
/* ---------------------------------------------------------------------------- */

count_character_changes([], 0).
count_character_changes([F | S], T) :- (is_change(F), count_character_changes(S, ST), T is ST + 1, !) ;
                                     count_character_changes(S, T).

is_change([learnto | _]).

/* NÚMERO DE ACCIONES QUE REALIZA EL PROTAGONISTA */
/* ---------------------------------------------------------------------------- */
numberOfMainCharacterActions((S, _), T) :- findall(A, (find_main_character(S, C),
                                           count_actions(S, C, A)), Acs),
                                           sum_list(Acs, T).
/* ---------------------------------------------------------------------------- */

find_main_character([F | R], C) :- F = [main_character, C] ; find_main_character(R, C).

count_actions([], _, 0).
count_actions([F | S], C, T) :- (is_main_character_action(F, C), count_actions(S, C, ST), T is ST + 1, !) ;
                                     count_actions(S, C, T).

is_main_character_action([doesfor, C | _], C).
is_main_character_action([preparefor, C | _], C).
is_main_character_action([learnto, C | _], C).
is_main_character_action([sing | _], _).
is_action([event, Ev], C) :- has_character(C, Ev).

has_character(C, Ev) :- format(atom(A), "~w", Ev), sub_string(A, _, _, _, C).

/* TOTAL DE ACCIONES */
/* ---------------------------------------------------------------------------- */
numberOfActions((S, _), T) :- count_actions(S, T).
/* ---------------------------------------------------------------------------- */

count_actions([], 0).
count_actions([F | S], T) :- (is_action(F), count_actions(S, ST), T is ST + 1, !) ;
                             count_actions(S, T).

is_action([doesfor | _]).
is_action([preparefor | _]).
is_action([learnto | _]).
is_action([sing | _]).
is_action([event | _]).

/* RATIO ACCIONES PROTAGONISTA/ACCIONES TOTALES */
/* ---------------------------------------------------------------------------- */
ratioActions(S, T) :- numberOfMainCharacterActions(S, MCA), numberOfActions(S, NA), T is MCA / NA.
/* ---------------------------------------------------------------------------- */


/* AMOR */
/* ---------------------------------------------------------------------------- */
loveLevel((S, _), T) :- measure_story_love(S, T).
/* ---------------------------------------------------------------------------- */

measure_story_love([], 0).
measure_story_love([F | S], T) :- (is_love_fact(F, Val), measure_story_love(S, ST), T is ST + Val, !) ;
                                     measure_story_love(S, T).

is_love_fact([brother | _], 2).
is_love_fact([parent | _], 2).
is_love_fact([friend | _], 1).

/* ODIO */
/* ---------------------------------------------------------------------------- */
hateLevel((S, _), T) :- measure_story_hate(S, T).
/* ---------------------------------------------------------------------------- */

measure_story_hate([], 0).
measure_story_hate([F | S], T) :- (is_hate_fact(F, Val), measure_story_hate(S, ST), T is ST + Val, !) ;
                                     measure_story_hate(S, T).

is_hate_fact([enemy | _], 1).
is_hate_fact([start, fight], 1).
is_hate_fact([get, _, angry], 2).
is_hate_fact([preparefor, _, fight], 3).

/* DRAMA */
/* ---------------------------------------------------------------------------- */
dramaLevel((S, _), T) :- measure_story_drama(S, T).
/* ---------------------------------------------------------------------------- */

measure_story_drama([], 0).
measure_story_drama([F | S], T) :- (is_drama_fact(F, Val), measure_story_drama(S, ST), T is ST + Val, !) ;
                                     measure_story_drama(S, T).

is_drama_fact([enemy | _], 1).
is_drama_fact([start, fight], 1).
is_drama_fact([get, _, scared], 2).
is_drama_fact([start, rain], 1).
is_drama_fact([get, _, wet], 1).

/* DIVERSIÓN */
/* ---------------------------------------------------------------------------- */
funLevel((S, _), T) :- measure_story_fun(S, T).
/* ---------------------------------------------------------------------------- */

measure_story_fun([], 0).
measure_story_fun([F | S], T) :- (is_fun_fact(F, Val), measure_story_fun(S, ST), T is ST + Val, !) ;
                                     measure_story_fun(S, T).

is_fun_fact([get, _, wet], 1).
is_fun_fact([get, _, scared], 2).

/* PLAUSIBILIDAD */
/* ---------------------------------------------------------------------------- */
plausibility((_, P), P).
/* ---------------------------------------------------------------------------- */

/* TIENE FECHA */
/* ---------------------------------------------------------------------------- */
hasDate((S, _), T) :- find_date(S, T).
/* ---------------------------------------------------------------------------- */

find_date([], 0).
find_date([F | S], T) :- (is_date(F), T is 1, !) ; find_date(S, T).

is_date([date | _]).

/* TIENE HORA */
/* ---------------------------------------------------------------------------- */
hasTime((S, _), T) :- find_time(S, T).
/* ---------------------------------------------------------------------------- */

find_time([], 0).
find_time([F | S], T) :- (is_time(F), T is 1, !) ; find_time(S, T).

is_time([time | _]).

/* TIENE CLIMA */
/* ---------------------------------------------------------------------------- */
hasWeather((S, _), T) :- find_weather(S, T).
/* ---------------------------------------------------------------------------- */

find_weather([], 0).
find_weather([F | S], T) :- (is_weather(F), T is 1, !) ; find_weather(S, T).

is_weather([_, sky]).
