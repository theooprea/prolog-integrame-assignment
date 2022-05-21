:- ensure_loaded('checker.pl').

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

intrebari(_, _) :- false.

intrebari(integ(_, _, [], _), []).
intrebari(integ(H, W, [((_,_), X)|LTail], Vocab), Lista_intrebari) :- atom(X), intrebari(integ(H, W, LTail, Vocab), Lista_intrebari).
intrebari(integ(H, W, [((_,_), [])|LTail], Vocab), Lista_intrebari) :- intrebari(integ(H, W, LTail, Vocab), Lista_intrebari).
intrebari(integ(H, W, [((R,C), [HList|TList])|LTail], Vocab), [((R, C), HList)|Lista_intrebari]) :- intrebari(integ(H, W, [((R,C), TList)|LTail], Vocab), Lista_intrebari).


% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

%id_intrebare(integ(_,_,[],_), _, _) :- false.
%id_intrebare(integ(H, W, [((_,_), x)|LTail], Vocab), Intrebare, Q_ID) :- id_intrebare(integ(H, W, LTail, Vocab), Intrebare, Q_ID).
%id_intrebare(integ(H, W, [((_,_), [])|LTail], Vocab), Intrebare, Q_ID) :- id_intrebare(integ(H, W, LTail, Vocab), Intrebare, Q_ID).
%id_intrebare(integ(_, _, [((_,_), [(Intrebare, _, Q_ID)|_])|_], Vocab), Intrebare, Q_ID).
%id_intrebare(integ(_, _, [((R,C), [(_, _, _)|TList])|LTail], Vocab), Intrebare, Q_ID) :- id_intrebare(integ(H, W, [((R, C), TList)|LTail], Vocab), Intrebare, Q_ID).
id_intrebare(integ(H, W, List, Vocab), Intrebare, Q_ID) :- intrebari(integ(H, W, List, Vocab), Res), member(((_,_), Intrebare, _, Q_ID), Res).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
completare_solutie(integ(_, _, [], _), (_, _), [], _) :- false.
completare_solutie(integ(H, W, [((_,_), X)|LTail], Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja) :- atom(X), completare_solutie(integ(H, W, LTail, Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja).
completare_solutie(integ(H, W, [((_,_), [])|LTail], Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja) :- completare_solutie(integ(H, W, LTail, Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja).
completare_solutie(integ(_, _, [((R,C), [(Intrebare, j, _)|_])|_], _), (Intrebare, Raspuns), Solutie_Result, ListaDeja) :- atom_chars(Raspuns, CharQuestion), R1 is R + 1, completare_intrebare_jos(R1, C, CharQuestion, Solutie_Result, ListaDeja).
completare_solutie(integ(_, _, [((R,C), [(Intrebare, d, _)|_])|_], _), (Intrebare, Raspuns), Solutie_Result, ListaDeja) :- atom_chars(Raspuns, CharQuestion), C1 is C + 1, completare_intrebare_dreapta(R, C1, CharQuestion, Solutie_Result, ListaDeja).
completare_solutie(integ(H, W, [((R,C), [(Intrebare2, j, _)|TList])|LTail], Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja) :- \+ (Intrebare2 == Intrebare), completare_solutie(integ(H, W, [((R,C), TList)|LTail], Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja).
completare_solutie(integ(H, W, [((R,C), [(Intrebare2, d, _)|TList])|LTail], Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja) :- \+ (Intrebare2 == Intrebare), completare_solutie(integ(H, W, [((R,C), TList)|LTail], Vocab), (Intrebare, Raspuns), Solutie_Result, ListaDeja).


completare_solutii(integ(_, _, _, _), [], []).
completare_solutii(integ(H, W, List, Vocab), [HeadSol|TailSol], ResList) :- completare_solutie(integ(H, W, List, Vocab), HeadSol, Var1, List), completare_solutii(integ(H, W, List, Vocab), TailSol, Var2), append(Var1, Var2, ResAux), sort(ResAux, ResList).

completare(_, _, _) :- false.
completare(integ(H, W, List, Vocab), Sol, integ(H, W, ResList, Vocab)) :- completare_solutii(integ(H, W, List, Vocab), Sol, ResSol), append(List, ResSol, ResList).

completare_intrebare_jos(_, _, _, _, _) :- false.
completare_intrebare_jos(_,_, [], [], _).
completare_intrebare_jos(R, C, [H|Tail], [((R, C), H)|ResTail], ListaDeja) :- R1 is R + 1, \+ (member(((R, C), X), ListaDeja), \+ (H==X)), completare_intrebare_jos(R1, C, Tail, ResTail, ListaDeja).

completare_intrebare_dreapta(_, _, _, _, _) :- false.
completare_intrebare_dreapta(_, _, [], [], _).
completare_intrebare_dreapta(R, C, [H|Tail], [((R, C), H)|ResTail], ListaDeja) :- C1 is C + 1, \+ (member(((R, C), X), ListaDeja), \+ (H==X)), completare_intrebare_dreapta(R, C1, Tail, ResTail, ListaDeja).

%atom_chars(string, ans).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
lungime_spatiu(_, _, _) :- false.
lungime_spatiu(integ(H, W, List, Vocab), Intrebare, Lungime) :- intrebari(integ(H, W, List, Vocab), Lista_intrebari), member(((R, C), Intrebare, j, _), Lista_intrebari), R1 is R + 1, lungime_jos(integ(H, W, List, Vocab), (R1, C), 1, Sol), Lungime is Sol.
lungime_spatiu(integ(H, W, List, Vocab), Intrebare, Lungime) :- intrebari(integ(H, W, List, Vocab), Lista_intrebari), member(((R, C), Intrebare, d, _), Lista_intrebari), C1 is C + 1, lungime_dreapta(integ(H, W, List, Vocab), (R, C1), 1, Sol), Lungime is Sol.

lungime_jos(_, _, _) :- false.
lungime_jos(integ(_, _, List, _), (R, C), ResultThusFar, Result) :- member(((R, C), X), List), (X == x), Result is ResultThusFar - 1.
lungime_jos(integ(_, _, List, _), (R, C), ResultThusFar, Result) :- member(((R, C), X), List), is_list(X), Result is ResultThusFar - 1.
lungime_jos(integ(H, W, List, Vocab), (R, C), ResultThusFar, Result) :- \+ member(((R, C), x), List), \+ member(((R, C), [_]), List), \+ member(((R, C), [_, _]), List), R1 is R + 1, ResultThusFar1 is ResultThusFar + 1, lungime_jos(integ(H, W, List, Vocab), (R1, C), ResultThusFar1, Result).

lungime_dreapta(_, _, _) :- false.
lungime_dreapta(integ(_, _, List, _), (R, C), ResultThusFar, Result) :- member(((R, C), X), List), (X == x), Result is ResultThusFar - 1.
lungime_dreapta(integ(_, _, List, _), (R, C), ResultThusFar, Result) :- member(((R, C), X), List), is_list(X), Result is ResultThusFar - 1.
lungime_dreapta(integ(H, W, List, Vocab), (R, C), ResultThusFar, Result) :- \+ member(((R, C), x), List), \+ member(((R, C), [_]), List), \+ member(((R, C), [_, _]), List), C1 is C + 1, ResultThusFar1 is ResultThusFar + 1, lungime_dreapta(integ(H, W, List, Vocab), (R, C1), ResultThusFar1, Result).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(_, _, _, _, _) :- false.
intersectie(integ(H, W, List, Vocab), Intrebare1, Poz1, Intrebare2, Poz2) :-
    intrebari(integ(H, W, List, Vocab), Lista_intrebari), member(((R1, C1), Intrebare1, j, _), Lista_intrebari), member(((R2, C2), Intrebare2, d, _), Lista_intrebari),
    Ccomun is C1, Rcomun is R2, Lungime1 is Rcomun - R1, Lungime2 is Ccomun - C2, lungime_spatiu(integ(H, W, List, Vocab), Intrebare1, Lungime1Max), lungime_spatiu(integ(H, W, List, Vocab), Intrebare2, Lungime2Max),
    Lungime1 =< Lungime1Max, Lungime2 =< Lungime2Max, Poz1 is Lungime1 - 1, Poz2 is Lungime2 - 1, Poz1 >= 0, Poz2 >= 0.

intersectie(integ(H, W, List, Vocab), Intrebare1, Poz1, Intrebare2, Poz2) :-
    intrebari(integ(H, W, List, Vocab), Lista_intrebari), member(((R1, C1), Intrebare1, d, _), Lista_intrebari), member(((R2, C2), Intrebare2, j, _), Lista_intrebari),
    Ccomun is C2, Rcomun is R1, Lungime1 is Ccomun - C1, Lungime2 is Rcomun - R2, lungime_spatiu(integ(H, W, List, Vocab), Intrebare1, Lungime1Max), lungime_spatiu(integ(H, W, List, Vocab), Intrebare2, Lungime2Max),
    Lungime1 =< Lungime1Max, Lungime2 =< Lungime2Max, Poz1 is Lungime1 - 1, Poz2 is Lungime2 - 1, Poz1 >= 0, Poz2 >= 0.

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_intrebare(_, _, _) :- false.
solutii_intrebare(integ(_, _, _, []), _, []).
solutii_intrebare(integ(H, W, List, [VocabHead|VocabTail]), Intrebare, [ToAdd|List_Words]) :- lungime_spatiu(integ(H, W, List, [VocabHead|VocabTail]), Intrebare, ResLength), atom_length(VocabHead, WordLength), (WordLength == ResLength), atom_chars(VocabHead, VocabHeadList), append(VocabHeadList, [], ToAdd), solutii_intrebare(integ(H, W, List, VocabTail), Intrebare, List_Words).
solutii_intrebare(integ(H, W, List, [VocabHead|VocabTail]), Intrebare, List_Words) :- lungime_spatiu(integ(H, W, List, [VocabHead|VocabTail]), Intrebare, ResLength), atom_length(VocabHead, WordLength), \+ (WordLength == ResLength), solutii_intrebare(integ(H, W, List, VocabTail), Intrebare, List_Words).

solutii_intrebari(_, _, _) :- false.
solutii_intrebari(integ(_, _, _, _), [], []).
solutii_intrebari(integ(H, W, List, Vocab), [((_, _), Intrebare, _, _)|TailIntreb], [(Intrebare, WordsList)|Solutii]) :- solutii_intrebare(integ(H, W, List, Vocab), Intrebare, ResList), append([], ResList, WordsList), solutii_intrebari(integ(H, W, List, Vocab), TailIntreb, Solutii).

solutii_posibile(_, _) :- false.
solutii_posibile(integ(H, W, List, Vocab), Solutii) :- intrebari(integ(H, W, List, Vocab), Sol), solutii_intrebari(integ(H, W, List, Vocab), Sol, Solutii).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
rezolvare_aux(_, _) :- false.
rezolvare_aux(integ(H, W, List, Vocab), Solutie) :-
    solutii_posibile(integ(H, W, List, Vocab), Lista_Raspunsuri_Posibile), intrebari(integ(H, W, List, Vocab), Questions), member(((_, _), Intrebare1, _, _), Questions), member(((_, _), Intrebare2, _, _), Questions),
    \+ (Intrebare1 == Intrebare2), intersectie(integ(H, W, List, Vocab), Intrebare1, Pozitie1, Intrebare2, Pozitie2),
    member((Intrebare1, Lista_Raspunsuri_1), Lista_Raspunsuri_Posibile), member((Intrebare2, Lista_Raspunsuri_2), Lista_Raspunsuri_Posibile), member(Raspuns1, Lista_Raspunsuri_1), member(Raspuns2, Lista_Raspunsuri_2),
    nth0(Pozitie1, Raspuns1, X), nth0(Pozitie2, Raspuns2, Y), (X == Y), atom_chars(Raspuns1String, Raspuns1), Solutie = [(Intrebare1, Raspuns1String)].

rezolvat(_) :- false.
rezolvat(integ(H, W, Lista, Vocab), Countsf) :- sort(Lista, Lista2), Total is H * W, length(Lista2, Lungime), (Lungime == Total), intrebari(integ(H, W, Lista, Vocab), AllQuestions), length(AllQuestions, Lung), (Countsf==Lung).

add_answer(integ(H, W, List, Vocab), _, [], _, Countsf) :- rezolvat(integ(H, W, List, Vocab), Countsf).
add_answer(integ(H, W, List, Vocab), ListaIntrebari, [ElementExtras|ListaPastrate], AlreadyPastrate, Countsf) :- member(SolutiePosibila, ListaIntrebari), member(ElementExtras, SolutiePosibila), \+ member(ElementExtras, AlreadyPastrate), completare(integ(H, W, List, Vocab), SolutiePosibila, IntegramaNoua), append(SolutiePosibila, AlreadyPastrate, NewAlreadyPastrate), Countsf2 is Countsf + 1, add_answer(IntegramaNoua, ListaIntrebari, ListaPastrate, NewAlreadyPastrate, Countsf2).

add_answer2(integ(H, W, List, Vocab), _, [], _, Countsf) :- rezolvat(integ(H, W, List, Vocab), Countsf).
add_answer2(integ(H, W, List, Vocab), ListaIntrebari, [SolutiePosibila|ListaPastrate], AlreadyPastrate, Countsf) :- member(SolutiePosibila, ListaIntrebari), \+ member(SolutiePosibila, AlreadyPastrate), completare(integ(H, W, List, Vocab), [SolutiePosibila], IntegramaNoua), append([SolutiePosibila], AlreadyPastrate, NewAlreadyPastrate), Countsf2 is Countsf + 1, add_answer(IntegramaNoua, ListaIntrebari, ListaPastrate, NewAlreadyPastrate, Countsf2).

rezolvare_2(_, _) :- false.
rezolvare_2(integ(H, W, List, Vocab), SolutieFinala) :- findall(Sol, rezolvare_aux(integ(H, W, List, Vocab), Sol), Bag), sort(Bag, SolutiePartiala), add_answer(integ(H, W, List, Vocab), SolutiePartiala, Pastrate, [], 0), sort(Pastrate, SolutieFinala).

rezolvare(_, _) :- false.
rezolvare(integ(H, W, List, Vocab), SolutieFinala) :- findall(Sol, rezolvare_2(integ(H, W, List, Vocab), Sol), Bag), sort(Bag, Intermediar), member(SolutieFinala, Intermediar).
