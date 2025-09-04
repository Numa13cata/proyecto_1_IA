:- encoding(utf8).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

% =========================
% Base de hechos (Prolog)
% =========================

% ---- Usuarios (U)
user(u1). user(u2). user(u3). user(u4). user(u5).
user(u6). user(u7). user(u8). user(u9). user(u10).

% ---- Categorías por CÓDIGO
category(c1).
category(c2).
category(c3).
category(c4).
category(c5).

% ---- Nombre de la categoría por CÓDIGO
category_name(c1, libro).
category_name(c2, tecnologia).
category_name(c3, musica).
category_name(c4, ropa).
category_name(c5, hogar).

% ---- Productos (P) asociados al CÓDIGO de categoría
% Catálogos ampliados para asegurar 3+ candidatos en varias categorías
product(p1,  c1).
product(p2,  c1).
product(p19, c1).
product(p20, c1).

product(p3,  c2).
product(p4,  c2).
product(p11, c2).
product(p12, c2).

product(p5,  c3).
product(p6,  c3).
product(p13, c3).
product(p14, c3).

product(p7,  c4).
product(p8,  c4).
product(p15, c4).
product(p16, c4).

product(p9,  c5).
product(p10, c5).
product(p17, c5).
product(p18, c5).

% ---- Nombres de productos
product_name(p1,  'Cien años de soledad').
product_name(p2,  'El señor de los anillos').
product_name(p19, 'Libro de ciencia ficción').
product_name(p20, 'Diccionario').
product_name(p3,  'Auriculares inalámbricos').
product_name(p4,  'Smartphone Android').
product_name(p11, 'Laptop ultraliviana').
product_name(p12, 'Tablet 10 pulgadas').
product_name(p5,  'Guitarra acústica').
product_name(p6,  'Teclado electrónico').
product_name(p13, 'Bajo eléctrico').
product_name(p14, 'Auriculares de estudio').
product_name(p7,  'Camiseta deportiva').
product_name(p8,  'Tenis de running').
product_name(p15, 'Pantalón deportivo').
product_name(p16, 'Chaqueta impermeable').
product_name(p9,  'Silla').
product_name(p10, 'Lámpara de escritorio').
product_name(p17, 'Estantería').
product_name(p18, 'Almohada ergonómica').

% ---- Compras (B): bought(U,P)  -> equivalente a B[u,p] = 1
bought(u1, p6).  bought(u1, p4).  bought(u1, p3).
bought(u2, p5).  bought(u2, p3).  bought(u2, p8).
bought(u3, p1).  bought(u3, p2).  bought(u3, p9).
bought(u4, p4).  bought(u4, p10). bought(u4, p7).
bought(u5, p8).  bought(u5, p7).  bought(u5, p9).
bought(u6, p2).  bought(u6, p6).  bought(u6, p5).
bought(u7, p2).  bought(u7, p6).  bought(u7, p5).
bought(u8, p1).
% (u9..u10 sin compras)

% ---- Calificaciones (R): rating(U,P,Score)  -> 1..5
rating(u1, p6, 5).  rating(u1, p4, 4).  rating(u1, p3, 3).
rating(u2, p5, 5).  rating(u2, p3, 2).  rating(u2, p8, 4).
rating(u3, p1, 4).  rating(u3, p2, 4).  rating(u3, p9, 5).
rating(u4, p4, 5).  rating(u4, p10, 4). rating(u4, p7, 2).
rating(u5, p8, 5).  rating(u5, p7, 4).  rating(u5, p9, 2).
rating(u6, p2, 5).  rating(u6, p6, 4).  rating(u6, p5, 3).

% Calificaciones para productos nuevos (para darles popularidad)
rating(u2, p11, 4). rating(u3, p11, 5). rating(u5, p11, 4).
rating(u1, p12, 4). rating(u6, p12, 5).
rating(u1, p13, 4). rating(u2, p14, 4).
rating(u3, p15, 4). rating(u4, p16, 5).
rating(u5, p17, 4). rating(u6, p18, 4).
rating(u2, p19, 5). rating(u3, p20, 4).

% =========================
% Reglas de recomendación
% =========================

% ---- Popularidad global de un producto (cantidad de ratings)
product_popularity(P, Count) :-
    findall(S, rating(_, P, S), Scores),
    length(Scores, Count).

% ---- Popularidad global de una categoría (usuarios únicos)
category_popularity(C, Count) :-
    findall(U, (bought(U, P), product(P, C)), UsersDup),
    sort(UsersDup, Users),
    length(Users, Count).

% ---- Caso A: Usuario sin compras → producto mas popular
recommend_one(User, Product) :-
    \+ bought(User, _),
    findall(Count-P, (product(P, _), product_popularity(P, Count)), L),
    keysort(L, Sorted),
    reverse(Sorted, [_-Product|_]).

% ---- Caso B: Usuario con 1 compra → otro de la misma categoría
recommend_one(User, Product) :-
    findall(P, bought(User, P), [OnlyProduct]),
    product(OnlyProduct, C),
    product(Product, C),
    Product \= OnlyProduct.

% ---- Caso C/D: Usuario con varias compras → mejor calificación propia;
%      si hay empate, desempatar por categoría mas popular; recomendar OTRO producto de esa categoría
recommend_one(User, Product) :-
    findall(Score-P, rating(User, P, Score), Ratings),
    Ratings = [_,_|_],
    keysort(Ratings, Sorted),
    reverse(Sorted, Rev),
    Rev = [BestScore-_|_],
    include(=(BestScore-_), Rev, BestPairs),
    findall(Pop-C,
            ( member(BestScore-Px, BestPairs),
              product(Px, C),
              category_popularity(C, Pop)
            ),
            CatPairs),
    keysort(CatPairs, CatPairsS),
    reverse(CatPairsS, [_-BestCat|_]),
    findall(Cnt-Q,
            ( product(Q, BestCat),
              \+ bought(User, Q),
              product_popularity(Q, Cnt)
            ),
            AltPairs),
    ( AltPairs \= [] ->
        keysort(AltPairs, AltSorted),
        reverse(AltSorted, [_-Product|_])
    ;   ( member(_-Py, Rev),
          product(Py, CatY),
          findall(Cn-Qy,
                  ( product(Qy, CatY),
                    \+ bought(User, Qy),
                    product_popularity(Qy, Cn)
                  ),
                  OptsY),
          OptsY \= [],
          keysort(OptsY, OptsYS),
          reverse(OptsYS, [_-Product|_])
        -> true
        ;   findall(Cg-Qg,
                    ( product(Qg, _),
                      \+ bought(User, Qg),
                      product_popularity(Qg, Cg)
                    ),
                    GlobalPairs),
            keysort(GlobalPairs, GlobalS),
            reverse(GlobalS, [_-Product|_])
        )
    ).

% ---- Items con calificación positiva (>3)
liked_item(User, Product) :-
    rating(User, Product, Score),
    Score > 3.

% ---- Tomar los primeros N elementos de una lista
take_n(_, N, []) :- N =< 0, !.
take_n([], _, []).
take_n([H|T], N, [H|R]) :-
    N > 0,
    N1 is N - 1,
    take_n(T, N1, R).

% ---- Top 10 de ítems gustados por una lista de usuarios
top_liked(Users, Top10) :-
    findall(P, (member(U, Users), liked_item(U, P)), AllProducts),
    sort(AllProducts, UniqueProducts),
    findall(Count-P,
            ( member(P, UniqueProducts),
              aggregate_all(count, member(P, AllProducts), Count)
            ),
            Counts),
    keysort(Counts, KSorted),
    reverse(KSorted, Desc),
    take_n(Desc, 10, Top10Pairs),
    findall(Product, member(_-Product, Top10Pairs), Top10).

% ---- Comparador de productos por popularidad
% better_prod(P, Q) :- Q es mas popular que P.
better_prod(P, Q) :-
    product_popularity(Q, C2),
    product_popularity(P, C1),
    C2 > C1.

% ---- Máximo global: P no tiene otro Q mas popular
max_product(P) :-
    product(P, _),
    \+ ( product(Q, _), better_prod(P, Q) ).

% ---- Recomendación simple (usuario sin compras)
recommend_simple(User, Product) :-
    \+ bought(User, _),
    max_product(Product).

% -------- Helpers para elegir categoría objetivo --------

no_purchases(User) :- \+ bought(User, _).

one_purchase(User, OnlyP) :-
    findall(P, bought(User, P), Ps),
    Ps = [OnlyP].

% Elegir categoría según reglas A/B/C-D
choose_category_for_list(User, CatChosen) :-
    % Caso A: sin compras
    no_purchases(User), !,
    findall(Count-P, (product(P, _), \+ bought(User, P), product_popularity(P, Count)), Pairs),
    Pairs \= [],
    keysort(Pairs, S),
    reverse(S, [_-BestP|_]),
    product(BestP, CatChosen).

choose_category_for_list(User, CatChosen) :-
    % Caso B: 1 compra
    one_purchase(User, OnlyP), !,
    product(OnlyP, CatChosen).

choose_category_for_list(User, CatChosen) :-
    % Caso C/D: 2+ compras -> mejor score, desempate por popularidad de categoría
    rating(User, P1, _), rating(User, P2, _), P1 \= P2,
    findall(S, rating(User, _, S), Scores),
    max_list(Scores, BestScore),
    findall(C, (rating(User, Px, BestScore), product(Px, C)), CatsRaw),
    sort(CatsRaw, CatsBest),
    findall(Pop-C, (member(C, CatsBest), category_popularity(C, Pop)), CatPairs),
    keysort(CatPairs, CatSorted),
    reverse(CatSorted, [_-CatChosen|_]).

% -------- Construcción y consulta principal (solo 1 parámetro visible) --------

% Lista de candidatos en una categoría (no comprados), ordenados por popularidad desc
sorted_candidates_in_cat(User, Cat, ProductsDesc) :-
    findall(Count-P,
            ( product(P, Cat),
              \+ bought(User, P),
              product_popularity(P, Count)
            ),
            Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, DescPairs),
    findall(P, member(_-P, DescPairs), ProductsDesc).

% -------- recommend_list/2 EXPUESTA COMO recommend_list(User, List) --------
% Devuelve TODOS los productos (no comprados) de la categoría elegida para el usuario.
recommend_list(User, List) :-
    choose_category_for_list(User, Cat),
    sorted_candidates_in_cat(User, Cat, List).
