:- encoding(utf8).

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
product(p1,  c1).
product(p2,  c1).
product(p3,  c2).
product(p4,  c2).
product(p5,  c3).
product(p6,  c3).
product(p7,  c4).
product(p8,  c4).
product(p9,  c5).
product(p10, c5).

% ---- Nombres de productos
product_name(p1,  'Cien años de soledad').
product_name(p2,  'El señor de los anillos').
product_name(p3,  'Auriculares inalámbricos').
product_name(p4,  'Smartphone Android').
product_name(p5,  'Guitarra acústica').
product_name(p6,  'Teclado electrónico').
product_name(p7,  'Camiseta deportiva').
product_name(p8,  'Tenis de running').
product_name(p9,  'Silla').
product_name(p10, 'Lámpara de escritorio').

% ---- Compras (B): bought(U,P)  -> equivalente a B[u,p] = 1
bought(u1, p6).  bought(u1, p4).  bought(u1, p3).
bought(u2, p5).  bought(u2, p3).  bought(u2, p8).
bought(u3, p1).  bought(u3, p2).  bought(u3, p9).
bought(u4, p4).  bought(u4, p10). bought(u4, p7).
bought(u5, p8).  bought(u5, p7).  bought(u5, p9).
bought(u6, p2).  bought(u6, p6).  bought(u6, p5).
bought(u7, p2).  bought(u7, p6).  bought(u7, p5).
% (u8..u10 sin compras para cubrir también el caso "sin historial")

% ---- Calificaciones (R): rating(U,P,Score)  -> 1..5
rating(u1, p6, 5).  rating(u1, p4, 4).  rating(u1, p3, 3).
rating(u2, p5, 5).  rating(u2, p3, 2).  rating(u2, p8, 4).
rating(u3, p1, 5).  rating(u3, p2, 4).  rating(u3, p9, 3).
rating(u4, p4, 5).  rating(u4, p10,4).  rating(u4, p7, 2).
rating(u5, p8, 5).  rating(u5, p7, 4).  rating(u5, p9, 2).
rating(u6, p2, 5).  rating(u6, p6, 4).  rating(u6, p5, 3).

% =========================
% Reglas de recomendación
% =========================

% ==== Popularidad global de un producto (cantidad de ratings) ====
product_popularity(P, Count) :-
    findall(Score, rating(_, P, Score), Scores),
    length(Scores, Count).

% ==== Popularidad global de una categoría ====
category_popularity(C, Count) :-
    findall(U, (bought(U, P), product(P, C)), Users),
    length(Users, Count).

% ==== Caso A: Usuario sin compras → producto mas popular ====
recommend_one(User, Product) :-
    \+ bought(User, _),  % no ha comprado nada
    setof(Count-P, product_popularity(P, Count), L),
    reverse(L, [_MaxCount-Product|_]).

% ==== Caso B: Usuario con 1 compra → otro de la misma categoría ====
recommend_one(User, Product) :-
    findall(P, bought(User, P), [OnlyProduct]),
    product(OnlyProduct, C),
    product(Product, C),
    Product \= OnlyProduct.

% ==== Caso C: Usuario con varias compras → mejor calificación ====
recommend_one(User, Product) :-
    findall(Score-P, rating(User, P, Score), Ratings),
    sort(Ratings, Sorted),
    reverse(Sorted, [BestScore-BestProduct|Rest]),
    (   % ==== Caso D: Empate de calificación ====
        member(BestScore-OtherProduct, Rest)
    ->  product(BestProduct, C1),
        product(OtherProduct, C2),
        category_popularity(C1, Pop1),
        category_popularity(C2, Pop2),
        (Pop1 >= Pop2 -> Product = BestProduct ; Product = OtherProduct)
    ;   Product = BestProduct
    ).
