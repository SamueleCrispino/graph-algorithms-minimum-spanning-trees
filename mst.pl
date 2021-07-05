% Crispino Samuele 856339


appendi([], L, L).
appendi([Y | Ys], X, [Y | Zs]) :-
    appendi(X, Ys, Zs).

lunghezza([], 0).
lunghezza([_ | T], N) :-
    lunghezza(T, Ns),
    N is Ns + 1.

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic node/3.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic preorder_arc/4.



new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.


delete_graph(G) :- retract(graph(G)).



new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- graph(G), assert(vertex(G, V)), !.


graph_vertices(G, Vs) :- graph(G), findall(V, vertex(G, V), Vs).


list_vertices(G) :- graph(G), listing(vertex(G, _)).




new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :- graph(G), vertex(G, U),
    vertex(G, V), Weight >= 0, assert(arc(G, U, V, Weight)), !.
new_arc(G, U, V) :- new_arc(G, U, V, 1), !.


graph_arcs(G, Es) :- graph(G), findall(arc(G, U, V, W),
                                       arc(G, U, V, W), Es).



vertex_neighbors(G, V, Ns) :- graph(G),
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Xs),
    findall(arc(G, V, N, W), arc(G, N, V, W), Ys),
    appendi(Xs, Ys, Ns).



adjs(G, V, Vs) :- graph(G),
    vertex(G, V),
    findall(vertex(G, X), arc(G, V, X, _), Vsx),
    findall(vertex(G, X), arc(G, X, V, _), Vsy),
    appendi(Vsx, Vsy, Vs).



list_arcs(G) :- graph(G), listing(arc(G, _, _, _)).



list_graph(G) :- graph(G), list_vertices(G), list_arcs(G).



read_graph(G, FileName) :- retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    new_graph(G),
    csv_read_file(FileName, Rows, [functor(table), arity(3), separator(0'\t)]),
    create_graph(G, Rows), !.

create_graph(_, []).
create_graph(G, [R | Rs]) :- arg(1, R, U),
    arg(2, R, V),
    arg(3, R, W),
    new_vertex(G, U),
    new_vertex(G, V),
    new_arc(G, U, V, W),
    create_graph(G, Rs).



write_graph(G, FileName, Type) :- Type = 'graph', !,
    graph(G),
    findall(member(U, V, W), arc(G, U, V, W), Es),
    csv_write_file(FileName, Es, [separator(0'\t)]).

write_graph(G, FileName, Type) :- Type = 'edges',
    csv_write_file(FileName, G, [separator(0'\t)]).

write_graph(G, FileName) :- write_graph(G, FileName, 'graph').




new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.



delete_heap(H) :-  retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

delete_nodes(H) :- retractall(node(H, _, _)).

heap_has_size(H, S) :- heap(H, S).



heap_empty(H) :- heap_has_size(H, 0).


heap_not_empty(H) :- heap_has_size(H, S), S > 0.

heap_head(H, K, V) :-  heap(H, _), heap_entry(H, 1, K, V).



heap_insert(H, K, V) :- heap(H, S),
    New_size is S + 1,
    fix_heap_size(H, New_size),
    assert(heap_entry(H,  New_size, K, V)),
    assert(node(H, K, V)),
    heapify(H, New_size).

heap_increase_key(H, S, K, _V) :-
    Prev is floor(S / 2),
    heap_entry(H, Prev, K2, _V2),
    K >= K2, !.

fix_heap_size(H, New_size) :- heap(H, New_size), !.
fix_heap_size(H, New_size) :- heap(H, S),
    S \= New_size,
    retract(heap(H, S)),
    assert(heap(H, New_size)), !.


heapify(_, S) :- S = 1, !.

heapify(H, S) :- heap_entry(H, S, K1, _V1),
    Prev is floor(S / 2),
    heap_entry(H, Prev, K2, _V2),
    K1 >= K2, !.

heapify(H, S) :- heap_entry(H, S, K1, V1),
    Prev is floor(S / 2),
    heap_entry(H, Prev, K2, V2),
    K1 < K2,
    retract(heap_entry(H, S, K1, V1)),
    retract(heap_entry(H, Prev, K2, V2)),
    assert(heap_entry(H, S, K2, V2)),
    assert(heap_entry(H, Prev, K1, V1)),
    heapify(H, Prev), !.


heapify_top(H, I, S, Left, Right) :-
    Left =< S,
    Right =< S,
    heap_entry(H, Left, Kl, _Vl),
    heap_entry(H, I, Ki, Vi),
    heap_entry(H, Right, Kr, Vr),
    Ki > Kr,
    Kl > Kr, !,
    retract(heap_entry(H, Right, Kr, Vr)),
    retract(heap_entry(H, I, Ki, Vi)),
    assert(heap_entry(H, Right, Ki, Vi)),
    assert(heap_entry(H, I, Kr, Vr)),
    heapify_init(H, Right, S).

heapify_top(H, I, S, Left, Right) :-
    Left =< S,
    Right =< S,
    heap_entry(H, Left, Kl, _Vl),
    heap_entry(H, I, Ki, _Vi),
    heap_entry(H, Right, Kr, _Vr),
    Ki > Kl,
    Kr >= Kl, !,
    retract(heap_entry(H, Left, Kl, Vl)),
    retract(heap_entry(H, I, Ki, Vi)),
    assert(heap_entry(H, Left, Ki, Vi)),
    assert(heap_entry(H, I, Kl, Vl)),
    heapify_init(H, Left, S).

heapify_top(H, I, S, Left, Right) :-
    Left =< S,
    Right =< S,
    heap_entry(H, Left, Kl, _Vl),
    heap_entry(H, I, Ki, _Vi),
    heap_entry(H, Right, Kr, _Vr),
    Ki =< Kl,
    Ki =< Kr, !.
heapify_top(H, I, S, Left, Right) :-
    Left =< S,
    Right > S,
    heap_entry(H, Left, Kl, Vl),
    heap_entry(H, I, Ki, Vi),
    Ki > Kl, !,
    retract(heap_entry(H, Left, Kl, Vl)),
    retract(heap_entry(H, I, Ki, Vi)),
    assert(heap_entry(H, Left, Ki, Vi)),
    assert(heap_entry(H, I, Kl, Vl)),
    heapify_init(H, Left, S).

heapify_top(H, I, S, Left, Right) :-
    Right > S,
    Left =< S,
    heap_entry(H, Left, Kl, _Vl),
    heap_entry(H, I, Ki, _Vi),
    Ki =< Kl, !.

heapify_top(_H, I, S, _Left, _Right) :- I >= S.

heapify_top(_H, _I, S, Left, _Right) :-
    Left > S, !.

heapify_init(H, I, S) :-
    Left is 2*I,
    Right is 2*I + 1,
    heapify_top(H, I, S, Left, Right), !.


heap_extract(H, K, V) :- heap(H, S), S = 1, !,
    heap_entry(H, 1, K, V),
    retract(heap_entry(H, 1, K, V)),
    New_size is S - 1,
    fix_heap_size(H, New_size), !.


heap_extract(H, K, V) :-
    heap(H, S),
    S > 1, !,
    heap_entry(H, 1, K, V),
    retract(heap_entry(H, 1, K, V)),
    heap_entry(H, S, Kf, Vf),
    assert(heap_entry(H, 1, Kf, Vf)),
    retract(heap_entry(H, S, Kf, Vf)),
    New_size is S - 1,
    fix_heap_size(H, New_size),
    heapify_init(H, 1, New_size), !.


heap_extract_non_ottimizzato(H, K, V) :- heap_not_empty(H),
    heap_head(H, K, V),
    retract(node(H, K, V)),
    delete_heap(H),
    new_heap(H),
    findall(node(H, K1, V1), node(H, K1, V1), Ns),
    inserisci(H, Ns).


inserisci(_H, []) :- !.
inserisci(H, [N|Ns]) :-
    arg(2, N, K),
    arg(3, N, V),
    retract(node(H, K, V)),
    heap_insert(H, K, V),
    inserisci(H, Ns).



modify_key(H, NewKey, OldKey, V) :- heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    heap(H, S),
    heapify(H, P),
    heapify_init(H, P, S).





list_heap(H) :- heap(H, _), listing(heap_entry(H, _, _, _)), !.




mst_prim(G, Source) :- graph(G),
    delete_mst(G),
    graph_vertices(G, Vs),
    rimuovi(Vs, Source, Xs),
    new_heap(prim_heap),
    init_prim(G, prim_heap, Xs),
    primo_giro(prim_heap, G, Source),
    while_heap(prim_heap, G), !.


init_prim(_, _, []) :- !.
init_prim(Grafo, Heap, [V | Vs]) :- heap_insert(Heap, inf, V),
    assert(vertex_key(Grafo, V, inf)),
    assert(vertex_previous(Grafo, V, nil)),
    init_prim(Grafo, Heap, Vs).


primo_giro(Heap, _, _) :- heap(Heap, S), S =< 0, !.

primo_giro(Heap, Graph, Source) :- heap(Heap, S), S > 0,
    adjs(Graph, Source, Vs),
    for_each_vertex(Graph, Heap, Source, Vs).


while_heap(Heap, _) :- heap(Heap, S), S = 0, !.

while_heap(Heap, Graph) :- heap(Heap, S), S > 0,
    heap_entry(Heap, 1, K, U),
    heap_extract(Heap, K, U),
    adjs(Graph, U, Vs),
    for_each_vertex(Graph, Heap, U, Vs),
    while_heap(Heap, Graph), !.


for_each_vertex(_, _, _, []) :- !.


for_each_vertex(Graph, Heap, U, [Vert | Vs]) :- arg(2, Vert, V),
    heap_entry(Heap, _, K, V),
    arc(Graph, U, V, W),
    W < K,
    retract(vertex_previous(Graph, V, _)),
    assert(vertex_previous(Graph, V, U)),
    modify_key(Heap, W, K, V),
    retract(vertex_key(Graph, V, K)),
    assert(vertex_key(Graph, V, W)),
    for_each_vertex(Graph, Heap, U, Vs), !.


for_each_vertex(Graph, Heap, U, [Vert | Vs]) :- arg(2, Vert, V),
    heap_entry(Heap, _, K, V),
    arc(Graph, V, U, W),
    W < K,
    retract(vertex_previous(Graph, V, _)),
    assert(vertex_previous(Graph, V, U)),
    modify_key(Heap, W, K, V),
    retract(vertex_key(Graph, V, K)),
    assert(vertex_key(Graph, V, W)),
    for_each_vertex(Graph, Heap, U, Vs), !.



for_each_vertex(Graph, Heap, U, [_V | Vs]) :-
    for_each_vertex(Graph, Heap, U, Vs).


mst_get(G, Source, PreorderTree) :-
    findall(arc(G, Source, V, W),
    lock(G, V, Source, W), Figli),
    preorder(G, Source, Figli),
    findall(arc(G, Padre, Figlio, Peso),
            preorder_arc(G, Padre, Figlio, Peso),
            PreorderTree), !.


preorder(_G, _U, []) :- !.

preorder(G, U, Figli) :-
    arc_list_min(Figli, arc(G, U, Vmin, Wmin)),
    rimuovi(Figli, arc(G, U, Vmin, Wmin), Resto),
    assert(preorder_arc(G, U, Vmin, Wmin)),
    findall(arc(G, Vmin, V, W), lock(G, V, Vmin, W),  Figli_Suc),
    preorder(G, Vmin, Figli_Suc),
    preorder(G, U, Resto).




arc_list_min([arc(G, U, V, W)|Ls], arc(G, U, Vmin, Wmin)) :-
    arc_list_min(Ls, arc(G, U, V, W), arc(G, U, Vmin, Wmin)), !.

arc_list_min([], Min, Min) :- !.

arc_list_min([arc(G, U, V, W) |Ls], arc(G, U, _VMin0, Wmin0), arc(G, U, VMin, WMin)) :-
    W < Wmin0,
    arc_list_min(Ls, arc(G, U, V, W),  arc(G, U, VMin, WMin)), !.

arc_list_min([arc(G, U, _V, W) |Ls], arc(G, U, VMin0, Wmin0), arc(G, U, VMin, WMin)) :-
    W > Wmin0,
    arc_list_min(Ls, arc(G, U, VMin0, Wmin0), arc(G, U, VMin, WMin)), !.

arc_list_min([arc(G, U, V, W) |Ls], arc(G, U, VMin0, Wmin0), arc(G, U, VMin, WMin)) :-
    W = Wmin0,
    sort([V, VMin0], [S | _]),
    arc_list_min(Ls, arc(G, U, S, Wmin0), arc(G, U, VMin, WMin)), !.




rimuovi([], _, []) :- !.
rimuovi([Elemento | Coda], Elemento, Coda2) :- rimuovi(Coda, Elemento, Coda2).
rimuovi([Testa| Coda], Elemento, [Testa| Coda2] ) :-
    Testa \= Elemento,
    rimuovi(Coda, Elemento, Coda2).


delete_mst(G) :-
    retractall(vertex_previous(G, _, _)),
    retractall(vertex_key(G, _, _)),
    retractall(preorder_arc(G, _, _, _)),
    delete_heap(prim_heap).



lock(G, V, U, W) :-
   vertex_previous(G, V, U),
   vertex_key(G, V, W).
