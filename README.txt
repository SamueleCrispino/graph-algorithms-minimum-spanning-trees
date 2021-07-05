ISRTRUZIONI PER L'ELABOARZIONE DEL'MST E SUA VISITA PRE-ORDER:

1- Creare il grafo:
  Il grafo può essere creato progressivamente con le funzioni new_graph, new_vertex e new_arc.
  Notare che per la funzione new_vertex non è possibile passare come parametro grafi non presenti
  nella base di verità, similmente, per la funzione new_arc, non è possibile passare come paramteri
  archi e vertici non presenti nella base di verità.
  Utilizzando la funzione read_graph(nome_grafo, file_name) è possibile creare automaticamente un Grafo
  a partire da un file csv (come indicato nel pdf di consegna del progetto) senza doversi preoccupare
  delle suddette regole di costruzione.
  La funzione read_graph sovrascrive qualsiasi grafo con lo stesso nome presente nella base di verità.

2- Creazione dell'MST e visita pre-order:

   mst_prim(nome_grafo, radice), mst_get(nome_grafo, radice, MST).

   All'inizio della funzione mst_prim viene richiamato il metodo delete_mst(nome_grafo),
   che cancella le informazioni relative allo heap del MST di un grafo, vertex_previous,
   vertex_key e alla visita pre-order dell'MST.
   In partica viene sovrascritto, se esiste, l'MST relativo a un grafo con lo stesso nome.


NB: Se si desidera eliminare le informazioni relative allo heap del MST di un grafo, vertex_previous,
vertex_key e alla visita pre-order dell'MST, è possibile utilizzare la funzione:
delete_mst(nome_grafo).


DESCRIZIONE FUNZIONI:

fix_heap_size
modifica la dimensione dello heap

heapify
ricostruisce lo heap partendo dal basso

heapify_top
ricostruisce lo heap partendo dalla testa dello heap

init_prim
inizializza i valori iniziali per vertex_key e vertex_previous

arc_list_min
individua l'arco minore in base al peso e l'ordine lessicografico

rimuovi
rimuove un elemento passato come parametro dalla lista passata per parametro

appendi
unisce i valori di due liste in una lista unica

lunghezza
ritorna la lunghezza di una lista

lock
funzione di appoggio per la funzione preorder

preorder
funzione di appoggio di mst_get

while_heap
funzione che cicla sui valori dello heap e che calcola gli adjs

for_each_vertex
cicla sugli adjs di un vertex ed elabora i valori per vertex_key e vertex_previous


new_graph(G).
Questo predicato inserisce un nuovo grafo nella base-dati Prolog.

delete_graph(G).
Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati Prolog.

new_vertex(G, V).
Aggiunge il vertice V nella base-dati Prolog.

graph_vertices(G, Vs).
Questo predicato è vero quanto Vs è una lista contenente tutti i vertici di G.

list_vertices(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici del grafo G.

new_arc(G, U, V, Weight).
Aggiunge un arco del grafo G alla base dati Prolog.

graph_arcs(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.

vertex_neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi,
arc(G, V, N, W), che portano ai vertici N immediatamente raggiungibili da V

adjs(G, V, Vs).
Questo predicato è vero quando V è un vertice di G e Vs è una lista contenente i vertici,
vertex(G, V), ad esso adiacenti.

list_arcs(G).
Questo predicato stampa alla console dell’interprete Prolog una lista degli archi del grafo G.

list_graph(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici e degli archi del
grafo G.


read_graph(G, FileName).
Questo predicato legge un “grafo” G, da un file FileName e lo inserisce nel data base di Prolog.

write_graph(G, FileName).
write_graph(G, FileName, Type).
Questo predicato è vero quando G viene scritto sul file FileName secondo il valore
dell’argomento Type. Type può essere graph o edges. Se Type è graph, allora G è un
termine che identifica un grafo nella base di dati Prolog; In FileName saranno scritti gli archi
del grafo secondo il formato descitto per read_graph/2. Se Type è edges, allora G è una lista
di archi, ognuno dei quali viene stampato su FileName, sempre secondo il formato descritto
per read_graph/2.


vertex_key(G, V, K).
Questo predicato è vero quando V è un vertice di G e, durante e dopo l’esecuzione dell’algoritmo di
Prim, contiene il peso minimo di un arco che connette V nell’albero minimo; se questo arco non
esiste (ed all’inizio dell’esecuzione) allora K è inf.


vertex_previous(G, V, U).
Questo predicato è vero quando V ed U sono vertici di G e, durante e dopo l’esecuzione
dell’algoritmo di Prim, il vertice U è il vertice “genitore” (“precedente”, o “parent”) di V nel
minimum spanning tree.


mst_prim(G, Source).
Questo predicato ha successo con un effetto collaterale. Dopo la sua prova, la base-dati Prolog ha
al suo interno i predicati vertex_key(G, V, k) per ogni V appartenente a G; la base-dati Prolog
contiene anche i predicati vertex_previous(G, V, U) per ogni V, ottenuti durante le iterazioni
dell’algoritmo di Prim.


mst_get(G, Source, PreorderTree).
Questo predicato è vero quando PreorderTree è una lista degli archi del MST ordinata secondo
un attraversamento preorder dello stesso, fatta rispetto al peso dell’arco.


new_heap(H).
Questo predicato inserisce un nuovo heap nella base-dati Prolog.

delete_heap(H).
Rimuove tutto lo heap (incluse tutte le “entries”) dalla base-dati Prolog


heap_has_size(H, S).
Questo predicato è vero quanto S è la dimensione corrente dello heap.

heap_empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.


heap_not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un elemento.


heap_head(H, K, V).
Il predicato head/3 è vero quando l’elemento dello heap H con chiave minima K è V.

heap_insert(H, K, V).
Il predicato insert/3 è vero quando l’elemento V è inserito nello heap H con chiave K.

heap_extract(H, K, V).
Il predicato extract/3 è vero quando la coppia K, V con K minima, è rimossa dallo heap H.

modify_key(H, NewKey, OldKey, V).
Il predicato modify_key/4 è vero quando la chiave OldKey (associata al valore V) è sostituita da
NewKey.

list_heap(H).
Il predicato richiama listing/1 per stampare sulla console Prolog lo stato interno dello heap
