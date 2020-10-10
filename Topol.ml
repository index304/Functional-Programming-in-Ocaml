(* Zadanie: Sortowanie Topologiczne aas*)
open PMap

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne
   
(* Funkcja pomocnicza przy rzucania wyjątku. *)
let g y = ()

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol lista = 
   (* Tworzę listę all zawierającą wszystkie wierzchołki grafu. *)
   let i = ref 0 in
   let all = ref [] in 
   List.iter ( fun (v, l2) ->
   all := v::(!all);List.iter (fun x-> all := x::(!all);) l2;) lista; 
   all := List.rev (!all); 
   
   (* Za pomocą modułu pMap skaluję wszystkie wierzchołki od 0 do n-1, gdzie n to liczba
      wszystkich wierzchołków w grafie. *)
   let mapa = ref empty in 
   let mapa2 = ref empty in
   List.iter ( fun x -> try  g (find x !mapa) 
   with Not_found -> mapa := add x !i !mapa; mapa2 := add !i x !mapa2; i := !i + 1) !all;
   
   (* Tworzę graf z przeskalowanymi wartościami. *)
   let graph = Array.make ( !i + 2 ) [] in
   List.iter ( fun (v, l2) ->
   List.iter ( fun x -> 
   graph.(find v !mapa) <- (find x !mapa)::graph.(find v !mapa);) l2;) lista; 
   
   (* Tworzę tablicę, która przyporządkowuje każdemu wierzchołkowi jego stopień wejścia. *)
   let deg = Array.make ( !i + 2 ) 0 in 
   let j = ref 0 in
   while !j < !i do 
      List.iter ( fun x -> deg.(x) <- deg.(x) + 1) graph.(!j);
      j := !j + 1;
   done;
   
   (* Wrzucam na kolejkę wszystkie wierzchołki o stopniu wejścia równym zero. *)
   let q = Queue.create () in 
   let j = ref 0 in 
   while !j < !i do 
      if deg.(!j) = 0 then Queue.add !j q;
      j := !j + 1;
   done;
   let wypisz = ref [] in  (* Tablica zawierająca wierzchołki w porządku topologicznym (odwróconym)*)
   
   (* Na początku kolejka q zawiera wszystkie wierzchołki o stopniu wejścia równym zero,
      następnie dopóki mogę zdejmuje z kolejki wierzchołek i usuwam wszystkie krawędzie,
      które wychodziły z tego wierzchołka. Jeśli stopień wejścia wierzchołka wcześniej połączonego
      z top jest równy zero to wrzucam go na kolejkę. *)
   while (Queue.length q) > 0 do 
      let top = Queue.take q in 
      wypisz := (find top !mapa2)::!wypisz;
      let k = ref 0 in 
      let x = ref graph.(top) in 
      while !k < (List.length graph.(top)) do 
         deg.(List.hd !x) <- deg.(List.hd !x) - 1; 
         if deg.(List.hd !x) = 0 then Queue.add (List.hd !x) q; 
         k := !k + 1; 
         x := List.tl !x;  
      done; 
   done; 
   
   (* Jeżeli został jakiś wierzchołek o stopniu wejścia większym od zera to znaczy, ze w grafie był
      cykl i rzucam wyjątek. *)
   let j = ref 0 in 
   while !j < !i do 
      if deg.(!j) > 0 then raise Cykliczne; 
      j := !j + 1;
   done; 
   
   (* Zwracam liste zawierająco posortowane topologicznie wierzchołki. *)
   List.rev !wypisz
;; 

