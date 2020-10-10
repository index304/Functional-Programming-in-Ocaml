
(* Definicja kolejki-tworzy ją kolejno wartość w danym węźle, 
   długość najkrótszej prawej ścieżki w drzewie, lewy syn i prawy syn 
   lub kolejka jest równa Null - jest to przypadek kiedy jest pusta *)
type 'a queue = |Null|Node of 'a * int * 'a queue * 'a queue;; 

(* Definicja pustej kolejki *)
let empty = Null;;

(* Definicja rzucanego wyjątku w przypadku gdy kolejka jest pusta i chemy usunąć z niej element*)
exception Empty;;   

(* Funkcje zwracające kolejno pierwszy, drugi, trzeci i czwarty element kolejki *)
let wartosc q  = match q with  Node(a, _, _, _) -> a |Null -> raise Empty;;
let len     q  = match q with |Null -> 0    |Node(_, b, _, _) -> b;;
let left    q  = match q with |Null -> Null |Node(_, _, c, _) -> c;;
let right   q  = match q with |Null -> Null |Node(_, _, _, d) -> d;;

(* Funkcja zwracająca kolejke, w której jest zachowany warunek lewicowości oraz 
   zaaktualizowana jest wartość najkrótszej prawej ścieżki *)
let normalizuj q = 
   match q with 
     |Null -> Null
     |Node(war, _, lewy, prawy) -> 
        if (len lewy) < (len prawy) then
           Node(war, (len lewy) + 1, prawy, lewy)
        else
           Node(war, (len prawy) + 1, lewy, prawy)
;; 

(* Funkcja łącząca dwie kolejki *)
let rec join q1 q2 = 
   if q1 = Null then q2
   else if q2 = Null then q1
   else 
      if (wartosc q1) <= (wartosc q2) then 
      let nq = join (right q1) q2 in 
         normalizuj (Node( (wartosc q1), (len q1), (left q1), nq))
      else
         join q2 q1
;;

(* Funkcja dodająca do kolejki nowy element *)
let add el q = 
   let q2 = Node(el, 1, Null, Null) in
      join q2 q
;; 

(* Funkcja usuwająca najmniejszy element z kolejki *)
let delete_min q = 
   match q with | Null -> raise Empty
                | Node(war, len, q1, q2) -> (war, join q1 q2)
;;

(* Funkcja sprawdzająca czy kolejka jest pusta *)
let is_empty q = q = Null;; 
