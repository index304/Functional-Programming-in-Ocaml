* Zadanie:  Origami *)

(** Punkt na płaszczyźnie *)
type point = float * float;; 

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int;; 

let eps = 1e-12;;

(** [prostokat (a, b) (c, d)] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych. *)
let prostokat (a, b) (c, d) = 
  fun (px, py) -> 
    if (a -. eps <= px && px <= c +. eps) && (b -. eps <= py && py <= d +. eps)
       then 1 
    else 0
;;

(* Funkcja zwraca kwadrat liczby x. *)
let sq x = x *. x;;

(** [kolko (a,b) r] zwraca kartkę, reprezentującą kółko domknięte o środku
 w punkcie (a, b) i promieniu [r]. *)
let kolko (a, b) r = 
  fun (px, py) ->
     if sq (px -. a) +. sq (py -. b) -. eps < r*.r
      then 1 
    else 0
;; 

(* Funkcja sprawdzająca czy x jest równy zero z dokładnością do epsilona. *)
let spr x = if x +. eps >= 0. && x -. eps <= 0. then 0. else 1.;;

(* Funkcja: zwraca zero jeśli punkt (x3, y3) leży na prostej wyznaczonej przez punkty
   (x1, y1), (x2, y2);
   zwraca jeden jeśli punkt (x3, y3) leży po lewej stronie tej prostej; 
   zwraca -1 jeśli punkt (x3, y3) leży po prawej stronie prostej.  *)
let strona (x1, y1) (x2, y2) (x3, y3) = 
    let  ilwek = (x3-.x1)*.(y2-.y1) -. (y3-.y1)*.(x2-.x1) in 
    if  spr ilwek = 0.
     then 0
    else  if ilwek <= 0.
     then -1
    else 1
;;

(*  Zwraca punkt symetryczny do punktu (x3, y3) względem prostej (x1, y1), (x2, y2).
    Wyznaczam go korzystając z własności iloczynu skalaranego.
    Najpierw znajduję punkt (x,y), który jest rzutem punktu (x3, y3) na prostą
    (x1, y1), (x2, y2), a następnie przesuwając punkt (x,y) o odpowiedni wektor
    otrzymuję szukany punkt.  *)
let sym (x1, y1) (x2, y2) (x3, y3) = 
  let licznik = (x3 -. x1)*.(x2 -. x1) +. (y3 -. y1)*.(y2 -. y1) in 
  let mianownik = (x1 -. x2)*.(x1 -. x2) +. (y1 -. y2)*.(y1 -. y2) in 
  let skalarny = licznik /. mianownik in 
  let x = x1 +. (x2 -. x1)*. skalarny in 
  let y = y1 +. (y2 -. y1)*. skalarny in 
  (x3 +. 2.*.(x -. x3), y3 +. 2.*.(y -. y3))
;; 

(** [zloz p q k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty p i q. *)
let zloz p q k = 
  fun r -> let war = strona p q r in 
  if war = 0
    then k r
  else if war = 1
    then 0
  else k r + k (sym p q r)
;;

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych z listy. *)
let skladaj l k = 
   List.fold_left (fun acc (p, q) -> zloz p q acc) k l
;;

