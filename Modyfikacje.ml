(* Modyfikacje drzew *)


(* Typ t składa się kolejno z: 
   1. Lewego poddrzewa 2. Przedziału liczb (a, b), gdzie a <= b
   3. Prawego poddrzewa 4. Wysokości drzewa 
   5. Ilość wszystkich liczb całkowitych zawartych w poddrzewie. 
*)
type t = 
   | Node of t * (int*int) * t * int * int
   | Empty
;;


let empty = Empty;; 

let is_empty x = x = Empty;;

(* Funkcja zwracająca wyskość drzewa *)
let height x = 
  match x with
  |Empty -> 0
  |Node(_, _, _, h, _) -> h
;; 

(* Funkcie zwracające kolejno pierwszy i drugi element pary *)
let fst (a, b) = a;; 
let snd (a, b) = b;; 

(* Funkcje zwracające kolejno pierwszy i ostatni element potrójnej krotki *)
let fst_3 (a, b, c) = a;;
let trd_3 (a, b, c) = c;;

(* Funkcja normalizująca dodawanie dwóch liczb, jeśli ich suma przekracza 
   max_int to funkcja zwraca max_int, w innym przypadku zwracana jest ich suma *)
let norm_max a b = 
  if a >= (max_int - b) || b >= (max_int - a) then 
    max_int 
  else 
    a + b
;;

(* Funkcja zwracająca ilość liczb na przedziale (a, b) *)
let l_interval (a, b) =
  if 0 <= b && a < 0
    then norm_max 1 (norm_max b (-a))
  else b - a + 1
;;

(* Funkcja zwracają ilość wszystkich liczb w poddrzewie *)
let ile_ponizej x = match x with
  |Empty -> 0
  |Node(_, _, _, _, n)-> n
;; 

(* Funkcja zwracająca ilość liczb w wierzchołku *)
let ile_w_wierz x = match x with 
  |Empty -> 0
  |Node(_,(a,b),_, _, _)-> l_interval (a,b)
;; 

(* Funkcja tworząca nowy węzeł w drzewie *)
let make l k r = Node (l, k, r, max (height l) (height r) + 1,
   norm_max (norm_max (ile_ponizej l) (ile_ponizej r)) (norm_max (ile_w_wierz l) (ile_w_wierz r)))

(* Funkcja porównująca dwa przedziały *)
let cmp (a, b) (c, d) = 
  if b < c 
    then -1
  else if a > d 
    then 1
  else 0 
;; 

(* Funkcja odpowiadająca za równoważenie drzewa AVL *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r
;;

(* Funkcja dodająca do drzewa przedział rozłączny z przedziałami 
   liczb już znajdującymi się w drzewie *)
let rec add_one x s = match s with
| Node (l, v, r, h, tmp) ->
    let c = cmp x v in
    if c = 0 then Node (l, v, r, h, tmp)
    else if c < 0 then
      let nl = add_one x l in
      bal nl v r
    else
      let nr = add_one x r in
      bal l v nr
| Empty -> Node (Empty, x, Empty, 1, 0)

(* Funkcja pomocnicza do do funkcji sprawdzającej czy dany przedział (a, b) jest
   rozłączny, sprawdza ona czy nie nastąpiło przekręcenie x i y przy dodawaniu do tych
   liczb kolejno -1 i +1 *)
let kresy (x, y) = 
  if x = max_int || y = min_int 
   then false 
  else
   true
;;

(* Funkcja sprawdzająca czy dany przedział (x,y) jest rozłączny z wszystkimi
   przedziałami w drzewie *)
let spr_czy_rozlaczny (x, y) s = 
  if kresy(x, y) = false then false 
  else 
  let rec pom (x, y) s = match s with
    |Empty -> true
    |Node(l, v, r, _, _) -> let c = cmp (x, y) v
    in 
    if c = 0
     then false
    else if c = -1
     then pom (x, y) l 
    else
     pom (x, y) r 
    in pom (x, y) s
;;  

(* Funkcja sprawdzająca czy element x należy do drzewa *)
let mem  x s = 
  let rec znajdz x s = match s with
    |Empty -> false
    |Node(l, v, r, _, _) -> let c = cmp (x,x) v in 
    if c = 0 
      then true
    else if c = -1
      then znajdz x l 
    else 
      znajdz x r
    in znajdz x s
;;

(* Funkcja iterująca się po wszystkich elementach drzewa *)
let iter f set =
  let rec loop s = match s with
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set
;; 

(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order. *)
let fold f set acc =
  let rec loop acc s = match s with 
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set
;;

(* Funkcja zwracająca listę z uporządkowanymi rosnąco przedziałami liczb należącymi do drzewa *)
let elements t = 
   let rec loop acc s = match s with
    | Empty -> acc
    | Node(l, v, r, _, _) -> loop (v::loop acc r) l in
   loop [] t
;;

(* Funkcja zwracająca ilość liczb w drzewie mniejszcyh bądź równych x *)
let below x t =
  let rec zlicz x t acc = match t with
  | Empty -> acc
  | Node (l, (a, b) , r, _, _) ->
    let c = cmp (x, x) (a, b) in
    if c = 0
        then norm_max acc (norm_max (ile_ponizej l) (norm_max (ile_w_wierz l) (l_interval (a, x))))
    else if c = -1
        then zlicz x l acc
    else  
      zlicz x r (norm_max (norm_max (ile_ponizej l) (ile_w_wierz l)) (norm_max (ile_w_wierz t) acc))
    in zlicz x t 0 
;;

(* Funkcja łącząca dwa zrównoważone drzewa AVL *)
 let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

(* Funkcje sprawdzające czy x jest kolejno pierwszym lub ostatnim elementem
   przedziału liczb v *)
let first_left x v = x = fst v;; 
let last_right x v = x = snd v;; 

(* Funkcja zwracająca potrójną krotkę, której pierwszym elementem jest drzewo z wszystkimi
   liczbami mniejszymi od x, drugim element zawiera informację czy x należy do drzewa, 
   trzeci element zawiera drzewo z wszystkimi elementami większymi od  x *)
let split x tree =
  let rec loop x tr = match tr with
    | Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = cmp (x,x) v in
        if c = 0 then
          if (first_left x v) && (last_right x v)
            then (l, true, r)     
          else if (first_left x v)
            then (l, true, add_one (x+1, snd v) r)
          else if (last_right x v)
            then (add_one (fst v, x-1) l , true, r)
          else 
            (add_one (fst v, x -1) l, true, add_one(x+1, snd v) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in
  let setl, pres, setr = loop x tree in
  setl, pres, setr
;;

(* Funkcja łącząca dwa drzewa o zbliżonej wysokości *)
let rec merge t1 t2 =
      match t1 , t2 with
      | (Empty, _) -> t2
      | (_, Empty) -> t1
      | (Node(l_t1, v_t1, r_t1, h_t1, _), Node(l_t2, v_t2, r_t2, h_t2, _)) ->
        if h_t2 + 2 < h_t1 
          then bal l_t1 v_t1 (merge r_t1 t2)
          else bal (merge t1 l_t2) v_t2 r_t2
;;

(* Funkcja usuwająca z drzewa przedział (x,y), wykorzystuje ona dwukrotnie funkcje split *)
let remove (x, y) s = 
  let nl = fst_3 (split x s) in 
  let nr = trd_3 (split y s) in
    merge nl nr
;;

(* Funkcja zwracająca przedział zawierający maksymalny element w drzewie *)
let rec find_max s = match s with 
  |Empty -> (0, 0)
  |Node(l, v, r, _, _) -> if r = Empty then v 
  else find_max r
;; 
(* Funkcja zwracająca przedział zawierający minimalny element w drzewie *)
let rec find_min s = match s with 
  |Empty -> (0, 0)
  |Node(l, v, r, _, _) -> if l = Empty then v 
  else find_min l
;; 

(* Funkcja zwracają x - 1 w przypadku gdy x-1 nie przekroczy minimalnej wartości *) 
let spr_x x = 
   if x = min_int 
      then min_int 
   else 
      x - 1;;
      
(* Funkcja zwracają y + 1 w przypadku gdy y+1 nie przekroczy maksymalnej wartości *) 
let spr_y y =
   if y = max_int
      then max_int
   else
      y + 1
;; 

(* Funkcja dodająca do drzewa przedział (x, y) *)
let rec add (x, y) s = 
  let spr = spr_czy_rozlaczny (x-1, y+1) s in 
  if spr then
   add_one (x,y) s 
  else let pocz = fst_3 (split x s) in                (* drzewo z liczbami mniejszymi od x *)
       let kon  = trd_3 (split y s) in                (* drzewo z liczbami większymi od y *)
       
       if pocz = Empty && kon = Empty
         then add_one (x,y) empty
       
       else if pocz = Empty then 
          let r_min  = find_min kon in                (* sprawdzenie czy minimalna wartość w drzewie kon wynosi y+1 *)
          if (fst r_min = spr_y y) then 
          join empty (x,snd r_min) (remove r_min kon) 
        else 
          join empty (x, y) kon
       
       else if kon = Empty then 
         let l_max  = find_max pocz in                (* sprawdzenie czy maksymalna wartość w drzewie pocz wynosi x-1 *)
         if (snd l_max  = spr_x x) then 
         join (remove l_max pocz) (fst l_max, y) empty
        else
         join pocz (x,y) empty 
       
       else 
         let l_max  = find_max pocz in                (* sprawdzenie czy maksymalna wartość w drzewie pocz wynosi x-1 *)
        let r_min  = find_min kon in                  (* sprawdzenie czy minimalna wartość w drzewie kon wynosi y+1 *)
        if (snd l_max = spr_x x) && (fst r_min = spr_y y) then 
          join (remove l_max pocz) (fst l_max, snd r_min) (remove r_min kon)
        else if (snd l_max = spr_x x) then
          join (remove l_max pocz) (fst l_max, y) kon
        else if (fst r_min = spr_y y) then 
          join pocz (x, snd r_min) (remove r_min kon)
        else 
          join pocz (x, y) kon
;;
