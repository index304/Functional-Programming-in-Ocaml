(* Zadanie: Arytmetyka Przybliżonych Wartości *)

(* Struktura wartosc moze byc zbiorem majacym jedna z ponizszych postaci:
1. Normalny przedzial [a, b] gdzie a<=b. Ten zbiór przyjmuje reprezentacje
   {left = a; right = b; podwojny  = false; not_number = false}
2. Suma przedzialow postaci [neg_infinity, a] U [b, infinity] gdzie a<b.
   Taki zbior reprezentuje jako {left = a; right = b; podwojny = true; not_number = false}
3. Przedział pusty tzn. kiedy a = nan lub b = nan. Taki zbiór reprezentuje jako:
	{left = 0.; right = 0.; podwojny = false; not_number = true} *)
	
type wartosc = {
   left : float;
   right : float;
   podwojny : bool;
   not_number : bool; 
};;
 
(* Obiekty pomocnicze *)
let empty = {left = 0.; right = 0.; podwojny = false; not_number = true};;
let all = {left = neg_infinity; right = infinity; podwojny = false; not_number = false};; 


(* Funkcja sprawdza czy zbiór jest pusty *)   
let zbior_empty (x:wartosc) (y:wartosc) = 
   x.not_number || y.not_number
;;

(* zamienia wszystkie wartości na przeciwne *)
let zmien_na_minus (x:wartosc) =
   {left = (-1.) *. x.right; right = (-1.) *. x.left; podwojny = x.podwojny; not_number = x.not_number}
;; 
   
(* Konstruktory, specyfikacja tak jak w treści zadania *)   
let wartosc_od_do x y = {
   left = x;
   right = y; 
   podwojny = false; 
   not_number = false; 
};;

let wartosc_dokladna x =
   wartosc_od_do x x
;;

let wartosc_dokladnosc x p = 
   if x < 0. then
      wartosc_od_do (x +. (x *. (p /. 100.))) (x -. (x *. (p /. 100.)))
   else 
      wartosc_od_do (x -. (x *. (p /. 100.))) (x +. (x *. (p /. 100.)))
;; 

(* Selektory, specyfikacja tak jak w treści zadania *)
let in_wartosc x y =
   if zbior_empty x x then false
   else if x.podwojny = true then 
      if x.left >= y || x.right <= y then true 
      else false
   else 
      if x.left <= y && x.right >=y  then true
      else false
;;

(* Zwraca minimalną wartość obiektu x *)
let min_wartosc x = 
   if x.not_number = true then nan
   else 
      if x.podwojny = true then neg_infinity 
      else x.left 
;; 

(* Zwraca maksymalną wartość obiektu x *)
let max_wartosc x = 
   if x.not_number = true then nan
   else 
      if x.podwojny = true then infinity 
      else x.right
;;

(* Zwraca średnią wartość obiektu x *)
let sr_wartosc x=
   if x.not_number = true then nan
   else 
      if x.podwojny = true then nan
      else (x.right +. x.left) /. 2.
;;

(* Sprawdza czy obiekt jest (-inf, inf) *)
let sprawdz_czy_all (x:wartosc)=
   if x.podwojny && x.left >= x.right then
      all 
   else x
;;

(* MODYFIKATORY *)
(* dodaje dwie wartości x i y *)
let plus x y =
   if zbior_empty x y then
      empty
   else 
      if x.podwojny && y.podwojny then all
      else if x.podwojny && (y.left = neg_infinity || y.right = infinity)
         then all
      else if  y.podwojny && (x.left = neg_infinity || x.right = infinity)
         then all 
      else if x.podwojny then
         sprawdz_czy_all {left = x.left +. y.right; right = x.right +. y.left; podwojny = true; not_number = false}
      else if y.podwojny then
         sprawdz_czy_all {left = y.left +. x.right; right = y.right +. x.left; podwojny = true; not_number = false}
      else 
         {left = x.left +. y.left; right = x.right +. y.right; podwojny = false; not_number = false}
;;  

(* Od wartości x odejmuje y *)
let  minus x y = 
   if zbior_empty x y then
      empty
   else 
      plus x (zmien_na_minus y)
;;
 
(* FUNKCJE WŁASNE *)

(* Zwraca minimum z dwóch liczb *)
 let min (x:float) (y:float) = 
   if x <= y then x 
   else y
;; 

(* Zwraca maksimum z dwóch liczb *)
let max (x:float) (y:float) =
   if x >= y then x
   else y
;; 
 
(* Rozwiązuje przypadek 0 * inf = 0 i 0 * (-inf) = 0 *)
 let zero_inf  (a:float) (b:float) = 
   if (a = infinity || a = neg_infinity) && b = 0. then
      0.
   else if (b = infinity || b = neg_infinity) && a = 0. then
      0.
   else
      a *. b
 ;; 
 
 (* Zwraca mininimum z czterech liczb *)
 let min_4 (a:float) (b:float) (c:float) (d:float) = 
   min (min (zero_inf a c) (zero_inf a d)) (min (zero_inf b c) (zero_inf b d) )
 ;; 

(* Zwraca maksimum z czterech liczb *)
 let max_4 (a:float) (b:float) (c:float) (d:float) = 
   max (max (zero_inf a c) (zero_inf a d)) (max (zero_inf b c) (zero_inf b d) )
 ;; 

(* Mnoży dwie wartości x i y w przypadku gdy x jest postaci (-inf, a)_(b,inf) 
   oraz y jest postaci (c, d) *)
let zwroc_mnozenie (x:wartosc) (y:wartosc) = 
   if y.left >= 0. && y.right >= 0. then
   sprawdz_czy_all{   
                  left = max (zero_inf x.left y.left) (zero_inf x.left y.right); 
                  right = min (zero_inf x.right y.left) (zero_inf x.right y.right); 
                  podwojny = true; 
                  not_number = false;
               }
   else if y.left <= 0. && y.right >= 0. then all
   else 
      sprawdz_czy_all{
                     left = max (zero_inf x.right y.left) (zero_inf x.right y.right); 
                     right = min (zero_inf x.left y.left) (zero_inf x.left y.right); 
                     podwojny = true; 
                     not_number = false;
                  }
;;    

(* Funkcja zwracająca wartość (1,1) /. x, pomaga ona przy dzieleniu dwóch przedziałów poprzez
   pomnożenie pierwszego przedziału przez odwrotność drugiego przedziału *)
let odwrotnosc x = 
   if (sprawdz_czy_all x) = all 
      then all 
   else 
      if x.podwojny = true then
         if x.left > 0. then
            {left = 1. /. x.right; right = 1. /. x.left; podwojny = true; not_number = false}
         else if x.left = 0. then
            {left = neg_infinity; right = 1. /. x.right; podwojny = false; not_number = false}
         else if x.left < 0. && x.right > 0. then
            {left = 1. /. x.left; right  = 1. /. x.right; podwojny = false; not_number = false}
         else if x.left < 0. && x.right = 0. then
            {left = 1. /. x.left; right = infinity; podwojny = false; not_number = false}
         else    (* a < 0 && b < 0) *)
            {left = 1. /. x.right; right = 1. /. x.left;  podwojny = true; not_number = false}
      else 
         if x.left < 0. && x.right > 0. then
            {left = 1. /. x.left; right = 1. /. x.right; podwojny = true; not_number = false}
         else if x.left = 0. && x.right > 0. then
            {left = 1. /. x.right; right = infinity; podwojny = false; not_number = false}
         else if x.left < 0. && x.right = 0. then
            {left = neg_infinity; right = 1. /. x.left; podwojny = false; not_number = false}
         else if x.left < 0. && x.right < 0. then
            {left = 1. /. x.right; right = 1. /. x.left; podwojny = false; not_number = false}
         else  (* ( x.left > 0. && x.right > 0. then *)
            {left = 1. /. x.right; right = 1. /. x.left; podwojny = false; not_number = false}
;; 

(* Mnoży dwa przedziały x i y *)
let razy x y = 
   if zbior_empty x y then
      empty
   else if (x.left = 0. && x.right = 0.) || (y.left = 0. && y.right = 0.)
      then {left = 0.; right = 0.; podwojny =false; not_number = false;}
   else 
      if x.podwojny = true && y.podwojny = true then
         sprawdz_czy_all {left = max (zero_inf x.left  y.right) (zero_inf x.right  y.left);
                          right = min (zero_inf x.left y.left) (zero_inf x.right y.right); 
                          podwojny = true; 
                          not_number = false;}
      else if x.podwojny = true then
          zwroc_mnozenie x y
      else if y.podwojny = true then
         zwroc_mnozenie y x
      else 
         {
            left = min_4 x.left x.right y.left y.right;
            right = max_4 x.left x.right y.left y.right;
            podwojny = false;
            not_number = false; 
         }
;;  

(* Dzieli przedział x przez y *)
let podzielic x y = 
   if zbior_empty x y then
      empty
   else 
      if y.left = 0. && y.right = 0. then 
         {left = nan; right = nan; podwojny = false; not_number = true}
      else
         razy x (odwrotnosc y)
;;              
