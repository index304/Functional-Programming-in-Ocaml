(* Zadanie: Przelewanka    *)
open Array

let snd(_, b) = b;; 
(* Liczy nwd liczb a i b, zakłada, że a >= b. *)
let rec nwd(a, b) =  if b = 0 then a else nwd (b, a mod b);;

(* Procedura, która mając tablicę par liczb wyznacza minimalną liczbę czynności 
   potrzebnych do uzyskania opisanej przez nie sytuacji. Jeżeli jej uzyskanie 
   nie jest możliwe, to poprawnym wynikiem jest -1. *)
let przelewanka tab = 
   let n = length tab in 
   let szukana = make n 0 in  (* Tablica zawierająca oczekiwane napełnienie szklanek. *)
   let maxi = make n 0  in    (* Tablica zawierająca maksymalną pojemność szklanek. *)
   begin 
      for i = 0 to (n-1) do 
         szukana.(i) <- snd (tab.(i)); 
         maxi.(i) <- fst (tab.(i)); 
      done
   end; 
   
   let mapa = Hashtbl.create 1000 in (* Mapa, na której zapamiętujemy odwiedzone stany. *)
   (* Sprawdźmy proste warunki początkowe czy da się uzyskać końcowy wynik *)
   let d1 = ref 0 in
   let d2 = ref 0 in  
   let licz = ref 0 in 
   begin 
      for i = 0 to (n-1) do 
         d1 := nwd(max !d1 maxi.(i), min !d1 maxi.(i));
         d2 := nwd(max !d2 szukana.(i), min !d2 szukana.(i)); 
         if szukana.(i) = maxi.(i) || szukana.(i) = 0 then licz := !licz + 1; 
      done 
   end; 
   
   let result = ref (-1) in 
   (* Warunek sprawdzający czy przynajmniej jedna z oczekiwanych szklanek z wodą jest
      pusta lub pełna oraz czy nwd szklanek napełnionych jest równe nwd szklanek
      o oczekiwanej zawartości. *)
   if (!d1 = !d2 && !licz <> 0) || !licz = n || (!d2 mod !d1 = 0 && !licz <> 0) then 
   begin 
      let q = Queue.create () in  
      let wyjdz = ref 0 in 
      Queue.add ((make n 0), 0) q; 
      
   (* Dopoki kolejka jest pusta i dopóki się opłaca, to przeglądamy wszystkie możliwe stany. *)
   while !wyjdz <> 1 && Queue.length q > 0 do 
      begin    
      let para = Queue.take q in 
      let top = fst para in 
      let dlugosc = snd para in  
      if top = szukana then (result := dlugosc; wyjdz := 1);  
      let kopia = copy top in 
      if !wyjdz = 0 && ((Hashtbl.mem mapa kopia) = false) then
         begin 
            Hashtbl.add mapa (copy kopia) (true);
            (* Dodajemy stan kiedy rozpatrywana szklanka jest pełna. *)
            for i = 0 to (n-1) do 
               if kopia.(i) = 0 then 
               begin 
                  kopia.(i) <- maxi.(i);
                     if Hashtbl.mem mapa kopia = false then
				      Queue.add ((copy kopia), dlugosc + 1) q;
                  kopia.(i) <- top.(i); 
               end
            done;
            
            (* Dodajemy stan kiedy rozpatrywana szklanka jest pusta. *)
            for i = 0 to (n-1) do 
               if kopia.(i) = maxi.(i) then
               begin
                  kopia.(i) <- 0; 
                  if Hashtbl.mem mapa kopia = false then
                     Queue.add ((copy kopia), dlugosc + 1) q;
                  kopia.(i) <- top.(i); 
               end
            done;
            
            (* Dodajemy stan kiedy przelewamy wodę ze szklanki o numerze i 
               do szklanki o numerze j, przy czym i != j. *)
            for i = 0 to (n-1) do 
               for j = 0 to (n-1) do 
                  if i <> j && kopia.(i) <> 0 && kopia.(j) <> maxi.(j) then
                     let pom = kopia.(j) in 
                     kopia.(j) <- min maxi.(j) (kopia.(j) + kopia.(i));
                     kopia.(i) <- max (kopia.(i) - maxi.(j) + pom) 0; 

                     if Hashtbl.mem mapa kopia = false then
                        Queue.add ((copy kopia), dlugosc + 1) q;
                
                     kopia.(i) <- top.(i); 
                     kopia.(j) <- top.(j);  
                done
             done
          end
       end 
   done end;  
   !result
;;
