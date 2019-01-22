(* 1. NALOGA *)

(* a *)

let uporabi f x = f x 

(* b *)

let ibaropu x f = f x


(* c *)

let zacetnih n list =
  let rec aux acc n list =
    if List.length list < n then None
    else
      match list with
      | [] -> Some (List.rev acc)
      | x :: xs ->
        if n <= 0 then Some (List.rev acc)
        else
          aux (x :: acc) (n - 1) xs
  in
  aux [] n list
      
(* test
let a = [1; 5; 6; 1; 2; 3; 8; 0];;
zacetnih 4 a;;
*)


(* 2. NALOGA *)
type 'a neprazen_sez = 
  | Konec of 'a   
  | Sestavljen of 'a * 'a neprazen_sez  (* sestavljen iz 'a in nepraznega 
  (torej sestavljen ali pa konec)*)

(* a *)

let prvi neprazen = 
  match neprazen with
  | Konec x -> x
  | Sestavljen (x, xs) -> x

let rec zadnji neprazen = 
  match neprazen with
  | Konec x -> x
  | Sestavljen(x, xs) -> zadnji xs

(* test
let sez = Sestavljen(3, Sestavljen(5, Sestavljen(8, Konec(9))));;
prvi sez;;
zadnji sez;;
*)
(* b *)

let rec dolzina neprazen = 
  match neprazen with
  | Konec x -> 1
  | Sestavljen(x, xs) -> 1 + dolzina xs

(* test
let sez = Sestavljen(3, Sestavljen(5, Sestavljen(8, Konec(9))));;
dolzina sez;;
*)

(* c *)

let rec pretvori_v_seznam neprazen =
  match neprazen with
  | Konec x -> x :: []
  | Sestavljen(x, xs) -> x :: (pretvori_v_seznam xs)

(* d *)

let zlozi f x neprazen =
  let sez = pretvori_v_seznam neprazen in
  List.fold_left f x sez

let rec zlozi2 f s neprazen =
  match neprazen with
  | Konec x -> f s x
  | Sestavljen(x, xs) -> zlozi2 f (f s x) xs
