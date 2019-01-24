(* === 1. NALOGA === *)

(* a *)

let podvoji_vsoto x y = 2 * (x + y)

(* b *)

let povsod_vecji (a, b, c) (x, y, z) =
  if a >= x && b >= y && c >= z then true
  else false

(* c *)

let uporabi_ce_lahko f opt =
  match opt with
  | None -> None
  | Some x -> Some (f x )

(* d *)

let stevilo_pojavitev a sez =
  (*pomozna funkcija, ki vrne stevilo ponovitev elementa a v seznamu sez*)
  let rec aux a sez acc =
    match sez with
    | [] -> acc
    | x :: xs -> 
      if x = a then aux a xs (acc + 1)
      else aux a xs acc
  in
  aux a sez 0

let pojavi_dvakrat a sez =
  if stevilo_pojavitev a sez == 2 then true
  else false

(* e *)

let izracunaj_v_tocki a sez_f =
  let rec aux a sez_f acc =
    match sez_f with
    | [] -> List.rev acc
    | f :: fs -> aux a fs ((f a) :: acc)
  in
  aux a sez_f []

(* f *)

let eksponent x p =
  let rec aux x p ze_izracunano =
    match p with
    | 0 -> ze_izracunano
    | a -> aux x (a - 1) (ze_izracunano * x)
  in
  aux x p 1  


