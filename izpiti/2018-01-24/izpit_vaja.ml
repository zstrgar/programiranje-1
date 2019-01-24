(* 1. NALOGA *)

(*a*)

let rec izpisi_vsa_stevila sez =
  match sez with
  | [] -> ()
  | x :: xs -> print_int x; izpisi_vsa_stevila xs

(*b*)

let rec map2_opt f sez1 sez2 =
  match sez1, sez2 with
  | (_, []) -> None
  | ([], _) -> None
  | (x :: xs, y :: ys) -> 
      Some ((f x y) :: (map2_opt f xs ys))

(* 2. NALOGA *)

(*a*)
type filter_tree = 
  | Vozlisce of filter_tree * int * filter_tree
  | List of int list

let primer = Vozlisce(Vozlisce(List [1], 5, List []), 10, (Vozlisce(List [], 15, List[19; 20])));;

(*b*)

let rec vstavi k filter_tree =
  match filter_tree with 
  | List [] -> List [k]
  | List (x :: xs) -> List (k :: x :: xs)
  | Vozlisce(levo, v, desno) ->
    if k <= v then
      Vozlisce(vstavi k levo, v, desno)
    else
      Vozlisce(levo, v, vstavi k desno)
  
(*c*)

let rec vstavi_seznam sez filter_tree =
  match sez with
  | [] -> filter_tree
  | x :: xs -> vstavi_seznam xs (vstavi x filter_tree)

(*d*)
(*
let rec preveri filter_tree =
  match filter_tree with
  | Vozlisce(levo, v, desno) ->
    match levo, desno with
    | List [], List [] -> true
    | List (x :: xs), List(y :: ys) ->
      if (x <= v) && (y > v) then 
        preveri Vozlisce(List(xs), v, List(ys))
      else
        false 
  *)

  (*
(* 3. NALOGA *)
type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  type t

  val id : t 
  val uporabi : t -> vektor -> vektor
  val iz_matrike : matrika -> t
  val iz_funkcije : (vektor -> vektor) -> t
  val kompozitum : t -> t -> t
end

(*a*)

module Matrika : Linearna = struct

  type t = matrika

  let id = (1, 0, 0, 1)
  let uporabi (a, b, c, d) (x, y) = 
    (a * x + b * y, c * x + d * y)
  let iz_matrike matrika = matrika
  let iz_funkcije f = 
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
  let kompozitum (a, b, c, d) (e, f, g, h) = 
    (a*e + b*g, a*f + b*h, c*e + d*g, c*f + c*h)
end


(*b*) 

module Funkcija : Linearna = struct

  type t = int*int -> int*int

  let id = (fun x -> x)
  let uporabi v f = f v
  let iz_matrike (a, b, c, d) = fun (x, y) -> (x * a + y * b, x * c + y * d)
  let iz_funkcije f = f
  let kompozitum f g = fun x -> f (g x)
end

*)
