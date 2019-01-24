(* === 2. NALOGA === *)

(* a *)

type 'a mm_drevo =
  | Prazno
  | Sestavljeno of 'a mm_drevo * ('a * int) * 'a mm_drevo

let leaf (el, st) = Sestavljeno (Prazno, (el, st), Prazno)

(* b *)

let rec vstavi a mm_drevo =
  match mm_drevo with
  | Prazno -> Sestavljeno(Prazno, (a, 1), Prazno)
  | Sestavljeno(levo, (el, st), desno) ->
    if a == el then Sestavljeno(levo, (el, st + 1), desno)
    else if a < el then Sestavljeno(vstavi a levo, (el, st), desno)
    else
      Sestavljeno(levo, (el, st), vstavi a desno)

(* c *)

let rec multimnozica_iz_seznama sez = 
  match sez with
  | [] -> Prazno
  | x :: [] -> vstavi x Prazno
  | x :: xs -> vstavi x (multimnozica_iz_seznama xs)

(* d *)

let rec velikost_multimnozice mm_drevo =
  match mm_drevo with
  | Prazno -> 0
  | Sestavljeno(levo, (el, st), desno) -> 
    velikost_multimnozice levo + st + velikost_multimnozice desno

(* e *)

let rec repeat x n =
  (* pomozna funkcija iz vaj (vrne seznam n ponovitev elementa x)*) 
  match n <= 0 with
  | true -> []
  | false -> x :: (repeat x (n-1))

let rec seznam_iz_multimnozice mm_drevo =
  match mm_drevo with
  | Prazno -> []
  | Sestavljeno(levo, (el, st), desno) -> 
    (seznam_iz_multimnozice levo) @ (repeat el st) @ (seznam_iz_multimnozice desno)
    
(*testni primeri*)
let a = [2;5;1;4;1;1;2;8;8];;
let multi_a = Sestavljeno(leaf (1, 3), (2, 2), Sestavljeno(leaf (4,1), (5,1), leaf (8,2)));;
let b = [1;1;1;1;9;7;4;3;2;0;0;0;1;2;3;4;5;6;7;8];;
let multi_b = multimnozica_iz_seznama b;;

