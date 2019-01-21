(* ========== Vaje 6: Dinamično programiranje  ========== *)

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]


let max_cheese cheese_matrix = 
  let max_r = Array.length cheese_matrix in   (*koliko je vrstic*)
  let max_c = Array.length cheese_matrix.(0) in   (* koliko je stolpcev, pogledal smo samo eno vrstico, to bi failal, če bi imel prazen seznam, to bomo zdej ignoriral*)
  let rec max_cheese' r c =
    if r >= max_r || c >= max_c then
      0     (* treba je prevert, če je res kul da je 0 (tokrat je ker gledamo maximalno vsoto in 0 bi zmeri manjša, oz zadošča tudi za matriko 1x1*)
    else
      let right = max_cheese' r (c + 1) in
      let down = max_cheese' (r + 1) c in
      let our_cheese = cheese_matrix.(r).(c) in
      our_cheese + max right down
    in
   max_cheese' 0 0

(* ta zadevica bo počas delala za zelo veliko matriko, ker vse računa. nič si ne zapovne *)

(*Če memoiziramo bo boljše :) *)

let max_cheese cheese_matrix =
  let max_r = Array.length cheese_matrix in   (*koliko je vrstic*)
  let max_c = Array.length cheese_matrix.(0) in   (* koliko je stolpcev, pogledal smo samo eno vrstico, to bi failal, če bi imel prazen seznam, to bomo zdej ignoriral*)
  let max_cheese' recursive_max_cheese' (r, c) =
    if r >= max_r || c >= max_c then
      0     (* treba je prevert, če je res kul da je 0 (tokrat je ker gledamo maximalno vsoto in 0 bi zmeri manjša, oz zadošča tudi za matriko 1x1*)
    else
      let right = recursive_max_cheese' (r, c + 1) in
      let down = recursive_max_cheese' (r + 1, c) in
      let our_cheese = cheese_matrix.(r).(c) in
      our_cheese + max right down
    in
   let memo_max_cheese = memoiziraj_rec max_cheese' in
   memo_max_cheese(0, 0)

(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

(*
let rec rdeci = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> modri(n - 1) + (n - 2)


let alternating_towers n = 
*)


(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.

 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]
