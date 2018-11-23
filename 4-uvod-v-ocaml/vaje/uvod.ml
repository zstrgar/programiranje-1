
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [penultimate_element] vrne predzadnji element danega seznama. V
 primeru prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec penultimate_element xs = 
  match xs with
  | [] -> failwith "List too short"
  | x :: _last :: [] -> x
  | x :: xs -> penultimate_element xs

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k xs = 
  match xs with 
  | [] -> failwith "List too short"
  | x :: xs -> 
    if k <= 0 then
      x
    else
      get (k-1) xs
  

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double xs =
  match xs with
  | [] -> []
  | x :: xs -> (*x :: x :: (double xs)*)
    let xs_doubled = double xs in
    x :: x :: xs_doubled 

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k xs = 
  match (k, xs) with
  | (_, []) -> ([], [])
  | (k, xs) when k <=0 -> ([], xs)
  | (k, x :: xs) ->
      let (list1, list2) = divide (k - 1) xs in
          (x :: list1, list2)

      

(*----------------------------------------------------------------------------*]
 Funkcija [delete k list] iz seznama izbriše [k]-ti element. V primeru
 prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k list = 
  match list with
  | [] -> failwith "List too short"
  | x :: xs -> 
      if k <= 0 then
        xs
      else
        x :: delete (k - 1) xs
      

(*----------------------------------------------------------------------------*]
 Funkcija [slice i k list] sestavi nov seznam, ki vsebuje elemente seznama
 [list] od vključno [i]-tega do izključno [k]-tega. Predpostavimo, da sta [i] in
 [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i k list = 
  let (_, slice1) = divide i list in
  let (slice2, _) = divide (k - i) slice1 in
  slice2


(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list =
  match list with
  | [] -> [x]
  | glava :: rep ->
      if k <= 0 then
        x :: glava :: rep
      else
        glava :: insert x (k - 1) rep

      

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list =
  let (list1, list2) = divide n list in
  list2 @ list1

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list =
  match list with
  | [] -> []
  | glava :: rep ->
    if x = glava then
      remove x rep
    else
      glava :: remove x rep

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome list = 
  let rec reverse list =
    match list with
    |[] -> []
    |x :: xs -> (reverse xs) @ [x]
  in
  reverse list = list

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components list1 list2 =
  match (list1, list2) with
  | (x :: xs, y :: ys) -> max x y :: max_on_components xs ys
  | _ -> []

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest list = 
  let rec largest list =
    match list with
    | [] -> failwith "Seznam nima elementov"
    | x :: [] -> x
    | x :: xs -> max x (largest xs)
    in
  let najvecji = largest list in
  let nov_seznam = remove najvecji list in
  largest nov_seznam
