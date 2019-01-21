(* PRVA NALOGA *)
(*A*)
let razlika_kvadratov x y =
  (x + y) * (x + y) - (x * x + y * y) 

(*b*)

let uporabi_na_paru f (x, y) = (f x, f y)

(*C*)

let rec ponovi_seznam n sez =
  if n <= 0 then []
  else
    sez @ (ponovi_seznam(n - 1) sez)

(*d*)

let razdeli sez =
  let rec aux acc1 acc2 sez =
    match sez with
    | [] -> (List.rev acc1, List.rev acc2)
    | x :: xs ->
      if x < 0 then
        aux (x :: acc1) acc2 xs
      else
        aux (acc1) (x :: acc2) xs
  in
  aux [] [] sez

(*DRUGA NALOGA*)
(*imamo tip drevo -> iz githuba vzamemo definicijo za drevo*)
type 'a drevo = 
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let leaf x = Sestavljeno (Prazno, x, Prazno)

let test1 =
  Sestavljeno(
    Sestavljeno(leaf 3, 10, Sestavljeno(leaf 14, 13, leaf 6)),
    11,
    Sestavljeno(leaf 2, 8, leaf 10))

let rec padajoca v drevo =
  match drevo with
  | Prazno -> []
  | Sestavljeno(levo, x, desno) ->
    if x > v then []
    else
      let leva = padajoca x levo in
      let desna = padajoca x desno in
      if List.length leva > List.length desna then 
        leva @ [x]
      else
        desna @ [x]

let rec narascajoca v drevo =
  match drevo with
    | Prazno -> []
    | Sestavljeno(levo, x, desno) ->
      if x < v then []
      else
        let leva = narascajoca x levo in
        let desna = narascajoca x desno in
        if List.length leva > List.length desna then 
          x :: leva
        else
          x :: desna


let rec monotona_pot drevo =
  match drevo with
  | Prazno -> []
  | Sestavljeno(levo, x, desno) ->
    let pure_left = monotona_pot levo in
    let pure_right = monotona_pot desno in
    let left_to_right = (padajoca x levo) @ [x] @ (narascajoca x desno) in
    let right_to_left = (padajoca x desno) @ [x] @ (narascajoca x levo) in
    let options = [pure_left; pure_right; left_to_right; right_to_left] in
    let pick_bigger x y = if List.length x > List.length y then x else y in
    List.fold_left pick_bigger pure_left options
  
(*TRETJA NALOGA*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

(*a*)

let test =
  Filter ((fun x -> x < 0), [],
  Filter ((fun x -> x < 10), [],
  Ostalo  []))

(*b*)

let rec vstavi x veriga =
  match veriga with
  | Ostalo xs -> Ostalo (x :: xs)
  | Filter(f, xs, filtri) ->
    if f x then Filter(f, x :: xs, filtri)
    else
      Filter(f, xs, vstavi x filtri)

(*c*)

let rec poisci x = function
  | Ostalo elementi -> List.mem x elementi
  | Filter(f, xs, filtri) -> 
(*pogledat morš samo en seznam, torej samo tistega, ki zadošča filtru!! seprav
bomo gledali če x zadošča filtru.. če zadošča bomo šli pa notr gledat*)
    if f x then List.mem x xs 
    else
      poisci x filtri

(*d*)

let rec izprazni_filtre = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter(f, elementi, filtri) ->
    let prazni_filtri, pobrani_elementi = izprazni_filtre filtri in
    let vsi_elementi = elementi @ pobrani_elementi in
    (Filter(f, [], prazni_filtri), vsi_elementi)

(*e*)

let rec dodaj_filter f veriga = 
  let veriga' = Filter(f, [], veriga) in
  let prazna_veriga, elementi = izprazni_filtre veriga' in
  List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi