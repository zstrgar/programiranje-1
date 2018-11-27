(* -------- 1 -------- *)
let vsota list =
  let rec vsota_aux acc list = 
    match list with
      | [] -> acc
      | glava :: rep -> vsota_aux (acc + glava) rep 
  in
  vsota_aux 0 list

(* -------- 2 -------- *)

let rec preveri list = 
  match list with
  | [] -> true
  | [x] -> true
  | x :: y :: xs ->
      if x > y then
        false
      else 
        preveri (y :: xs)


(* -------- 3 -------- *)

let rec vstavi n list =
  match list with
  | [] -> [n]
  | x :: xs ->
    if x > n then
      n :: list
    else
      x :: (vstavi n xs)

let rec uredi list = 
  match list with
  | [] -> []
  | [x] -> [x]
  | x :: xs -> vstavi x (uredi xs)

 
(* -------- 4 -------- *)


(* -------- 5 -------- *)


type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

type priority =
  | Top
  | Group

type status = 
  | Staff
  | Passenger             

(* -------- 6 -------- *)

(* -------- 7 -------- *)