

(* Signature: int list -> int *)
let conArrLen l =
(* Recursive auxiliary function with signature int list -> int -> int -> int
   Hint: matching with 3 cases on first argument *)
  in match l with
    | [] -> error()
    | head::next::tail where head == next or head != next - 1 ->  (next, explode(next::tail))
    | head::next::tail where head == next - 1 ->  if l[1] <= (head, explode(head::tail))[1]
                                                  then (head, explode(head::tail))
                                                  else  l



(* Test *)
if conArrLen [1; 2] <> 2 then
  print_endline "Faux";
if conArrLen [1; 0] <> 1 then
  print_endline "Faux";
if conArrLen [11; 12; 13; 14; 0] <> 4 then
  print_endline "Faux";;

(* Signature: int list -> int*int *)
let maxConArrLen l =
(* Recursive auxiliary function
   POSSIBLE signature (may differ) : int list -> int -> int -> int -> int*int *)
  in match l with
    | [] -> error()
    | head::tail -> if conArrLen l > maxLen then recMaxConArrlen tail (index+1) (conArrLen l) index
    else recMaxConArrLen tail (index+1) maxLen indexOfMax;


(* Test *)
if maxConArrLen [1; 2] <> (0,2) then
  print_endline "Faux";
if (maxConArrLen [1; 0] <> (0,1) && maxConArrLen [1; 0] <> (1,1)) then
  print_endline "Faux";
if maxConArrLen [11; 12; 13; 14; 0] <> (0,4) then
  print_endline "Faux";
if maxConArrLen [1; 11; 12; 13; 14; 0] <> (1,4) then
  print_endline "Faux";;