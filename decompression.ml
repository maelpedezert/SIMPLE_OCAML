(*
A block of digits representing a number followed by a block of chars
representing the substring to be repeated.

The main idea is the following:
Define the helper functions
1) readInt l
signature: char list -> int*char list
Function: Takes a list that starts with a digit
"Consumes" the list until a non-digit char is
returns the tuple int computed * rest of the list (which in our case cannot be empty)
If you struggle with this, make a first version which only works for single digit integers
to pass the first tests
EX
['9'; 'a'; 'b'; 'c'; '4'; 'a'] -> '9', ['a'; 'b'; 'c'; '4'; 'a']

2) isNumeric c
signature: char -> bool
Function: True if c is a numeric char

3) getSubString l
signature: char list -> char list*char list
Function: Takes a list of chars that starts with a non-digit.
Read the list and construct the substring until the next digit is found OR until the list is EMPTY
Return the substring * rest of list. rest of list is either empty or starts with digit char
EX
['a'; 'b'; 'c'; '4'; 'a'] -> ['a'; 'b'; 'c']*['4'; 'a']
Note: This is where you can reuse the concept of how concatenate works

Use these helper functions in a local recursive function
to determine the number of times a substring has to be repeated and what that substring is
before performing a recursive call.
Be reminded that you can construct "local variables" like
let myint,mysubstringlist = readInt current_list in
    (*some other expression*)
*)

#use "repeater.ml"

let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

let repeater n l1 lorig =
    let rec repeaterImpl_ c list_c = match c with
        | 0 -> list_c
        | c -> repeaterImpl_ (c-1) (concatenate l1 list_c);
    in match n with
		|_ when n < 0 -> lorig
		|_ -> repeaterImpl_ n lorig;;


let readInt l =
    let rec readIntImpl_ current_list n = match current_list with
        | '0'::tail -> readIntImpl_ tail (n*10)
        | '1'::tail -> readIntImpl_ tail (n*10 + 1)
        | '2'::tail -> readIntImpl_ tail (n*10 + 2)
        | '3'::tail -> readIntImpl_ tail (n*10 + 3)
        | '4'::tail -> readIntImpl_ tail (n*10 + 4)
        | '5'::tail -> readIntImpl_ tail (n*10 + 5)
        | '6'::tail -> readIntImpl_ tail (n*10 + 6)
        | '7'::tail -> readIntImpl_ tail (n*10 + 7)
        | '8'::tail -> readIntImpl_ tail (n*10 + 8)
        | '9'::tail -> readIntImpl_ tail (n*10 + 9)
        | head::tail -> (n,current_list)
        | [] -> (n,[]) ;(*error*)
    in match l with
        | [] -> (-1,[])
        | head::tail -> readIntImpl_ l 0;;


let isNumeric c = match c with
    (* I will leave that to you *)

let getSubString l =
    let rec getSubStringImpl_ l = match l with
        | [] -> [],[]
        | h::t when isNumeric(String.make 1 h) = true -> [], h::t
        | h::t when isNumeric(String.make 1 h) = false -> h::fst(getSubStringImpl_ t), snd(getSubStringImpl_ t);
    in match l with
        | [] -> [],[]
        | h::t when isNumeric(String.make 1 h) = false -> getSubStringImpl_ l
		| _ -> raise (Invalid_argument "Invalid input");;


let decompress l =
    let rec decompressImpl_ current_list = match current_list with
        | [] -> []
        | h::t when isNumeric(String.make 1 h) = true ->
		repeater
		(fst(readInt(current_list)))
		(fst(getSubString(snd(readInt(current_list)))))
		(decompressImpl_ ((snd(getSubString(snd(readInt(current_list)))))))
        | h::t when isNumeric(String.make 1 h) = false ->
		repeater
		1
		(fst(getSubString(snd(readInt(t)))))
		(decompressImpl_ (snd(getSubString(snd(readInt(t))))));
    in match l with
        | [] -> []
         | h::t when isNumeric(String.make 1 h) -> decompressImpl_ l
		| _ -> raise (Invalid_argument "Invalid input, list must start with a digit");;

(*test*)
if (decompress (explode "1abc")) <> (explode "abc") then
    print_endline "Failed 1abc";;
if (decompress (explode "3abc")) <> (explode "abcabcabc") then
    print_endline "Failed 3abc";;
if (decompress (explode "3abc")) <> (repeater 3 (explode "abc") []) then
    print_endline "Failed 3abc";;
if (decompress (explode "3abc4edf")) <> (repeater 3 (explode "abc") (repeater 4 (explode "edf") [])) then
    print_endline "Failed 3abc4edf";;
if (decompress (explode "31abc94edf")) <> (repeater 31 (explode "abc") (repeater 94 (explode "edf") [])) then
    print_endline "Failed 31abc94edf";;

