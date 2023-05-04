(* 1. Tail of a List *)

let rec last list =
  match list with [] -> None | [ x ] -> Some x | _ :: xs -> last xs

(* 2. Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

(* 3. N'th Element of a List *)
let rec nth list n =
  match (list, n) with
  | [], _ -> None
  | _, x when x < 0 -> None
  | x :: _, 0 -> Some x
  | _ :: xs, n -> nth xs (n - 1)

(* 4. Length of a List *)
let rec length = function [] -> 0 | _ :: xs -> 1 + length xs

let length_tail list =
  let rec aux n = function [] -> n | _ :: xs -> aux (n + 1) xs in
  aux 0 list

(* 5. Reverse a List *)
let rec rev = function [] -> [] | [ x ] -> [ x ] | x :: xs -> rev xs @ [ x ]

let rev_tail list =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: acc) xs in
  aux [] list

(* 6. Palindrome *)
let rec is_palindrome word = word = rev word

(* 7. Run-Length Encoding *)
let encode list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | x :: (y :: _ as ys) ->
        if x = y then aux (count + 1) acc ys
        else aux 0 ((count + 1, x) :: acc) ys
  in
  List.rev (aux 0 [] list)

(* 8. Duplicate the Elements of a List *)
let rec duplicate = function [] -> [] | x :: xs -> [ x; x ] @ duplicate xs

let duplicate_tail list =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: x :: acc) xs in
  List.rev (aux [] list)

(* 9. Split a List Into Two Parts; The Length of the First Part Is Given *)
let split list n =
  let rec aux acc list =
    match (acc, list) with
    | acc, [] -> acc
    | (xs, _), ys when n = List.length xs -> (xs, ys)
    | (xs, _), y :: ys -> aux (xs @ [ y ], ys) ys
  in
  aux ([], []) list

(* 10. Remove the K'th Element From a List *)
let rec remove_at n list =
  match (n, list) with
  | _, [] -> []
  | 0, x :: xs -> xs
  | n, x :: xs -> x :: remove_at (n - 1) xs

(* 11. Insert an Element at a Given Position Into a List *)
let rec insert_at elem n list =
  match (n, list) with
  | _, [] -> []
  | 0, xs -> elem :: xs
  | n, x :: xs -> x :: insert_at elem (n - 1) xs

(* 12. Create a List Containing All Integers Within a Given Range *)
let rec range a b =
  match (a, b) with a, b when a = b -> [ a ] | a, b -> a :: range (a + 1) b

(* 13. Flatten a List Flatten a List  *)
type 'a node = One of 'a | Many of 'a node list

let rec flattern = function
  | [] -> []
  | One x :: xs -> x :: flattern xs
  | Many x :: xs -> flattern x @ flattern xs

(* 14. Eliminate Consecutive Duplicates *)
let rec compress = function
  | x :: (y :: _ as ys) -> if x = y then compress ys else x :: compress ys
  | smaller -> smaller

(* 15. Pack Consecutive Duplicates *)
let pack list =
  let rec aux p acc = function
    | [] -> []
    | [ x ] -> (x :: p) :: acc
    | x :: (y :: _ as ys) ->
        if x = y then aux (x :: p) acc ys else aux [] ((x :: p) :: acc) ys
  in
  List.rev (aux [] [] list)
