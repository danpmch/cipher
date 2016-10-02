
open Batteries

let rotNChar (n : int) (c : char) : char =
   let unascii i = i - (int_of_char 'a') in
   let ascii i = i + (int_of_char 'a') in
   let rot i = (i + n) mod 26 in
   Char.lowercase c
 |> int_of_char
 |> unascii
 |> rot
 |> ascii
 |> char_of_int

let rotNString (n : int) : string -> string = String.map (rotNChar n)

let rot13char = rotNChar 13
let rot13 text = String.map rot13char text

let rotAll text =
   let shifts = List.of_enum (1 --^ 26) in
   let shiftFuncs = List.map rotNString shifts in
   List.map ((|>) text) shiftFuncs

let () =
   let text = Sys.argv.(1) in
   let decrypts = rotAll text in
   (
   print_string "Applying all decryptions to: ";
   print_string text;
   print_newline ();
   print_string (String.join "\n" decrypts);
   print_newline ();
   )

