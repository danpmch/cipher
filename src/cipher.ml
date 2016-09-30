
open Batteries

let rot13char c =
   let unascii i = i - (int_of_char 'a') in
   let ascii i = i + (int_of_char 'a') in
   let rot i = (i + 13) mod 26 in
   Char.lowercase c
 |> int_of_char
 |> unascii
 |> rot
 |> ascii
 |> char_of_int

let rot13 text = String.map rot13char text

let () =
   let cipher = rot13 "test" in
   let uncipher = rot13 cipher in
   (
   print_string cipher;
   print_newline ();
   print_string uncipher;
   print_newline ();
   )

