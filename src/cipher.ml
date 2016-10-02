
open Batteries

module type Alphabet = sig
   type letter
   val letters: letter list

   val of_string : string -> letter option

   val separator: string
   val to_string : letter -> string
end

module Rotate (A : Alphabet) = struct

   let rotNLetter (n : int) (l : A.letter) : A.letter =
      let to_int l = match List.index_of l A.letters with
      | Some i -> i
      | _ -> failwith ("Error, invalid letter" ^ (A.to_string l)) in let of_int i = List.at A.letters i in
      let rot i = (i + n) mod (List.length A.letters) in
      to_int l
    |> rot
    |> of_int

   let rotNString (n : int) : A.letter list -> A.letter list = List.map (rotNLetter n)

   let rotAll text : A.letter list list =
      let totalLetters = List.length A.letters in
      let shifts = List.of_enum (1 --^ totalLetters) in
      let shiftFuncs = List.map rotNString shifts in
      List.map ((|>) text) shiftFuncs

end

let nsplit s sep =
   let toStringList = List.map Char.escaped % String.to_list in
   try String.nsplit s sep
   with Invalid_argument _ -> toStringList s

module Translate (A : Alphabet) = struct
   let of_string str =
      let opts = List.map A.of_string (nsplit str A.separator) in
      List.fold_right (fun l acc -> match l with
      | Some l' -> l' :: acc
      | None -> acc) opts []

   let to_strings : A.letter list -> string list = List.map A.to_string
   let to_string : A.letter list -> string = String.concat A.separator % to_strings
end

module LowercaseEnglish : Alphabet = struct
   type letter = char

   let letters =
      let (--) = Char.(--) in
      List.of_enum (Enum.append ('a'--'z') ('0' -- '9'))

   let of_string x = match String.to_list x with
   | c :: [] -> if List.mem c letters then Some c else None
   | _ -> None

   let separator = ""
   let to_string x = Char.escaped x

end

module MorseCode : Alphabet = struct
   type letter = string
   let letters = [
      ".-"   (*A*);
      "-..." (*B*);
      "-.-." (*C*);
      "-.."  (*D*);
      "."    (*E*);
      "..-." (*F*);
      "--."  (*G*);
      "...." (*H*);
      ".."   (*I*);
      ".---" (*J*);
      "-.-"  (*K*);
      ".-.." (*L*);
      "--"   (*M*);
      "-."   (*N*);
      "---"  (*O*);
      ".--." (*P*);
      "--.-" (*Q*);
      ".-."  (*R*);
      "..."  (*S*);
      "-"    (*T*);
      "..-"  (*U*);
      "...-" (*V*);
      ".--"  (*W*);
      "-..-" (*X*);
      "-.--" (*Y*);
      "--.." (*Z*);

      ".----" (*1*);
      "..---" (*2*);
      "...--" (*3*);
      "....-" (*4*);
      "....." (*5*);
      "-...." (*6*);
      "--..." (*7*);
      "---.." (*8*);
      "----." (*9*);
      "-----" (*0*);
   ]

   let of_string x = if List.mem x letters then Some x else None

   let separator = " "
   let to_string x = x
end

module EnglishRotation = Rotate( LowercaseEnglish )
module EnglishTranslation = Translate( LowercaseEnglish )

module MorseRotation = Rotate( MorseCode )
module MorseTranslation = Translate( MorseCode )

let toOtherAlphabet (alpha1Letters: 'a list) (alpha2Letters: 'b list) text: 'b list =
   let alist = List.combine alpha1Letters alpha2Letters in
   List.map (fun e -> List.assoc e alist) text

let toMorse = toOtherAlphabet LowercaseEnglish.letters MorseCode.letters
let toEnglish = toOtherAlphabet MorseCode.letters LowercaseEnglish.letters

let () =
   let anonArgs: string list ref = ref [] in
   let anonFunc = (fun anon -> anonArgs := List.append !anonArgs [anon]) in
   let action: (string list -> unit) ref = ref (fun _ -> ()) in
   (
   Arg.parse [
      ("--to-morse",
       (Arg.Unit (fun () -> (
          action := (fun args ->
             let english = EnglishTranslation.of_string (List.at args 0) in
             let morse = toMorse english in
             let result = MorseTranslation.to_string morse in
             (print_string result; print_newline ();)
          )
       ))),
       "Translate english input to morse");
      ("--to-english",
       (Arg.Unit (fun () -> (
          action := (fun args ->
             let morse = MorseTranslation.of_string (List.at args 0) in
             let english = toEnglish morse in
             let result = EnglishTranslation.to_string english in
             (print_string result; print_newline ();)
          )
       ))),
       "Translate morse input to english. Note that if the morse begins with '-', you'll need to include a leading space because the Ocaml arg parser is shockingly dumb.");
      ("--rotate",
       (Arg.Int (fun n -> (
          action := (fun args ->
             let english = EnglishTranslation.of_string (List.at args 0) in
             let rotated = EnglishRotation.rotNString n english in
             let result = EnglishTranslation.to_string rotated in
             (print_string result; print_newline ();)
          )
       ))),
       "Apply a rotation of n to the input english characters");
      ("--rotate-all",
       (Arg.Unit (fun () -> (
          action := (fun args ->
             let english = EnglishTranslation.of_string (List.at args 0) in
             let rotated = EnglishRotation.rotAll english in
             let englishResults = List.map EnglishTranslation.to_string rotated in
             let result = String.join "\n" englishResults in
             (print_string result; print_newline ();)
          )
       ))),
       "Apply all possible rotations to the input english characters");
   ]
   anonFunc
   "usage: cipher [option] text";
   !action !anonArgs
   )

