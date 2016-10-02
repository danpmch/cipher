
open Batteries

module type Alphabet = sig
   type letter
   val letters: letter list

   val of_string : string -> letter option
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
   let rot13 = rotNString 13

   let rotAll text : A.letter list list =
      let totalLetters = List.length A.letters in
      let shifts = List.of_enum (1 --^ totalLetters) in
      let shiftFuncs = List.map rotNString shifts in
      List.map ((|>) text) shiftFuncs

end

module Translate (A : Alphabet) = struct
   let of_strings strs =
      let opts = List.map A.of_string strs in
      List.fold_right (fun l acc -> match l with
      | Some l' -> l' :: acc
      | None -> acc) opts []

   let to_strings : A.letter list -> string list = List.map A.to_string
   let to_string : A.letter list -> string = String.concat "" % to_strings
end

module LowercaseEnglish : Alphabet = struct
   type letter = char

   let letters =
      let (--) = Char.(--) in
      List.of_enum (Enum.append ('a'--'z') ('0' -- '9'))

   let of_string x = match String.to_list x with
   | c :: [] -> if List.mem c letters then Some c else None
   | _ -> None

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
   let to_string x = x
end

module EnglishRotation = Rotate( LowercaseEnglish )
module EnglishTranslation = Translate( LowercaseEnglish )

module MorseRotation = Rotate( MorseCode )
module MorseTranslation = Translate( MorseCode )

let toStringList = List.map Char.escaped % String.to_list

let toOtherAlphabet (alpha1Letters: 'a list) (alpha2Letters: 'b list) text: 'b list =
   let alist = List.combine alpha1Letters alpha2Letters in
   List.map (fun e -> List.assoc e alist) text

let toMorse = toOtherAlphabet LowercaseEnglish.letters MorseCode.letters
let toEnglish = toOtherAlphabet MorseCode.letters LowercaseEnglish.letters

let () =
   let text = Sys.argv.(1) in
   let englishText = (EnglishTranslation.of_strings % toStringList) text in
   let morseText = toMorse englishText in
   let decrypts = MorseRotation.rotAll morseText in
   let untranslatedDecrypts = List.map (EnglishTranslation.to_string % toEnglish) decrypts in
   (
   print_string "Applying all decryptions to: ";
   print_string text;
   print_newline ();
   print_string (String.join "\n" untranslatedDecrypts);
   print_newline ();
   )

