
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

let registre = ref (Array.init 4 (fun _ -> [||]))
let depot = ref (Array.init 4 (fun _ -> 0))
let colonnes = ref (Array.init 8 (fun _ -> [|Card.of_num 0|]))

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

let rec construct_FreeCell count_permut pos_col permut acc = 
  match pos_col with
  | 8 -> acc
  | _ -> if pos_col mod 2 = 0 then acc.(pos_col) <- Array.init 7 (fun i -> List.nth permut (count_permut+i))
         else acc.(pos_col) <- Array.init 6 (fun i -> List.nth permut (count_permut+i)); 
         if pos_col mod 2 = 0 then construct_FreeCell (count_permut+7) (pos_col+1) permut acc
         else construct_FreeCell (count_permut+6) (pos_col+1) permut acc

let rec construct_Seahaven count_permut pos_col permut acc =
  match pos_col with
  | 10 -> acc, (List.nth permut 50, List.nth permut 51)
  | _ -> acc.(pos_col) <- Array.init 5 (fun i -> List.nth permut (count_permut+i));
         construct_Seahaven (count_permut+5) (pos_col+1) permut acc

let rec construct_MidnightOil count_permut pos_col permut acc =
  match pos_col with
  | 18 -> acc
  | 17 -> acc.(pos_col) <- Array.init 1 (fun i -> List.nth permut (count_permut+i));
          construct_MidnightOil (count_permut+1) (pos_col+1) permut acc
  | _ -> acc.(pos_col) <- Array.init 3 (fun i -> List.nth permut (count_permut+i));
         construct_MidnightOil (count_permut+3) (pos_col+1) permut acc

let rec construct_BakersDozen count_permut pos_col permut acc =
  match pos_col with
  | 13 -> acc
  | _ -> acc.(pos_col) <- Array.init 4 (fun i -> List.nth permut (count_permut+i));
         construct_BakersDozen (count_permut+4) (pos_col+1) permut acc

(* METHODES FREECELL *)

let rec length_array array = 
  match array with
  | [| |] -> 0
  | _ -> 1 + length_array (Array.sub array 1 (Array.length array - 1))

let color card =
  match card with
  | (_, c) -> match c with
              | Card.Trefle | Card.Pique -> "black"
              | Card.Carreau | Card.Coeur -> "red"
let num card =
  match card with
  | (n, _) -> n

let suit card =
  match card with 
  | (_, s) -> match s with
              | Card.Trefle -> "Trefle"
              | Card.Carreau -> "Carreau"
              | Card.Coeur -> "Coeur"
              | Card.Pique -> "Pique"

let is_same_color (card1 : Card.card) (card2 : Card.card) =
  let color1 = color card1 in
  let color2 = color card2 in
  color1 = color2

let is_same_suit (card1 : Card.card) (card2 : Card.card) =
  let suit1 = suit card1 in
  let suit2 = suit card2 in
  suit1 = suit2

let find_col array card =
  let colonnes_length = length_array !array in
  let rec loop1 n = 
    match n with
    | n -> 
        if n = colonnes_length then colonnes_length
        else
          let col_length = length_array !array.(n) in
          let pos = ref (col_length - 1) in
          if !colonnes.(n).(!pos) = card then n
          else loop1 (n+1)
    in
    loop1 0 

let move_card col_card col_dest =
  (* On deplace la carte card sur la carte dest *)
  (* On retire la carte card de sa colonne *)
  let col_card_length = length_array !colonnes.(col_card) in
  let pos_card = ref (col_card_length - 1) in
  let card = !colonnes.(col_card).(!pos_card) in
  let new_col_card = Array.sub !colonnes.(col_card) 0 (!pos_card) in
  Array.set !colonnes col_card new_col_card;
  (* On ajoute la carte card à la colonne de dest *)
  let col_dest_length = length_array !colonnes.(col_dest) in
  let pos_dest = ref (col_dest_length - 1) in
  let new_col_dest = Array.make (col_dest_length + 1) card in
  Array.blit !colonnes.(col_dest) 0 new_col_dest 0 (col_dest_length);
  Array.set !colonnes col_dest new_col_dest;
  "SUCCES";;

let move_to_col_freecell (card_num : int) (dest_num : int) n = 
  (* Si les cartes card et dest ne sont pas de couleurs différentes ou/et que le numero
     de card n'est pas egal au numero de dest-1, c'est un ECHEC*)
  let card_of_card_num = Card.of_num card_num in
  let card_of_dest_num = Card.of_num dest_num in

  if is_same_color card_of_card_num card_of_dest_num || num card_of_card_num <> num card_of_dest_num - 1 then "ECHEC "^string_of_int n
  else
    (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
    let col_card = find_col colonnes card_of_card_num in

    (* On cherche la colonne où se trouve la carte dest. Elle doit être la dernière carte de sa colonne *)
    let col_dest = find_col colonnes card_of_dest_num in

    (* Si col_dest = colonnes_length -> ECHEC *)
    let colonnes_length = length_array !colonnes in
    if col_dest = colonnes_length || col_card = colonnes_length then "ECHEC "^string_of_int n
    else
      move_card col_card col_dest;;

let move_to_col_seahaven (card_num : int) (dest_num : int) n = 
  let card_of_card_num = Card.of_num card_num in
  let card_of_dest_num = Card.of_num dest_num in

  if is_same_suit card_of_card_num card_of_dest_num = false || num card_of_card_num <> num card_of_dest_num - 1 then "ECHEC "^string_of_int n
  else
    (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
    let col_card = find_col colonnes card_of_card_num in
   
    (* On cherche la colonne où se trouve la carte dest. Elle doit être la dernière carte de sa colonne *)
    let col_dest = find_col colonnes card_of_dest_num in

    let colonnes_length = length_array !colonnes in
    if col_dest = colonnes_length || col_card = colonnes_length then "ECHEC "^string_of_int n
    else 
      move_card col_card col_dest;;

let move_to_col_midnight (card_num : int) (dest_num : int) n = 
  let card_of_card_num = Card.of_num card_num in
  let card_of_dest_num = Card.of_num dest_num in

  if is_same_suit card_of_card_num card_of_dest_num = false || num card_of_card_num <> num card_of_dest_num - 1 then "ECHEC "^string_of_int n
  else 
    (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
    let col_card = find_col colonnes card_of_card_num in
   
    (* On cherche la colonne où se trouve la carte dest. Elle doit être la dernière carte de sa colonne *)
    let col_dest = find_col colonnes card_of_dest_num in

    let colonnes_length = length_array !colonnes in
    if col_dest = colonnes_length || col_card = colonnes_length then "ECHEC "^string_of_int n
    else 
      move_card col_card col_dest;;

let move_to_col_baker (card_num : int) (dest_num : int) n = 
  let card_of_card_num = Card.of_num card_num in
  let card_of_dest_num = Card.of_num dest_num in

  if num card_of_card_num <> num card_of_dest_num - 1 then "ECHEC "^string_of_int n
  else 
    (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
    let col_card = find_col colonnes card_of_card_num in
   
    (* On cherche la colonne où se trouve la carte dest. Elle doit être la dernière carte de sa colonne *)
    let col_dest = find_col colonnes card_of_dest_num in

    let colonnes_length = length_array !colonnes in
    if col_dest = colonnes_length || col_card = colonnes_length then "ECHEC "^string_of_int n
    else 
      move_card col_card col_dest;;

let find_empty_col colonnes =
  let colonnes_length = length_array !colonnes in
  let rec loop n = 
    match n with
    | n -> 
        if n = colonnes_length then colonnes_length
        else
          let col_length = length_array !colonnes.(n) in
          if col_length = 0 then n
          else loop (n+1)
  in
  loop 0;;

let move_to_empty_col_freecell (card_num : int) n = 
  let card_of_card_num = Card.of_num card_num in
  (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
  let col_card = find_col colonnes card_of_card_num in
  (* On trouve la premiere colonne vide *)
  let col_dest = find_empty_col colonnes in

  let colonnes_length = length_array !colonnes in
  (* Si col_dest = colonnes_length || col_card = colonnes_length -> ECHEC *)
  if col_dest = colonnes_length || col_card = colonnes_length then "ECHEC "^string_of_int n
  else 
    (* On retire la carte card de sa colonne *)
    let col_card_length = length_array !colonnes.(col_card) in
    let pos_card = ref (col_card_length - 1) in
    let card = !colonnes.(col_card).(!pos_card) in
    let new_col_card = Array.sub !colonnes.(col_card) 0 (!pos_card) in
    Array.set !colonnes col_card new_col_card;
    (* Et la place dans la colonne vide *)
    let new_col_dest = Array.make 1 card in
    Array.set !colonnes col_dest new_col_dest;
    "SUCCES";;

let move_to_empty_col_seahaven (card_num : int) n =
  let card_of_card_num = Card.of_num card_num in
  (* On teste si la carte est un roi *)
  let card_num = num card_of_card_num in
  if card_num <> 13 then "ECHEC "^string_of_int n
  else
    (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
    let col_card = find_col colonnes card_of_card_num in
    (* On trouve la premiere colonne vide *)
    let col_dest = find_empty_col colonnes in

    let colonnes_length = length_array !colonnes in
    (* Si col_dest = colonnes_length || col_card = colonnes_length -> ECHEC *)
    if col_dest = colonnes_length || col_card = colonnes_length then "ECHEC "^string_of_int n
    else 
      (* On retire la carte card de sa colonne *)
      let col_card_length = length_array !colonnes.(col_card) in
      let pos_card = ref (col_card_length - 1) in
      let card = !colonnes.(col_card).(!pos_card) in
      let new_col_card = Array.sub !colonnes.(col_card) 0 (!pos_card) in
      Array.set !colonnes col_card new_col_card;
      (* Et la place dans la colonne vide *)
      let new_col_dest = Array.make 1 card in
      Array.set !colonnes col_dest new_col_dest;
      "SUCCES";;

let find_empty_registre registre =
  let registres_length = 4 in
  let rec loop n = 
    match n with
    | n -> 
        if n = registres_length then registres_length
        else
          let reg_length = length_array !registre.(n) in
          if reg_length = 0 then n
          else loop (n+1)
  in
  loop 0;;

let move_to_registre_fc_st (card_num : int) n = 
  let card_of_card_num = Card.of_num card_num in
  (* On cherche la colonne où se trouve la carte card. Elle doit être la dernière carte de sa colonne *)
  let col_card = find_col colonnes card_of_card_num in
  
  (* On trouve un registre vide *)
  let reg_dest = find_empty_registre registre in

  let colonnes_length = length_array !colonnes in
  let registres_length = 4 in
  (* Si reg_dest = registres_length || col_card = registres_length -> ECHEC *)
  if reg_dest = registres_length || col_card = colonnes_length then "ECHEC "^string_of_int n
  else
    (* On retire la carte de la colonne *)
    let col_card_length = length_array !colonnes.(col_card) in
    let pos_card = ref (col_card_length - 1) in
    let card = !colonnes.(col_card).(!pos_card) in
    let new_col_card = Array.sub !colonnes.(col_card) 0 (!pos_card) in
    Array.set !colonnes col_card new_col_card;
    (* Il y a une carte par registre, donc on a pas besoin de parcourir la position du registre *)
    let new_reg_dest = Array.make 1 card in
    Array.set !registre reg_dest new_reg_dest;
    "SUCCES";;

let move_to_col (card_num : int) (dest_num : int) game (n : int) = 
  match game with
  | Freecell -> move_to_col_freecell card_num dest_num n
  | Seahaven -> move_to_col_seahaven card_num dest_num n
  | Midnight -> move_to_col_midnight card_num dest_num n
  | Baker -> move_to_col_baker card_num dest_num n
  

let move_to_empty_col (card_num : int) game (n : int) = 
  match game with
  | Freecell -> move_to_empty_col_freecell card_num n
  | Seahaven -> move_to_empty_col_seahaven card_num n
  | Midnight -> "Impossible de déplacer une carte dans une colonne vide dans Midnight:\n ECHEC "^string_of_int n
  | Baker -> "Impossible de déplacer une carte dans une colonne vide dans Baker:\n ECHEC "^string_of_int n


let move_to_registre (card_num : int) game (n : int) = 
  match game with
  | Freecell -> move_to_registre_fc_st card_num n
  | Seahaven -> move_to_registre_fc_st card_num n
  | Midnight -> "Impossible de déplacer une carte dans un registre dans Midnight:\n ECHEC "^string_of_int n
  | Baker -> "Impossible de déplacer une carte dans un registre dans Baker:\n ECHEC "^string_of_int n
  
let move (card_num : int) (dest_num : int) game (n : int) = 
  match dest_num with
  | _ -> if dest_num = -1 then move_to_empty_col card_num game n
         else if dest_num = -2 then move_to_registre card_num game n
         else move_to_col card_num dest_num game n;;

(* teste les coup donnée dans le fichier *)
(* card_num : correspond a la carte a deplacer.
      c'est un nombre entier de 0 à 51 *)
(* dest : correspond a la destination du deplacement. *)
(*    C'est soit :
          - Un entier de 0 à 51 
          - La lettre majuscule V, qui indique un mouvement vers la premiere colonne vide 
          - La lettre majuscule T, indique un mouvement vers un registre *)
let check_move_sol card dest game n = 
  let card_num = int_of_string card in
  let dest_num = match dest with
    | "V" -> -1
    | "T" -> -2
    | _ -> int_of_string dest in
  let res = move card_num dest_num game n in
  if res = "SUCCES" then "SUCCES"
  else "ECHEC "^string_of_int n


(* lit un fichier .sol *)
let read_sol_file filename = 
  let ic = open_in filename in
  let bool = ref false in
  let rec loop () n = 
    try
      bool := true;
      (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;
      print_string "----------\n";
      Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !registre;*)
      let line = input_line ic in
      (* Le formatage dans les fichiers sur chaque ligne est %s %s*)
      let (card, dest) = Scanf.sscanf line "%s %s" (fun card dest -> (card, dest)) in
      let res = check_move_sol card dest config.game n in
      if res = "ECHEC "^string_of_int n then 
        "ECHEC "^string_of_int n
      else loop () (n+1)
    with End_of_file -> if !bool then "ECHEC "^string_of_int n else "SUCCES"
  in
  let res = loop () 1 in
  close_in ic;
  let res = res in
  match res with
  | "SUCCES" -> print_string "SUCCES" ; exit 0
  | _ -> print_string res ; exit 1;;


let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  match conf.game with
  | Freecell -> (*print_string "\nConstruction de la partie FreeCell:\n";*)
                let freeCell = construct_FreeCell 0 0 permut (Array.init 8 (fun _ -> [|0|])) in
                colonnes := Array.map (fun x -> Array.map (fun y -> Card.of_num y) x) freeCell;
                (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;*)
  | Seahaven -> (*print_string "\nConstruction de la partie Seahaven:\n";*)
                let seahaven = construct_Seahaven 0 0 permut (Array.init 10 (fun _ -> [|0|])) in
                colonnes := Array.map (fun x -> Array.map (fun y -> Card.of_num y) x) (fst seahaven);
                Array.set !registre 0 [|Card.of_num (fst (snd seahaven))|];
                Array.set !registre 1 [|Card.of_num (snd (snd seahaven))|];
                Array.set !registre 2 [||];
                Array.set !registre 3 [||];
                (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;*)

  | Midnight -> (*print_string "\nConstruction de la partie Midnight:\n";*)
                let midnight = construct_MidnightOil 0 0 permut (Array.init 18 (fun _ -> [|0|])) in
                colonnes := Array.map (fun x -> Array.map (fun y -> Card.of_num y) x) midnight;
                (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;*)

  | Baker ->    (*print_string "\nConstruction de la partie Baker:\n";*)
                let baker = construct_BakersDozen 0 0 permut (Array.init 13 (fun _ -> [|0|])) in
                colonnes := Array.map (fun x -> Array.map (fun y -> Card.of_num y) x) baker;;
                (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;*)
  
let filename mode = 
  match mode with
  | Check filename -> filename
  | Search filename -> filename

let main () =
  let speclist = [
    ("-check", Arg.String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
    ("-search", Arg.String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")
  ] in
  let usage_msg = "XpatSolver <game>.<number> : search solution for Xpat2 game <number>" in
  Arg.parse speclist set_game_seed usage_msg;

  set_game_seed Sys.argv.(1);
  treat_game config;

  (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;
  print_string "----------\n";
  Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !registre;;*)

  print_string (read_sol_file (filename config.mode));;

let _ = if not !Sys.interactive then main () else ()
