
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

let registre = ref (Array.init 4 (fun _ -> [|Card.of_num 0|]))
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
         else acc.(pos_col) <- Array.init 6 (
          (* On veut print "ECHEC 1"*)
          fun i -> List.nth permut (count_permut+i)); 
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
                colonnes := Array.map (fun x -> Array.map (fun y -> Card.of_num y) x) baker;
                (*Array.iter (fun x -> Array.iter (fun y -> print_string (Card.to_string y^" ")) x; print_newline ()) !colonnes;*)
  exit 0;;

(* teste les coup donnée dans le fichier *)
let check_move_sol card dest = "TODO"
(* lit un fichier .sol *)
let read_sol_file filename = "TODO"

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config
  

let _ = if not !Sys.interactive then main () else ()
