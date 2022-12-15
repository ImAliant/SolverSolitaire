(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (between 1 and 999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).

*)
open Fifo

(* The numbers manipulated below will be in [0..randmax[ *)
let randmax = 1_000_000_000

(* Converting an integer n in [0..randmax[ to an integer in [0..limit[ *)
let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))

(*

a) Créer tout d'abord les 55 premières paires suivantes:
  * premières composantes : 0 pour la premiere paire,
    puis ajouter 21 modulo 55 à chaque fois
  * secondes composantes : graine, puis 1, puis les "différences"
    successives entre les deux dernières secondes composantes.
    Par "différence" entre a et b on entend
      - Ou bien (a-b) si b<=a
      - Ou bien (a-b+randmax) si a<b

b) Trier ces 55 paires par ordre croissant selon leurs premières composantes,
   puis séparer entre les 24 premières paires et les 31 suivantes.
   Pour les 31 paires, leurs secondes composantes sont à mettre dans
   une FIFO f1_init, dans cet ordre (voir `Fifo.of_list` documenté dans
   `Fifo.mli`). De même pour les 24 paires, leurs secondes composantes sont
   à mettre dans une FIFO f2_init, dans cet ordre.

c) Un *tirage* à partir de deux FIFO (f1,f2) consiste à prendre
   leurs premières valeurs respectives n1 et n2 (cf `Fifo.pop`),
   puis calculer la "différence" de n1 et n2 (comme auparavant),
   nommons-la d. Ce d est alors le résultat du tirage, associé
   à deux nouvelles FIFO constituées des restes des anciennes FIFO
   auxquelles on a rajouté respectivement n2 et d (cf `Fifo.push`).

d) On commence alors par faire 165 tirages successifs en partant
   de (f1_init,f2_init). Ces tirages servent juste à mélanger encore
   les FIFO qui nous servent d'état de notre générateur pseudo-aléatoire,
   les entiers issus de ces 165 premiers tirages ne sont pas considérés.

e) La fonction de tirage vue précédemment produit un entier dans
   [0..randmax[. Pour en déduire un entier dans [0..limit[ (ou limit est
   un entier positif quelconque), on utilisera alors la fonction `reduce`
   fournie plus haut.
   Les tirages suivants nous servent à créer la permutation voulue des
   52 cartes. On commence avec une liste des nombres successifs entre 0 et 51.
   Un tirage dans [0..52[ nous donne alors la position du dernier nombre
   à mettre dans notre permutation. On enlève alors le nombre à cette position
   dans la liste. Puis un tirage dans [0..51[ nous donne la position
   (dans la liste restante) de l'avant-dernier nombre de notre permutation.
   On continue ainsi à tirer des positions valides dans la liste résiduelle,
   puis à retirer les nombres à ces positions tirées pour les ajouter devant
   la permutation, jusqu'à épuisement de la liste. Le dernier nombre retiré
   de la liste donne donc la tête de la permutation.

   NB: /!\ la version initiale de ce commentaire donnait par erreur
   la permutation dans l'ordre inverse).

Un exemple complet de génération d'une permutation (pour la graine 1)
est maintenant donné dans le fichier XpatRandomExemple.ml, étape par étape.

*)


(* For now, we provide a shuffle function that can handle a few examples.
   This can be kept later for testing your implementation. *)

let diff a b =
   if a >= b then a - b else a - b + randmax;;

let rec paires a b graine i acc =
   match i with
   | 55 -> List.rev acc
   | 0 -> paires a b graine (i+1) acc@[(0,graine)]
   | 1 -> paires graine 1 graine (i+1) acc@[(21,1)]
   | _ -> paires b (diff a b) graine (i+1) acc@[(i*21 mod 55, diff a b)];;
    
let init_fifo p =
   let paires_array = Array.of_list p in
   Array.sort (fun (a,_) (b,_) -> compare a b) paires_array;
   let first24 = Array.sub paires_array 0 24 in
   let last31 = Array.sub paires_array 24 31 in
   let last31_snd_components = Array.map snd last31 in
   let first24_snd_components = Array.map snd first24 in
   let f1_init = of_list (Array.to_list last31_snd_components) in
   let f2_init = of_list (Array.to_list first24_snd_components) in
   (f1_init, f2_init);;
    
let tirage f1 f2 =
   let n1, f1' = pop f1 in
   let n2, f2' = pop f2 in
    
   let d = diff n1 n2 in
   push n2 f1', push d f2';;
    
let rec fifo_ignored_165 i f1 f2 =
   match i with
   | 165 -> f1, f2
   | _ -> let f1', f2' = tirage f1 f2 in fifo_ignored_165 (i+1) f1' f2';;
    
let rec tirages_52 i f1 f2 acc =
   match i with
   | 52 -> acc
   | _ -> 
      let n1, f1' = pop f1 in
      let n2, f2' = pop f2 in
    
      let d = diff n1 n2 in
      let f1' = push n2 f1' in
      let f2' = push d f2' in
      tirages_52 (i+1) f1' f2' (push d acc);;
    
let rec tirage_reduce t_52 acc count =
   match count with
   | 52 -> acc
   | _ -> 
      let n, t_52' = pop t_52 in
      tirage_reduce t_52' (push (reduce n (52-count)) acc) (count+1) 
      
let rec perm r_52 t_51 acc =
   match r_52 with
   | [] -> List.rev acc
   | h::t -> 
         let x = List.nth t_51 h in
         let t_51' = List.filter (fun x' -> x' <> x) t_51 in
         perm t (t_51') (acc@[x])
    
let shuffle n = 
   let p = paires 0 0 n 0 [] in
   let f1_init, f2_init = init_fifo p in
       
   let fifo_165 = fifo_ignored_165 0 f1_init f2_init in
   let f1_165 = fst fifo_165 in
   let f2_165 = snd fifo_165 in
    
   let t_52 = tirages_52 0 f1_165 f2_165 empty in
   let reduce_52 = tirage_reduce t_52 empty 0 in
    
   let t_51 = List.init 52 (fun x -> x) in
    
   perm (to_list reduce_52) t_51 [];;

  