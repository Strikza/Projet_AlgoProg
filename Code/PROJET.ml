#load "btree.cmo";;
open Btree;;
#show Btree;;

#load "bst.cmo";;
open Bst;;
(** Il se peut qu'il faille charger 2 fois cette commande **)
(*  C'est un problème que l'on a rencontré plusieur fois   *)
#show Bst;;

open List;;
open Random;;
#show Random;;


(**--------------------------- Exercice 1 : Arbres Binaires de Recherche ---------------------------**)

(*=/1/============================================================================== *)

let bst_rnd_create sizeBorn =
  Random.self_init();
  let l  = ref [] and
      size = Random.int(sizeBorn) in
  for i=0 to size do
    l := Random.int(100)::(!l);
  done;
  bst_lbuild(!l)
;;

let test = bst_rnd_create(20);;
show_int_btree(test);;


(*=/2/============================================================================== *)

let compute_imbalance() =
  let sum = ref 0.0 and
      n = 10000 in
  for i=0 to n do
    let t = bst_rnd_create(20) in
    sum := !sum +. float_of_int(height(lson(t)) - height(rson(t)));
   done;
  !sum/.float_of_int(n)
;;

compute_imbalance();;
(*  On remarque ici que lorsqu'on lance plusieurs fois la fonction, le *)
(* résultat est presque systémtiquement le même, soit environ 0.       *)



(*=/3/============================================================================== *)


(* *** Sous-fonctions *** *)
(* *)(* Créé une sous-suite croissante *)
(* *)
(* *)let rec incSubSuite(n, len : int*int): int list =
(* *)  if len=0
(* *)  then []
(* *)  else
(* *)    n::incSubSuite(n+1, len-1)
(* *);;
(* *)
(* *)(* Créé une sous-suite décroissante *)
(* *)let rec decSubSuite(n, len : int*int): int list =
(* *)  if len=0
(* *)  then []
(* *)  else
(* *)    n::decSubSuite(n-1, len-1)
(* *);;
(* *)
(* *)(* Créé une sous suite à longueur fixe *)
(* *)let fixedSubSuite(order, n, len : bool*int*int): int list =
(* *)  if order
(* *)  then incSubSuite(n, len)
(* *)  else decSubSuite(n, len)
(* *);;
(* *)
(* *)(* Créé une sous-suite à longueur variable *)
(* *)let randSubSuite(order, n : bool*int): int list =
(* *)  let len = Random.int(10)+1 in
(* *)  fixedSubSuite(order, n, len)
(* *);;
(* *)
(* ********************** *)
(* *)
(* *)(* Créé une liste de sous-suite ordonnées à longueur fixe *)
(* *)let rec fixedSuite(len, order, lenSubSuite : int*bool*int): int list =
(* *)  if len=0
(* *)  then []
(* *)  else
(* *)    let n = Random.int(100) in
(* *)    fixedSubSuite(order, n, lenSubSuite)@fixedSuite(len-1, order, lenSubSuite)
(* *);;
(* *)
(* *)
(* *)(* Créé une liste de sous-suite ordonnées à longueur variable *)
(* *)let rec randSuite(len, order : int*bool): int list =
(* *)  if len=0
(* *)  then []
(* *)  else
(* *)    let n = Random.int(100) in
(* *)    randSubSuite(order, n)@randSuite(len-1, order)
(* *);;
(* *)
(* ********************** *)
(* *)
(* *)(* Calcul le déséquilibre d'une suite ordonnée à longueur fixe, croissant ou décroissant*)
(* *)let compute_imbalanceFixedSuite(order : bool): float =
(* *)  let sum = ref 0.0 and
(* *)     n = 50 in
(* *) for i=0 to n do
(* *)    let l = fixedSuite(n, order, n) in
(* *)    let t = bst_lbuild(l) in 
(* *)    sum := !sum +. float_of_int(height(lson(t)) - height(rson(t)));
(* *)   done;
(* *)  !sum/.float_of_int(n)
(* *);;
(* *)
(* *)(* Calcul le déséquilibre d'une suite ordonnée à longueur variable, croissant ou décroissant*)
(* *)let compute_imbalanceRandSuite(order : bool): float =
(* *)  let sum = ref 0.0 and
(* *)     n = 50 in
(* *) for i=0 to n do
(* *)    let l = randSuite(n, order) in
(* *)    let t = bst_lbuild(l) in 
(* *)    sum := !sum +. float_of_int(height(lson(t)) - height(rson(t)));
(* *)   done;
(* *)  !sum/.float_of_int(n)
(* *);;
(* *)
(* ********************** *)


(* Calcul le déséquilibre d'une suite ordonnée à longueur fixe ou aléatoire, croissant ou décroissant*)
let compute_imbalanceSuite() =
  (compute_imbalanceFixedSuite(false) +.
     compute_imbalanceRandSuite(true) +.
     compute_imbalanceFixedSuite(true) +.
     compute_imbalanceRandSuite(false))/.4.
;;

compute_imbalanceSuite();;

(*  En réalisant le même processus sur des suites ordonnées de nombres entiers, on remarque qu'il  *)
(* n'y a pas de moyenne précise, à contrario de la question 2 où l'on trouvait environ une moyenne *)
(* de 0. Ici on reamrque surtout un interval qui se dessine, qui est le suivant : [-5;5]. Mais la  *)
(* valeur de retour à tendance à plus se rapprocher de 0 que des bornes de l'interval.             *)



(**----------------------------------- Exercice 2 : Arbres AVL -------------------------------------**)
