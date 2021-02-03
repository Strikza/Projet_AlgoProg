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

let desequilibre(tree : 'a bst): int =
  let (ls, rs) = (lson(tree), rson(tree)) in
  if (isEmpty(ls) && isEmpty(rs))
  then 0
  else
    if isEmpty(ls)
    then height(ls) - (height(rs)+1)
    else
      if isEmpty(rs)
      then (height(ls)+1) - height(rs)
      else (height(ls)+1) - (height(rs)+1)
;;

desequilibre(test);;

let compute_imbalance() =
  let sum = ref 0.0 and
      n = 10000 in
  for i=0 to n do
    let t = bst_rnd_create(20) in
    sum := !sum +. float_of_int(desequilibre(t));
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
(* *)    sum := !sum +. float_of_int(desequilibre(t));
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
(* *)    sum := !sum +. float_of_int(desequilibre(t));
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
(* Le module AVL (à compléter une fois les fonctions finies) *)
module Avl =
  struct
    type 'a avl = (int*'a) bst;;

    let show_avl_int(tree : 'a avl) : unit = show((fun (h, root) -> (string_of_int h) ^ "||" ^ (string_of_int root)), tree)
    ;;

    let rg (tree : 'a avl): 'a avl =
      if isEmpty(tree) && isEmpty(rson(tree))
      then failwith"Function: rg (rotation gauche)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        let ((hR, rR), lsR, rsR) = (root(rs), lson(rs), rson(rs)) in
        let (nh, nhR) =
          if hR = 0
          then (0, 1)
          else (0, 0)
        in
        rooting((nhR, rR), rooting((nh, r), ls, lsR), rsR)
    ;;

    let rd (tree: 'a avl) : 'a avl =
      if isEmpty(tree) && isEmpty(lson(tree))
      then failwith"Function: rd (rotation droite)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        let ((hL, rL), lsL, rsL) = (root(ls), lson(ls), rson(ls)) in
        let (nh, nhL) =
          if hL = 0
          then (0, -1)
          else (0, 0)
        in
        rooting((nhL, rL), lsL, rooting((nh, r), rsL, rs))
    ;;

    let rgd (tree: 'a avl) : 'a avl =
      if isEmpty(tree) && isEmpty(lson(tree)) && isEmpty(lson(rson(tree)))
      then failwith"Function: rgd (rotation gauche-droite)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        rd(rooting((h, r), rg(ls), rs))
    ;;

    let rdg (tree: 'a avl) : 'a avl =
      if isEmpty(tree) && isEmpty(rson(tree)) && isEmpty(rson(lson(tree)))
      then failwith"Function: rgd (rotation droite-gauche)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        rg(rooting((h, r), ls, rd(rs)))
    ;;
    
     let rebalance_avl(tree: 'a avl) : 'a avl =
      if isEmpty(tree)
      then empty()
      else
        let ((i, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        begin
          match i with
          | 2 ->
             let ((i_L, r_L), ls_L, rs_L) = (root(ls), lson(ls), rson(ls)) in
             begin
               match i_L with
               | -1 -> rgd(tree)
               | 1 -> rd(tree)
               | _ -> failwith"DEBUG : rebalance_avl function"
             end
          | -2 ->
             let ((i_R, r_R), ls_R, rs_R) = (root(rs), lson(rs), rson(rs)) in
             begin
               match i_R with
               | 1 -> rdg(tree)
               | -1 -> rg(tree)
               | _ -> failwith"DEBUG : rebalance_avl function"
             end
          | _ -> tree
        end
    ;;

    let rec insert_avl(e, tree: 'a * 'a avl) : 'a avl =
      if isEmpty(tree)
      then rooting((0,e), empty(), empty())
      else
        let ((i, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        let final_tree =
          if e<r
          then rooting((i+1,r), insert_avl(e,ls), rs)
          else rooting((i-1,r), ls, insert_avl(e,rs))
        in
        rebalance_avl(final_tree) 
    ;;
    
  end
;;
(* --------------------------------------------------------- *)

open Avl;;
#show Avl;;

(* Zone de test *)

let tAvlGD = rooting((-1, 2),
                  rooting((0, 1), empty(), empty()),
                  rooting((0, 4),
                          rooting((0, 3), empty(), empty()),
                          rooting((0, 5), empty(), empty())))
;;
show_avl_int(tAvlGD);;


let tAvlRG = rg(tAvlGD);;
show_avl_int(tAvlRG);;

let tAvlRD = rd(tAvlRG);;
show_avl_int(tAvlRD);;

let tInsert1 = insert_avl(25,tAvlRG);;
show_avl_int(tInsert1);;

let tInsert1 = insert_avl(9,tInsert1);;
show_avl_int(tInsert1);;

let tInsert1 = insert_avl(11,tInsert1);;
show_avl_int(tInsert1);;



let tAvlRGD = rooting((2, 8),
                      rooting((-1, 3),
                              rooting((0, 1), empty(), empty()),
                              rooting((0, 6),
                                      rooting((0, 4), empty(), empty()),
                                      rooting((0, 7), empty(), empty()))
                        ),
                      rooting((0, 10), empty(), empty()))
;;
show_avl_int(tAvlRGD);;

let tInsert2 = insert_avl(-4,tAvlRGD);;
show_avl_int(tInsert2);;

let tAvlRGDBis = rgd(tAvlRGD);;
show_avl_int(tAvlRGDBis);;


let tAvlRDG = rooting((-2, 2),
                      rooting((0, 1), empty(), empty()),
                      rooting((1, 6),
                              rooting((0, 4),
                                      rooting((0, 3), empty(), empty()),
                                      rooting((0, 5), empty(), empty())),
                              rooting((0, 7), empty(), empty())))
;;
show_avl_int(tAvlRDG);;

let tAvlRDGBis = rdg(tAvlRDG);;
show_avl_int(tAvlRDGBis);;


;;

