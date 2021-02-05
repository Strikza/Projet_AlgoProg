(** Authors :

    - Andrianarivony Henintsoa
    - Goubeau Samuel

 **)

(**
    Pour le module Bst, nous n'avons que 2 versions d'ocaml,
   donc il y a de forte chance qu'il faille compiler bst.ml
   si vous n'avez pas l'une des 2 versions déjà présente
 **)


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

(* Q.1.1 ============================================================================== *)

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


(* Q.1.2 ============================================================================== *)

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
(*  On remarque ici que lorsqu'on lance plusieurs fois la fonction, le          *)
(* résultat obtenu est (presque) systémtiquement le même, soit environ 0.       *)



(* Q.1.3 ============================================================================== *)


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
(* *)let compute_imbalanceFixedSuite(order, nb_suite : bool*int): float =
(* *)  let sum = ref 0.0 and
(* *)     n = nb_suite in
(* *) for i=0 to n do
(* *)    let l = fixedSuite(n, order, n) in
(* *)    let t = bst_lbuild(l) in 
(* *)    sum := !sum +. float_of_int(desequilibre(t));
(* *)   done;
(* *)  !sum/.float_of_int(n)
(* *);;
(* *)
(* *)(* Calcul le déséquilibre d'une suite ordonnée à longueur variable, croissant ou décroissant*)
(* *)let compute_imbalanceRandSuite(order, nb_suite : bool*int): float =
(* *)  let sum = ref 0.0 and
(* *)     n = nb_suite in
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
  (compute_imbalanceFixedSuite(false, 50) +.
     compute_imbalanceRandSuite(true, 50) +.
     compute_imbalanceFixedSuite(true, 50) +.
     compute_imbalanceRandSuite(false, 50))/.4.
;;

compute_imbalanceSuite();;

(*  En réalisant le même processus sur des suites ordonnées de nombres entiers, on remarque qu'il  *)
(* n'y a pas de moyenne précise, à contrario de la question 2 où l'on trouvait environ une moyenne *)
(* de 0. Ici on reamrque surtout un interval qui se dessine, qui est le suivant : [-5;5]. Mais la  *)
(* valeur de retour à tendance à plus se rapprocher de 0 que des bornes de l'interval.             *)



(**--------------------------------- Exercice 2 : Arbres AVL ----------------------------------**)

(* Le module AVL (à compléter une fois les fonctions finies) *)
module Avl =
  struct
    type 'a avl = (int*'a) bst;;
    (** L'entier ici sert à stocker le déséquilibre de l'arbre**)

    
    (** Afficher un avl**)
    let show_avl_int(tree : 'a avl) : unit =
      show((fun (h, root) -> (string_of_int h) ^ "||" ^ (string_of_int root)), tree)
    ;;

    
    (** Créer un avl vide **)
    let emptyAvl(): 'a avl = empty();;


    (** Acceder à la valeur de déséquilibre d'un noeud d'un arbre **)
    let balanceNode(tree : 'a avl): int =
      if isEmpty(tree)
      then 0
      else
        let (i, _) = root(tree) in
        i
    ;;


    (** Acceder à la valeur d'un noeud d'un arbre **)
    let rootNode(tree : 'a avl): 'a =
      let (_, r) = root(tree) in
      r
    ;;

    
    (* Q.2.1.1 ============================================================================== *)


    (** Effectuer une rotation gauche sur un arbre **)
    let rg (tree : 'a avl): 'a avl =
      if isEmpty(tree) && isEmpty(rson(tree))
      then failwith"Function: rg (rotation gauche)"
      else
        let ((i, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        let ((iR, rR), lsR, rsR) = (root(rs), lson(rs), rson(rs)) in
        let (ni, niR) =
          (
            desequilibre(rooting((0, rR), rooting((0, r), ls, lsR), rsR)),
            desequilibre(rooting((0, r), ls, lsR))
          )
        in
        rooting((ni, rR), rooting((niR, r), ls, lsR), rsR)
    ;;


    (** Effectuer une rotation droite sur un arbre **)
    let rd (tree: 'a avl) : 'a avl =
      if isEmpty(tree) && isEmpty(lson(tree))
      then failwith"Function: rd (rotation droite)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        let ((hL, rL), lsL, rsL) = (root(ls), lson(ls), rson(ls)) in
        let (ni, niL) =
          (
            desequilibre(rooting((0, rL), lsL, rooting((0, r), rsL, rs))),
            desequilibre(rooting((0, r), rsL, rs))
          )
        in
        rooting((ni, rL), lsL, rooting((niL, r), rsL, rs))
    ;;

    
    (** Effectuer une rotation gauche-droite sur un arbre **)
    let rgd (tree: 'a avl) : 'a avl =
      if isEmpty(tree) && isEmpty(lson(tree)) && isEmpty(lson(rson(tree)))
      then failwith"Function: rgd (rotation gauche-droite)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        rd(rooting((h, r), rg(ls), rs))
    ;;


    (** Effectuer une rotation droite-gauche sur un arbre **)
    let rdg (tree: 'a avl) : 'a avl =
      if isEmpty(tree) && isEmpty(rson(tree)) && isEmpty(rson(lson(tree)))
      then failwith"Function: rgd (rotation droite-gauche)"
      else
        let ((h, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        rg(rooting((h, r), ls, rd(rs)))
    ;;

    (* Q.2.1.2 ============================================================================== *)


    (** Rééquilibrer un arbre **)
     let rebalance_avl(tree: 'a avl) : 'a avl =
      if isEmpty(tree)
      then emptyAvl()
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
               | _ -> tree
             end
          | -2 ->
             let ((i_R, r_R), ls_R, rs_R) = (root(rs), lson(rs), rson(rs)) in
             begin
               match i_R with
               | 1 -> rdg(tree)
               | -1 -> rg(tree)
               | _ -> tree
             end
          | _ -> rooting((desequilibre(rooting((i,r), ls, rs)),r),ls,rs)
        end
     ;;

    (* Q.2.1.3 ============================================================================== *)


    (** Insersion d'un élément dans un arbre **)
    let rec insert_avl(e, tree: 'a * 'a avl) : 'a avl =
      if isEmpty(tree)
      then rooting((0,e), empty(), empty())
      else
        let ((_, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        let final_tree =
          if e<r
          then
            let nls = insert_avl(e,ls) in
            rooting((desequilibre(rooting((0, r), nls, rs)), r), nls, rs)
          else if e>r
          then
            let nrs = insert_avl(e,rs) in
            rooting((desequilibre(rooting((0, r), ls, nrs)), r), ls, nrs)
          else tree
        in
        rebalance_avl(final_tree) 
    ;;


    (** Recherche de l'élément maximum dans un arbre **)
    let rec max(tree: 'a avl) : 'a =
      if isEmpty(tree)
      then failwith"Function: rgd (rotation gauche-droite)"
      else
        let ((_, r), _, rs) = (root(tree), lson(tree), rson(tree)) in
        if isEmpty(rs)
        then r
        else
          max(rs)
    ;;


    (** Suppression de l'élément maximum d'un arbre **)
    let rec dmax(tree: 'a avl) : 'a avl =
       if isEmpty(tree)
      then failwith"Function: rgd (rotation gauche-droite)"
       else
         let ((d, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        if isEmpty(rs)
        then ls
        else
          rebalance_avl(rooting((d,r),ls,dmax(rs)))
    ;;
    

    (** Suppression d'un élément d'un arbre **)
    let rec suppr_avl(e, tree : 'a * 'a avl): 'a avl =
      if isEmpty(tree)
      then emptyAvl()
      else
        let ((i, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
        if e<r
        then rebalance_avl(rooting((i, r), suppr_avl(e, ls), rs))
        else if e>r
        then rebalance_avl(rooting((i, r), ls, suppr_avl(e, rs)))
        else
          if not(isEmpty(ls)) && not(isEmpty(rs))
          then
            rebalance_avl(rooting((i, max(ls)), dmax(ls), rs))
          else if not(isEmpty(rs))
          then rs
          else ls
    ;;   
    
  end
;;


open Avl;;
#show Avl;;

(* --- Zone de test du module Avl --- *)

(* Test d'insertion et de suppression dans un Avl *)
let avl = rooting((0,2),
                  rooting((0,1), empty(), empty()),
                  rooting((0,3), empty(), empty()))
;;
show_avl_int(avl);;

let avl = insert_avl(-1,avl);;
show_avl_int(avl);;

let avl = insert_avl(7,avl);;
show_avl_int(avl);;

let avl = insert_avl(25,avl);;
show_avl_int(avl);;

let avl = dmax(avl);;
show_avl_int(avl);;

let avl = suppr_avl(2,avl);;
show_avl_int(avl);;

let avl = suppr_avl(10,avl);;
show_avl_int(avl);;


(* Test des rotations gauche et droite *)
let tAvl = rooting((-1, 2),
                  rooting((0, 1), empty(), empty()),
                  rooting((0, 4),
                          rooting((0, 3), empty(), empty()),
                          rooting((0, 5), empty(), empty())))
;;
show_avl_int(tAvl);;

let tAvlRG = rg(tAvl);;
show_avl_int(tAvlRG);;

let tAvlRD = rd(tAvlRG);;
show_avl_int(tAvlRD);;


(* Test d'une rotation gauche-droite *)
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

let tAvlRGDBis = rgd(tAvlRGD);;
show_avl_int(tAvlRGDBis);;


(* Test d'une rotation droite-gauche *)
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



(* Q.1.4 ================================================================================= *)

(** Recherche d'un arbre, dans un arbre, par rapport un élément à sa racine **)
    let rec seek_avl(e, tree: 'a * 'a avl) :'a avl =
      let (seek1,seek2,seek3) = (bst_seek(tree, (0,e)), bst_seek(tree, (-1,e)), bst_seek(tree, (1,e))) in
      if isEmpty(seek1)
      then if isEmpty(seek2)
           then seek3
           else seek2
      else seek1
    ;;
    
(* Test de la fonction de recherche implémentant celle de bst *)
show_avl_int(avl);;

let seek1 = seek_avl(1,avl);;
show_avl_int(seek1);;

let seek2 = seek_avl(25,avl);;
show_avl_int(seek2);;



(* Q.2.2.1 ============================================================================== *)

(** Création d'un avl à partir d'une liste d'entiers **)
let rec avl_lbuild(l : 'a list): 'a avl =
  if l = []
  then emptyAvl()
  else insert_avl(hd(l), avl_lbuild(tl(l)))
;;


(** Génération d'un avl à partir de liste d'entiers aléatoires **)
let avl_rnd_create sizeBorn =
  Random.self_init();
  let l  = ref [] and size = Random.int(sizeBorn) in
  for i=0 to size do
    l := Random.int(100)::(!l);
  done;
  avl_lbuild(!l)
;;



(* Q.2.2.2 ============================================================================== *)

(** Rééquilibrer un arbre, en comptant le nombre de rotation **)
let rebalance_avl_bis(tree, count: 'a avl * int) : 'a avl * int =
  if isEmpty(tree)
  then (emptyAvl(), count)
  else
    let ((i, r), ls, rs) = (root(tree), lson(tree), rson(tree)) in
    begin
      match i with
      | 2 ->
         let ((i_L, r_L), ls_L, rs_L) = (root(ls), lson(ls), rson(ls)) in
         begin
           match i_L with
           | -1 -> (rgd(tree), count+2)
           | 1 -> (rd(tree), count+1)
           | _ -> (tree, count)
         end
      | -2 ->
         let ((i_R, r_R), ls_R, rs_R) = (root(rs), lson(rs), rson(rs)) in
         begin
           match i_R with
               | 1 -> (rdg(tree), count+2)
               | -1 -> (rg(tree), count+1)
               | _ -> (tree, count)
         end
      | _ -> (tree, count)
    end
;;


(** Insersion d'un élément dans un arbre, en comptant le nombre de rotation **)
let rec insert_avl_bis(e, tree, count: 'a * 'a avl * int) : 'a avl * int =
  if isEmpty(tree)
  then (rooting((0,e), empty(), empty()), count)
  else
    let ((_, r), ls, rs) = (root(tree), lson(tree), rson(tree)) and
        countBis = ref count in
    let final_tree =
      if e<r
      then
        let (nls, countTmp) = insert_avl_bis(e,ls, count) in
        countBis := countTmp;
        rooting((desequilibre(rooting((0, r), nls, rs)), r), nls, rs)
      else if e>r
      then
        let (nrs, countTmp) = insert_avl_bis(e, rs, count) in
        countBis := countTmp;
        rooting((desequilibre(rooting((0, r), ls, nrs)), r), ls, nrs)
      else tree
    in
    rebalance_avl_bis(final_tree, !countBis)
;;

(** Création d'un avl à partir d'une liste d'entiers, , en comptant le nombre de rotation **)
let rec avl_lbuild_bis(l, count : 'a list * int): 'a avl * int =
  if l = []
  then (emptyAvl(), count)
  else
    let (treetmp, counttmp) = avl_lbuild_bis(tl(l), count) in
    insert_avl_bis(hd(l), treetmp, counttmp)
;;

(* Test des fonctions avec un compteur : *)

(* avec insert_avl *)
let a0 = emptyAvl();;
show_avl_int(a0);;
let (a1, count1) = insert_avl_bis(1, a0, 0);;
show_avl_int(a1);;
let (a2, count2) = insert_avl_bis(2, a1, count1);;
show_avl_int(a2);;
let (a3, count3) = insert_avl_bis(3, a2, count2);;
show_avl_int(a3);;
let (a4, count4) = insert_avl_bis(4, a3, count3);;
show_avl_int(a4);;
let (a5, count5) = insert_avl_bis(5, a4, count4);;
show_avl_int(a5);;
(* avec avl_lbuild *)
let (a6, count6) = avl_lbuild_bis([1;2;3;4;5], 0);;
show_avl_int(a6);;
let (a7, count7) = avl_lbuild_bis([1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;18;19;20], 0);;
show_avl_int(a7);;


(** Calcul la rotation moyenne pour la création d'un Avl avec des listes (sous suite fixe) **)
let average_rotation_fixed_suite(order, nb_tree : bool*int): float =
  let nb_rota = ref 0.0 in
  for i=0 to nb_tree do
    let l = fixedSuite(300, order, 10) in
    let (_, count) = avl_lbuild_bis(l, 0) in 
    nb_rota := !nb_rota +. float_of_int(count);
   done;
  !nb_rota/.float_of_int(nb_tree)
;;

(** Calcul la rotation moyenne pour la création d'un Avl avec des listes (sous suite non-fixe) **)
let average_rotation_rand_suite(order, nb_tree : bool*int): float =
  let nb_rota = ref 0.0 in
  for i=0 to nb_tree do
    let l = randSuite(200, order) in
    let (_, count) = avl_lbuild_bis(l, 0) in 
    nb_rota := !nb_rota +. float_of_int(count);
  done;
  !nb_rota/.float_of_int(nb_tree)
;;

(** Calcul la rotation moyenne pour la création d'un Avl avec des listes **)
let average_rotation_suite() =
  (average_rotation_fixed_suite(true, 50)+.
   average_rotation_fixed_suite(false, 50)+.
   average_rotation_rand_suite(true, 50)+.
   average_rotation_rand_suite(false, 50))/.4.
;;

average_rotation_suite();;
(*  On obtient en résultat une moyenne d'environ 122 rotations par avl construit *)
(* avec ce type de liste                                                         *)
