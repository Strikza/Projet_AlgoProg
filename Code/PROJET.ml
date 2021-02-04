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
  if isEmpty(tree)
  then 0
  else
    let (ls, rs) = (lson(tree), rson(tree)) in
    if (isEmpty(ls) && isEmpty(rs))
    then 0
    else
      if isEmpty(ls)
      then 0 - (height(rs)+1)
      else
        if isEmpty(rs)
        then (height(ls)+1)
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
    (** L'entier ici sert à stocker le déséquilibre **)

    
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
         
    
  end
;;
(* --------------------------------------------------------- *)

open Avl;;
#show Avl;;

(* Zone de test *)

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

zlet tAvlGD = rooting((-1, 2),
                  rooting((0, 1), empty(), empty()),
                  rooting((0, 4),
                          rooting((0, 3), empty(), empty()),
                          rooting((0, 5), empty(), empty())))
;;
show_avl_int(tAvlGD);;

let t = dmax(tAvlGD);;
show_avl_int(t);;

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

let oui = dmax(tInsert1);;
show_avl_int(oui);;



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

let tInsert2 = insert_avl(-4,tAvlRGDBis);;
show_avl_int(tInsert2);;

let dmax


let tAvlRDG = rooting((-2, 2),
                      rooting((0, 1), empty(), empty()),
                      rooting((1, 6),
                              rooting((0, 4),
                                      rooting((0, 3), empty(), empty()),
                                      rooting((0, 5), empty(), empty())),
                              rooting((0, 7), empty(), empty())))
;;
show_avl_int(tAvlRDG);;

let t1 = dmax(tAvlRDG);;
show_avl_int(t1);;

let t2 = suppr_avl(4, tAvlRDG);;
show_avl_int(t2);;



let tAvlRDGBis = rdg(tAvlRDG);;
show_avl_int(tAvlRDGBis);;

let myAvl : int avl = emptyAvl();;
show_avl_int(myAvl);;

show_avl_int((insert_avl(2, myAvl)));;
show_avl_int((insert_avl(4, insert_avl(2, myAvl))));;
show_avl_int((insert_avl(1, insert_avl(4, insert_avl(2, myAvl)))));;
show_avl_int((insert_avl(4, insert_avl(1, insert_avl(4,insert_avl(2, myAvl))))));;
show_avl_int((insert_avl(3, insert_avl(5, insert_avl(1, insert_avl(4, insert_avl(2, myAvl)))))));;
show_avl_int(insert_avl(7, (insert_avl(3, insert_avl(5, insert_avl(1, insert_avl(4, insert_avl(2, myAvl))))))));;

