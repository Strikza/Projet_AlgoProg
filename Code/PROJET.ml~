#load "btree.cmo";;
open Btree;;
#show Btree;;

#load "bst.cmo";;
open Bst;;
#show Bst;;

open List;;
open Random;;
#show Random;;


(**------------ Exercice 1 : Arbre Binaire de Recherche -------------**)

(*=/1/============================================== *)

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


(*=/2/============================================== *)

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
(* résultat est presque systémtiquement le même, environ 0.            *)



(*=/3/============================================== *)






