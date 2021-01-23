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

let bst_rnd_create(sizeBorn : int): 'a bst =
  Random.self_init();
  let l : int list ref = ref [] and
      size = Random.int(sizeBorn) in
  for i=0 to size do
    l := Random.int(100)::(!l);
  done;
  bst_lbuild(!l)
;;

let test = bst_rnd_create(20);;
show_int_btree(test);;


(* ================================================================== *)


let max(x, y : int*int): int =
  if(x<y)
  then y
  else x
;;

let rec height(btree : 'a t_btree): int =
  if(isEmpty(btree))
  then 0
  else
    let (ls, rs) = (lson(btree), rson(btree)) in
    if(isEmpty(ls) && isEmpty(rs))
    then 0
    else
      1 + max(height(ls), height(rs))
;;

height(test);;


let compute_imbalance(): float =
  let sum : float ref = ref 0.0 and
      n = 10000 in
  for i=0 to n do
    let t = bst_rnd_create(20) in
    sum := !sum +. float_of_int(height(lson(t)) - height(rson(t)));
   done;
  !sum/.float_of_int(n)
;;

compute_imbalance();;


(* ================================================================== *)


