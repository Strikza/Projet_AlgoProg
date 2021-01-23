#load "btree.cmo";;
open Btree;;
#show Btree;;

#use "bst.ml";;
open Bst;;
#show Bst;;

open List;;

open Random;;
#show Random;;

(**EXO1**)

(*Q1*)

let bst_rnd_create(size : int): 'a bst =
  Random.self_init();
  let l : int list ref = ref [] in
  for i=0 to size-1 do
    l := Random.int(1000)::(!l);
  done;
  bst_lbuild(!l)
;;

let test = randomBst(8);;
show_int_btree(test);;

(*Q2*)

let max(a, b) = Pervasives.max a b
;;

let rec height(tree : 'a t_btree) : int =
  if ( isEmpty(tree) || (isEmpty(lson(tree)) && isEmpty(rson(tree))) )
  then 0
  else (
   1 + max(height(lson(tree)), height(rson(tree)))
  )
;;


let compute_imbalance(size: int): int =
  let sum : int ref = ref 0 in
  for i=0 to 20 do
    let = bst_rnd_create(size) and
        (lh,rh) = (height(lson(t)),height(rson(t))) in
    sum := !sum + abs(lh-lr);
   done;
  sum := !sum/20
;;

   
   
