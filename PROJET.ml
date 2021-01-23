#load "btree.cmo";;
open Btree;;
#show Btree;;

#use "bst.ml";;
open Bst;;
#show Bst;;

open List;;
open Random;;
#show Random;;

let randomBst(size : int): 'a bst =
  Random.self_init();
  let l : int list ref = ref [] in
  for i=0 to size-1 do
    l := Random.int(1000)::(!l);
  done;
  bst_lbuild(!l)
;;

let test = randomBst(8);;
show_int_btree(test);;
