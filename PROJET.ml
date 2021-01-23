#load "btree.cmo";;
open Btree;;
#show Btree;;

#use "bst.ml";;
open Bst;;
#show Bst;;

open List;;
open Random;;
#show Random;;

let bst_rnd_create(): 'a bst =
  Random.self_init();
  let l : int list ref = ref [] and
      size = Random.int(20) in
  for i=0 to size-1 do
    l := Random.int(100)::(!l);
  done;
  bst_lbuild(!l)
;;

let test = bst_rnd_create();;
show_int_btree(test);;
