#directory"G:/Cours_Univ/L3_Informatique/Semestre_5/Algo_Prog/Ressources/bytecodes/4.02.1+ocp1";; (* à lancer sur Windows *)
#directory"/Volumes/Disque_Sam/Cours_Univ/L3_Informatique/Semestre_5/Algo_Prog/Ressources/bytecodes/4.10.0";;

#load "btree.cmo";;
open Btree;;
#show Btree;;

#use "bst.ml";;
open Bst;;

(** EXERCICE 1 **)

let test_bst : int bst = rooting(4,
                                 rooting(3,
                                         rooting(1, empty(), empty()),
                                         rooting(2, empty(), empty())),
                                 rooting(6,
                                         rooting(5, empty(), empty()),
                                         rooting(7, empty(), empty())))
;;

show_int_btree(test_bst);;
#trace bst_seek;;
root(bst_seek(test_bst, 7));;

let test_bstBis = bst_linsert(test_bst, 8);;
show_int_btree(test_bstBis);;
root(bst_seek(test_bstBis, 8));;

let l_bst = 2::3::1::5::6::[];; (* [6; 5; 1; 3; 2] *)
let test_bstBisBis = bst_lbuild(l_bst);;
show_int_btree(test_bstBisBis);;
root(bst_seek(test_bstBisBis, 2));;
