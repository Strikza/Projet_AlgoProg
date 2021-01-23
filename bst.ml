(*#directory"G:/Cours_Univ/L3_Informatique/Semestre_5/Algo_Prog/Ressources/bytecodes/4.02.1+ocp1";; (* à lancer sur Windows *)*)
#directory"/Volumes/Disque_Sam/Cours_Univ/L3_Informatique/Semestre_5/Algo_Prog/Ressources/bytecodes/4.10.0";;

#load "btree.cmo";;
open Btree;;
open List;; 

module type Bst =
  sig
    type 'a bst = 'a t_btree;;

    val bst_seek : 'a bst * 'a -> 'a bst
    val bst_linsert : 'a bst * 'a -> 'a bst
    val bst_lbuild : 'a list -> 'a bst
  end 
;;

module Bst : Bst =
  struct
    type 'a bst = 'a t_btree;;
    let rec bst_seek(b, v : 'a bst * 'a): 'a bst =
      if isEmpty(b)
      then b
      else
        let (r, ls, rs) : ('a * 'a bst * 'a bst) = (root(b), lson(b), rson(b)) in

        if r = v
        then b
        else
          if r > v
          then bst_seek(ls, v)
          else bst_seek(rs, v)
    ;;

    let rec bst_linsert(b, v : 'a bst * 'a): 'a bst =
      if isEmpty(b)
      then rooting(v, empty(), empty())
      else
        let (r, ls, rs) : ('a * 'a bst * 'a bst) = (root(b), lson(b), rson(b)) in

        if r > v
        then rooting(r, bst_linsert(ls, v), rs)
        else rooting(r, ls, bst_linsert(rs, v))
    ;;

    let rec bst_lbuild(l : 'a list): 'a bst =
      if l = []
      then empty()
      else bst_linsert(bst_lbuild(tl(l)), hd(l))
    ;;
    
  end
;;



open Bst;;
#show Bst;;
