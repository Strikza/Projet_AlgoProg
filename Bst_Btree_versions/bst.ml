open Btree;;
open List;;

module Bst : Bst =
  struct
    type 'a bst = 'a t_btree;;
    
    let max2(x, y): int =
      if x>y
      then x
      else y
    ;;
    
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

    let rec height(bt : 'a bst): int =
      if(isEmpty(bt))
      then 0
      else
        let (ls, rs) = (lson(bt), rson(bt)) in
        if(isEmpty(ls) && isEmpty(rs))
        then 0
        else
          1 + max2(height(ls), height(rs))
    ;;
  end
;;
