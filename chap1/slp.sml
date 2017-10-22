type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
       | OpExp of exp * binop * exp
       | EseqExp of stm * exp

val prog =
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm
                (AssignStm("b",
                          EseqExp
                              (PrintStm
                                [IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
                               OpExp(NumExp 10, Times, IdExp"a"))),
                PrintStm[IdExp "b"]))

(* Return max number of arguments of any PrintStm.
 * E.g., maxargs prog -> 2
 *)
fun maxargs (s:stm) : int =
  let
    fun helperStm s =
      case s of
           PrintStm xs => List.foldl
                            (fn (e, acc) =>
                                Int.max (helperExp e, acc))
                            (List.length xs)
                            xs
         | AssignStm (_, e) =>
             helperExp e
         | CompoundStm (s1, s2) => Int.max (helperStm s1, helperStm s2)
    and helperExp e =
      case e of
           OpExp (e1, _, e2) => Int.max (helperExp e1, helperExp e2)
         | EseqExp (s, e) => Int.max (helperStm s, helperExp e)
         | _ => 0

  in
    helperStm s
  end


(* Straight-line program interpreter *)
datatype table = T of (id * int) list

fun update (T t, i:id, v:int) : table = T ((i,v)::t)

fun lookup (T t, i:id) : int option =
  case t of
      (j,v)::t' => if j = i
                      then SOME v
                      else lookup (T t', i)
    | [] => NONE

fun interpStm (s:stm, t:table):table =
  case s of
       CompoundStm (s1, s2) => 
        let
          val t' = interpStm (s1, t)
        in
          interpStm (s2, t')
        end
     | AssignStm (i, e) =>
        let
          val (v, _) = interpExp (e, t) (* throw away local env *)
        in
          update (t, i, v)
        end
     | PrintStm expList =>
        let
          val t'' = List.foldl
                    (fn (e:exp, acc:table) =>
                      let
                        val (v, t') = interpExp (e, acc)
                        val () = print ((Int.toString v) ^ "\n")
                      in
                        t'
                      end)
                    t
                    expList
        in
          t''
        end

and interpExp (e:exp, t:table):(int * table) =
  case e of
       IdExp i =>
         let
           val v = lookup (t, i)
         in
           if isSome v
           then (valOf v, t)
           else raise Fail ("Couldn't find '" ^ i ^ "' in lookup table")
         end
     | NumExp n => (n, t)
     | OpExp (e1, opp, e2) =>
         let
           val (v1, t') = interpExp (e1, t)
           val (v2, t'') = interpExp (e2, t')
         in
           case opp of
                Plus => (v1 + v2, t'')
              | Minus => (v1 - v2, t'')
              | Times => (v1 * v2, t'')
              | Div => (v1 div v2, t'')
         end
     | EseqExp (s, e') =>
         let
           val t' = interpStm (s, t)
         in
           interpExp (e', t')
         end

(* interp prog *)
fun interp (s:stm):unit =
  let
    val t = T []
    val _ = interpStm (s, t)
  in
    ()
  end

(* ex.1.1 *)
type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert (k, LEAF) = TREE (LEAF, k, LEAF)
  | insert (k, t as TREE (l,k',r)) =
      if k < k'
        then TREE (insert (k, l), k', r)
      else if k > k'
        then TREE (l,k',insert(k,r))
      else t

fun member (t,k) =
  case t of
       LEAF => false
     | TREE (l, k', r) =>
         if k = k'
           then true
         else if k < k'
           then member (l, k)
         else member (r, k)

(* val t = *)
(*   insert("t", *)
(*     insert("s", *)
(*       insert("b", *)
(*         insert("f", *)
(*           insert("p", *)
(*             insert("i", *)
(*               insert("p", *)
(*                 insert("s", *)
(*                   insert ("t", *)
(*                     empty))))))))); *)
(* val t' = *)
(*   insert("i", *)
(*     insert("h", *)
(*       insert("g", *)
(*         insert("f", *)
(*           insert("e", *)
(*             insert("d", *)
(*               insert("c", *)
(*                 insert("b", *)
(*                   insert ("a", *)
(*                     empty))))))))); *)



(* member ("a", t); *)
(* member ("t", t); *)


datatype 'a treee = E | T of 'a treee * (key * 'a) * 'a treee

fun insertt (E, k, v) = T (E,(k,v), E)
  | insertt (T (l,n as (k',_),r), k, v) =
      if k < k'
        then T (insertt (l, k, v), n, r)
      else if k > k'
        then T (l,n,insertt(r,k,v))
      else T (l,(k,v),r)

fun lookup (t, k) =
  case t of
       E => raise Fail ("key " ^ k ^ " does not exit")
     | T (l, n as (k',v), r) =>
         if k = k'
           then v
         else if k < k'
           then lookup (l, k)
         else lookup (r, k)
