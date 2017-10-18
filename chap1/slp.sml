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
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* Return max number of arguments of any PrintStm.
 * E.g., maxargs prog -> 2
 *)
fun maxargs (s: stm) : int =
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


