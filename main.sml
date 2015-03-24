structure Main =
struct

fun print_prog (prog_ir : MipsFrame.frag list) = 
    foldl (fn (exp, ()) =>
              case exp of MipsFrame.PROC {body, frame} =>
			  (print ("************** For function " ^ (Symbol.name (MipsFrame.name frame)) ^": **************\n");
			   Printtree.printtree(TextIO.stdOut, body))
			| _ => ()) () prog_ir
          
(*fun main str = testAll (fn file => print_prog(Semant.transProg(Parse.parse file))) str*)


fun compile fname =
    let val ast = Parse.parse fname
	val () = PrintAbsyn.print(TextIO.stdOut, ast)
	val ir = (FindEscape.findEscape(ast);
                  Semant.transProg ast)
    in
	print_prog ir
    end
(*
fun main str = Semant.transProg (Parse.parse str)
*)

end
