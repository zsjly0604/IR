signature TRANSLATE =
sig
   
    type access
    type frag

    datatype exp =  Ex of Tree.exp
                  | Nx of Tree.stm
                  | Cx of Temp.label * Temp.label -> Tree.stm
    datatype level =  outermost
                    | nestLevel of {frame: MipsFrame.frame, parent: level} * unit ref
    val newLevel : {parent: level, name: Temp.label,
                    formals: bool list} -> level
    val mainLevel : level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
    
    val newDoneLabel : unit -> Temp.label

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> Temp.label * Temp.label -> Tree.stm

    val frags : MipsFrame.frag list ref
    val reset : unit -> unit
    val getResult : unit -> MipsFrame.frag list

    val seq : Tree.stm list -> Tree.stm

    val nilExp : unit -> exp
    val intExp : int -> exp
    val stringExp : string -> exp
    val simpleVar : access * level -> exp
    val subscriptVar : exp * exp -> exp
    val fieldVar : exp * int -> exp
    val assignExp : exp * exp -> exp
    val seqExp : exp list * exp -> exp
    val binop : Absyn.oper * exp * exp -> exp
    val relop : Absyn.oper * exp * exp -> exp
    val ifthenExp : exp * exp -> exp
    val ifthenelseExp : exp * exp * exp -> exp
    val whileExp : exp * exp * Temp.label option -> exp
    val forExp : exp * exp * exp * exp * Temp.label option -> exp
    val breakExp : Temp.label option -> exp
    val callExp : level * Tree.label * level * exp list * bool -> exp
    val recordExp : exp list -> exp
    val arrayExp : exp * exp -> exp
    val procEntryExit : {level:level, body:exp} -> unit
end
