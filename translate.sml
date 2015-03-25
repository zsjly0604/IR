structure Translate : TRANSLATE = 
struct
  structure A = Absyn
  datatype exp =  Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm 
    
  datatype level =  outermost
                  | nestLevel of {frame: MipsFrame.frame, parent: level} * unit ref
  
  type access = level * MipsFrame.access
  type frag = MipsFrame.frag
  
  val frags = ref []: frag list ref
  val Impossible = ErrorMsg.impossible

   fun seq ([s]) = s
     | seq (h::t) = Tree.SEQ(h, seq(t)) 
     | seq [] = Impossible "Can't seq empty list"
   
   fun unEx (Ex e) = e
     | unEx (Cx genstm) = let val r = Temp.newtemp()
                              val t = Temp.newlabel() and f = Temp.newlabel()
                          in
                            Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                                      genstm(t, f),
                                      Tree.LABEL f,
                                      Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
                                      Tree.LABEL t],
                                      Tree.TEMP r)
                         end
     | unEx (Nx s) = case s of 
                        Tree.EXP(exp) => exp 
		      | _  => Tree.ESEQ(s, Tree.CONST 0)

   fun unNx (Ex e) = Tree.EXP e
     | unNx (Cx genstm) = let val t = Temp.newlabel() and f = Temp.newlabel()
                          in
                            genstm(t, f)
                          end
     | unNx (Nx s) = s

   fun unCx (Ex e) = (fn (t, f) => Tree.CJUMP(Tree.EQ, e, Tree.CONST 1, t, f))
     | unCx (Cx genstm) = genstm
     | unCx (Nx s) = Impossible "Can't convert Nx to Cx"  
  
   fun newLevel ({parent: level, name: Temp.label, formals: bool list}) =
      let
	  val formals' = true::formals
	  val newframe = MipsFrame.newFrame({name=name, formals = formals'})
      in
	  nestLevel({parent=parent, frame=newframe}, ref())
      end

  fun allocLocal (nestLevel({parent=parent, frame=frame}, unique)) = 
    let 
      fun allocL (escape:bool) = 
        let
          val frameAccess = MipsFrame.allocLocal frame escape
        in
          (nestLevel({parent=parent, frame=frame}, unique), frameAccess)
        end
    in
      allocL
    end
    | allocLocal(outermost) = (Impossible "Error: cannot allocal local variable in ROOT level!")

  fun procEntryExit ({level= nestLevel ({frame=frame, parent=parent}, unique), body = body}) = 
    let
      val body' = MipsFrame.procEntryExit1(frame, unNx body)
      val moveStm = Tree.MOVE(Tree.TEMP MipsFrame.RV, unEx body)
    in
      frags := (MipsFrame.PROC {body = moveStm, frame = frame})::(!frags)
    end
    | procEntryExit({level= outermost, body=body}) = (Impossible "Error: no function should be at the ROOT level!")       

   fun formals (nestLevel({parent=parent, frame=frame}, unique)) =
       let
	   val frameformals = #formals frame
	   fun makeTformal (level, []) = []
	     | makeTformal (level, frameformals::tail) = (level,frameformals)::makeTformal(level,tail)
       in
	   makeTformal(nestLevel({parent=parent,frame=frame},unique),frameformals)
       end
      | formals(outermost) = (Impossible "no formals at outermost")
	       
   fun followSL (nestLevel(defLevel), nestLevel(useLevel)) =
      let val dref = #2 defLevel
          val uref = #2 useLevel
          val {frame = uframe, parent = uparent} = #1 useLevel
      in
        if dref = uref then Tree.TEMP(MipsFrame.FP)
        else
          let val locals = #locals uframe
          in
            Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST (!locals), followSL(nestLevel(defLevel), uparent)))  
          end
       end
     | followSL (nestLevel(defLevel), outermost) = Tree.TEMP MipsFrame.FP
     | followSL (outermost, _) = Impossible "No function defined at outermost"
 
  fun reset () = frags := nil 
  fun getResult () = !frags

  fun newDoneLabel () = Temp.newlabel()
  
  val mainLevel = newLevel({name = Temp.namedlabel "main", formals = [], parent = outermost})

  fun nilExp () = Ex(Tree.CONST 0)

  fun intExp (n) = Ex(Tree.CONST n)
 
  fun stringExp (str) = 
    let val strExist = List.find(fn (x) => case x of
                               MipsFrame.PROC(_) => false
                             | MipsFrame.STRING(lab, s) => s = str) (!frags)
    in
      case strExist of
          SOME(MipsFrame.STRING(lab, s)) => Ex(Tree.NAME lab)
        | NONE => let val lab = Temp.newlabel()
                  in
                     !frags = MipsFrame.STRING(lab, str) :: (!frags);
                     Ex(Tree.NAME lab)
                  end
	| _  => Impossible "string can't stored as function"
     end  

  fun simpleVar ((defLevel, frameAccess), useLevel) =
    let val staticLink = followSL(defLevel, useLevel)
        val varBase = MipsFrame.exp(frameAccess) staticLink
    in    
        Ex(MipsFrame.exp(frameAccess) staticLink)
    end
  
  fun subscriptVar (arrayBase, index) =
    let val arrayBase = unEx arrayBase
        val index = unEx index
    in
      case index of
        Tree.CONST n => Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, arrayBase, Tree.CONST(MipsFrame.wordSize * n))))
       | _  => Impossible "Index must be an integer" 
    end

  fun fieldVar (recordBase, index) =
    let val recordBase = unEx recordBase
    in
      Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, recordBase, Tree.CONST (MipsFrame.wordSize * index))))
    end

  fun assignExp (left, right) =
    let val left = unEx left
        val right = unEx right
    in
      Nx(Tree.MOVE(left, right))
    end

  fun seqExp (expFront, expLast) =
      case expFront of
          [] => (case expLast of
                  Nx(s) => expLast
		| _ => Ex(unEx expLast))
	 | _  => (case expLast of
                  Nx(s) => Nx(Tree.SEQ(seq(map unNx expFront), s))
		| _ => Ex(Tree.ESEQ(seq(map unNx expFront), unEx expLast)))

  fun binop (oper, left, right) =
     let val troper = case oper of
                         A.PlusOp => Tree.PLUS
		       | A.MinusOp => Tree.MINUS
		       | A.TimesOp => Tree.MUL
		       | A.DivideOp => Tree.DIV
		       | _ => Impossible "Invalid binop operator"
         val left = unEx left
         val right = unEx right
     in
       Ex(Tree.BINOP(troper, left, right))
     end
   
  fun relop (oper, left, right) =
    let val troper = case oper of
                        A.LeOp => Tree.LE
		      | A.LtOp => Tree.LT
		      | A.GeOp => Tree.GE
		      | A.GtOp => Tree.GT
		      | A.EqOp => Tree.EQ
		      | A.NeqOp => Tree.NE
		      | _ => Impossible "Invalid relop operator" 
         val left = unEx left
         val right = unEx right
    in
      Cx(fn(t, f) => Tree.CJUMP(troper, left, right, t, f))
    end      

  fun ifthenExp (test, then') =
    let val t = Temp.newlabel() and f = Temp.newlabel()
        val test = unCx test
        val then' = unNx then'
    in
      Nx(seq[test(t, f),
             Tree.LABEL t,
             then',
             Tree.LABEL f])
    end

   fun ifthenelseExp (test, then', else') =
    let val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
        val done_label = Temp.newlabel()
        val test = unCx test
    in
      case then' of
         Ex(e) => Ex(Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP r, unEx(then')), 
                                   test(t, f),
                                   Tree.LABEL f,
                                   Tree.MOVE(Tree.TEMP r, unEx(else')),
                                   Tree.LABEL t],
                       Tree.TEMP r))
       |  _  => Nx(seq[test(t, f),
                     Tree.LABEL t,
                     unNx(then'),
                     Tree.JUMP(Tree.NAME(done_label), [done_label]),
                     Tree.LABEL f,
                     unNx(else'),
                     Tree.LABEL done_label])
    end

  fun whileExp (test, body, SOME(done_label)) =
    let val test_label = Temp.newlabel() and body_label = Temp.newlabel()
        val test = unCx test
        val body = unNx body
    in 
      Nx(seq[Tree.LABEL test_label,
             test(body_label, done_label),
             Tree.LABEL body_label,
             body,
             Tree.JUMP(Tree.NAME test_label, [test_label]),
             Tree.LABEL done_label])
    end
    | whileExp (test, body, NONE) = Impossible "While loop done_label can't be NONE" 
  
  fun forExp (var, lo, hi, body, SOME(done_label)) =
    let val body_label = Temp.newlabel()
        val var = unEx var
        val lo = unEx lo
        val hi = unEx hi
    in
      Nx(seq[Tree.MOVE(var, lo),
             Tree.CJUMP(Tree.LE, var, hi, body_label, done_label),
             Tree.LABEL body_label,
             unNx body,
             Tree.CJUMP(Tree.LT, var, hi, body_label, done_label),
             Tree.MOVE(var, Tree.BINOP(Tree.PLUS, var, Tree.CONST 1)),
             Tree.LABEL done_label
             ])
    end
    | forExp (var, lo, hi, body, NONE) = Impossible "For loop done_label can't be NONE" 

   fun breakExp (done_label) = case done_label of
                                 SOME(lab) => Nx(Tree.JUMP(Tree.NAME lab, [lab]))
			       | NONE => Impossible "Can't break at label NONE" 

   fun callExp (defLevel, funcLabel, useLevel, args, isprocedure) = 
     let val staticLink = followSL(defLevel, useLevel) 
         val Exargs = map unEx args
     in
       case isprocedure of
          true => Nx(Tree.EXP(Tree.CALL(Tree.NAME funcLabel, staticLink::Exargs)))
	| false =>Ex(Tree.CALL(Tree.NAME funcLabel, staticLink::Exargs)) 
     end
                             
   fun recordExp (fields) = 
     let val r = Temp.newtemp()
         val size = List.length(fields)
         val recordBase = Tree.MOVE(Tree.TEMP r, MipsFrame.externalCall("allocRecord", [Tree.CONST (size * MipsFrame.wordSize)]))
         fun initialize (fields, index) =
           case fields of
             [] => []
	   | h::t => Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP r, Tree.CONST (MipsFrame.wordSize * index))), unEx(h))::initialize(t, index + 1)
     in
        Ex(Tree.ESEQ(seq(recordBase::initialize(fields, 0)), Tree.TEMP r))
     end

  fun arrayExp (size, init) =
     Ex(MipsFrame.externalCall("initArray", [unEx size, unEx init]))

end
