structure MipsFrame : FRAME =
struct
  
  datatype access =  InFrame of int
                   | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, locals: int ref, offset:int ref}
  val wordSize = 4
  val RV = Temp.newtemp()
  val FP = Temp.newtemp()
  datatype frag = PROC of {body:Tree.stm, frame:frame}
		| STRING of Temp.label * string
		  
  fun name (f:frame) = #name f
  
  fun formals (f:frame) = #formals f
  
  fun allocLocal ({name = name, formals = formals, offset = offset, locals = locals}) =
      let val CurrOffset = offset		  
      fun allocLoc(escape) = if escape
			     then (CurrOffset := !CurrOffset - wordSize;(locals := !locals+1);InFrame(!CurrOffset))
			     else InReg(Temp.newtemp())
      in
	  allocLoc
      end
      
  fun newFrame ({name:Temp.label, formals = formals}) =
      let val tempoffset = ref 0
	  fun allocFrame(escape) = if escape
				   then (tempoffset := !tempoffset -wordSize; InFrame(!tempoffset))
				   else InReg(Temp.newtemp())
      in
	  {name = name, formals = (map allocFrame formals),offset = tempoffset, locals = ref 0 }
      end

				  
  fun exp (InFrame(k)) = 
      let
	  fun getexp(framePointer) =
	      Tree.MEM(Tree.BINOP(Tree.PLUS, framePointer, Tree.CONST(k)))
      in
       getexp
      end
    | exp (InReg(temp)) = 
      let
	  fun getexp(_) = Tree.TEMP(temp)
      in
          getexp
      end 

    fun externalCall (funcname, args) =
       Tree.CALL(Tree.NAME(Temp.namedlabel funcname), args)

    fun procEntryExit1 (frame,body)=body
	
end
