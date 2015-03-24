signature SEMANT =
sig
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  val transExp: (venv * tenv * Translate.level * Temp.label option) ->  Absyn.exp -> expty
  val transVar: (venv * tenv * Translate.level * Temp.label option) -> Absyn.var -> expty
  val transDec: venv * tenv * Absyn.dec * Temp.label option * Translate.level * Translate.exp list-> {venv: venv, tenv: tenv, explist: Translate.exp list}
  val transTy:  tenv * Absyn.ty -> Types.ty
  val transProg: Absyn.exp -> Translate.frag list
end

structure Semant :> SEMANT = struct
  structure A = Absyn
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}
  val err = ErrorMsg.error
   
  fun actual_ty (Types.NAME(s, ty)) =
    (case !ty of
         NONE => Types.NAME(s,ty)
      | SOME t => actual_ty t)
    | actual_ty t = t 

  fun checkInt (ty, pos, des) =
    case ty of
      Types.INT => true
    | _ => (err pos ("Integer required: " ^ des);false)
  
  fun checkString (ty, pos, des) =
    case ty of
        Types.STRING => true
       |_  => (err pos ("String required: " ^ des);false)
  
  fun checkUnit (ty, pos, des) =
    case ty of
        Types.UNIT => true
	| _ => (err pos ("Unit required: " ^ des);false)

  fun checkRecord (ty, pos, des) =
    case ty of
        Types.RECORD(slist,unique) => true
	| _ => (err pos ("Record required: " ^ des);false)

  fun checkArray (ty, pos, des) =
    case ty of
        Types.ARRAY(ty, unique) => true
	| _ => (err pos ("Array required: " ^ des);false)

  fun compareTypePair (tyspec, tyactual, pos) = 
    let val tyactual' = actual_ty tyactual
        val tyspec' = actual_ty tyspec
    in
      if tyactual' = Types.NIL then case tyspec' of
                                        Types.RECORD(slist,unique) => true
                                      | _ => false
      else (tyspec' = tyactual')
    end

  fun compareTypeList (tyspec, tyactual, pos) = 
    ListPair.allEq(fn (ty1, ty2) => compareTypePair(ty1, ty2, pos)) (tyspec, tyactual)
      
  fun lookType (tenv, id, pos) = 
    case Symbol.look(tenv, id) of
       SOME ty => actual_ty ty
     | NONE =>  (err pos ("Undefined Type: " ^ Symbol.name id);
                 Types.UNIT)

  fun indexOf (a, lst) =
    let fun indexOfAcc(a, [], acc) = ~1
	  | indexOfAcc(a, h::l, acc) =
              if (a = h) then acc
              else indexOfAcc(a, l, acc+1)
    in
       indexOfAcc(a, lst, 0)
    end
 
  (*fun printType (Types.INT, pos) = err pos "Integer"
    | printType (Types.STRING, pos) = err pos "String"
    | printType (Types.ARRAY(ty, unique), pos) = err pos "Array"
    | printType (Types.RECORD(slist, unique), pos) = err pos "Record"
    | printType (Types.UNIT, pos) = err pos "Unit"
    | printType (Types.NAME(n,nref),pos) = err pos "Name"
    | printType (Types.NIL, pos)= err pos "NIL" *)

  fun checkname [] = false
    | checkname (name::ns) = (List.exists(fn n => n = name) ns) orelse (checkname ns)
		     
  fun transTy (tenv, ty) =
    let fun checkDups [] = false 
          | checkDups (a::l) = (List.exists (fn {name, escape, typ, pos} => if name = (#name a) then  (err pos ("Duplicate field names." ^ Symbol.name name);true)
                                                                            else false) l;
                                checkDups(l))  
        fun recordtys(fields) = map (fn{name, escape, typ, pos} => case SOME(lookType(tenv, typ, pos)) of
                 SOME t => (name, t)
               | NONE => (name,Types.UNIT)) fields   
    in
      case ty of
        A.NameTy(n, pos) => lookType(tenv, n, pos)
       | A.RecordTy fields => (checkDups fields; Types.RECORD(recordtys(fields), ref()))
       | A.ArrayTy(n, pos) => Types.ARRAY(lookType(tenv, n, pos), ref())
    end

    fun transExp (venv, tenv, level, loop_done_label) =
	let fun trexp (A.NilExp) = {exp = Translate.nilExp(), ty = Types.NIL}
	  | trexp (A.VarExp var) = transVar(venv, tenv, level, loop_done_label) var
	  | trexp (A.IntExp i) = {exp = Translate.intExp(i), ty = Types.INT} 
	  | trexp (A.StringExp (str, pos))  = {exp = Translate.stringExp(str), ty = Types.STRING}
	  | trexp (A.CallExp{func, args, pos}) = 
              let val trargs = map trexp args
                  val expargs = map #exp trargs
                  val tyargs = map #ty trargs
              in
                case Symbol.look(venv, func) of
                  SOME(Env.FunEntry{level = funcLevel, label = funcLabel, formals=tyformals, result=tyresult}) => if List.length(tyformals) <> List.length(tyargs) then (err pos ("Number of function parameters is wrong: "^Symbol.name func); {exp = Translate.nilExp(), ty=Types.UNIT})
                                                                                                                else if compareTypeList(tyformals, tyargs, pos) then {exp = Translate.callExp(funcLevel, funcLabel, level, expargs, tyresult = Types.UNIT), ty = tyresult}
                                                                                                                     else (err pos ("Function parameter type mismatch: " ^ Symbol.name func);{exp = Translate.nilExp(), ty = Types.UNIT})
                  | _  => (err pos ("Undefined function: " ^ Symbol.name func);
                      {exp = Translate.nilExp(), ty = Types.UNIT}) 
              end
	  | trexp (A.OpExp{left, oper, right, pos})  =
              let val {exp = expleft, ty = tyleft} = trexp left
                  val {exp = expright, ty = tyright} = trexp right
                  val operType =  if (oper = A.PlusOp orelse oper = A.MinusOp orelse oper = A.TimesOp orelse oper = A.DivideOp) then "math"
                                  else if (oper = A.LtOp orelse oper = A.LeOp orelse oper = A.GtOp orelse oper = A.GeOp) then "inequality"
                                       else if (oper = A.EqOp orelse oper = A.NeqOp) then "equality"
                                            else "error"
              in        
                  case operType of
                    "math" => if (checkInt(tyleft, pos, "Calculation") andalso checkInt(tyright, pos, "Calculation")) then {exp = Translate.binop(oper, expleft, expright), ty = Types.INT}
                              else {exp = (Translate.nilExp()), ty = Types.UNIT}
                   | "inequality" => (case tyleft of
                                      Types.INT => if checkInt(tyright, pos, "Comparison") then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                   else {exp = Translate.nilExp(), ty = Types.UNIT}
		                    | Types.STRING => if checkString(tyright, pos, "Comparison") then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                      else {exp = Translate.nilExp(), ty = Types.UNIT}  
                                    | _  => (err pos "Can't compare inequality";{exp = Translate.nilExp(), ty = Types.UNIT}))
	           | "equality" => (case tyleft of
                                     Types.INT => if checkInt(tyright, pos, "Comparison") then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                  else {exp = Translate.nilExp(), ty = Types.UNIT}
		                    | Types.STRING => if checkString(tyright, pos, "Comparison") then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                      else {exp = Translate.nilExp(), ty = Types.UNIT}  
                                    | Types.RECORD(slist, unique) => if ((tyright = Types.NIL) orelse (checkRecord(tyright, pos, "Comparison"))) then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                                     else {exp = Translate.nilExp(), ty = Types.UNIT}
                                    | Types.ARRAY(ty, unique) => if checkArray(tyright, pos, "Comparison") then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                                 else {exp = Translate.nilExp(), ty = Types.UNIT}
                                    | Types.NIL => if checkRecord(tyright, pos, "Comparison") then {exp = Translate.relop(oper, expleft, expright),  ty = Types.INT}
                                                   else {exp = Translate.nilExp(), ty = Types.UNIT}
			            | _  => (err pos "Can't compare equality";{exp = Translate.nilExp(), ty = Types.UNIT}))
	            | _  =>    (err pos "Illegal comparison operator";{exp = Translate.nilExp(), ty = Types.UNIT})
              end
	  | trexp (A.RecordExp{fields, typ, pos})  = 
              let val tyrecord = lookType(tenv, typ, pos)
                  val fieldnames = map #1 fields
                  val trfields = map trexp (map #2 fields)
                  val tyfields = map #ty trfields
                  val expfields = map #exp trfields
              in
                case tyrecord of
                  Types.RECORD(slist, unique) => let val decfieldnames = map #1 slist
                                                     val decfieldtypes = map actual_ty (map #2 slist)
                                                 in
                                                   if decfieldnames = fieldnames then if compareTypeList(decfieldtypes, tyfields, pos) then {exp = Translate.recordExp(expfields), ty = tyrecord}                                    
                                                                                      else (err pos ("Record field types mismatch: " ^ Symbol.name typ);
                                                                                            {exp = Translate.nilExp(), ty = Types.UNIT}) 
                                                   else (err pos ("Record field names mismatch: " ^ Symbol.name typ);
                                                         {exp = Translate.nilExp(), ty = Types.UNIT})
                                                 end
	       | _ =>  (err pos ("Undefined record: " ^ Symbol.name typ);
                        {exp = Translate.nilExp(),  ty = Types.UNIT})
            end
	| trexp (A.SeqExp expposList)  =
            let val expList = map #1 expposList
                val trexpList = map trexp expList
                val expexpList = map #exp trexpList
                val tyexpList = map #ty trexpList
            in  
               case List.length(trexpList) of
                  0 => {exp = Translate.nilExp(), ty = Types.UNIT}
		| _  => {exp = Translate.seqExp(expexpList),  ty = List.last(tyexpList)}
            end
	| trexp (A.AssignExp{var, exp, pos})  =
            let val {exp = expspec, ty = tyspec} = transVar(venv, tenv, level, loop_done_label) var
                val {exp = expactual, ty = tyactual} = trexp exp
            in 
              if compareTypePair(tyspec, tyactual, pos) then {exp = Translate.assignExp(expspec, expactual),  ty = Types.UNIT}
              else (err pos "Assignment type mismatch";
                   {exp = Translate.nilExp(), ty = Types.UNIT})
            end
	| trexp (A.IfExp{test, then', else', pos}) =
            let val {exp = exptest, ty = tytest} = trexp test
                val {exp = expthen', ty = tythen'} = trexp then'
            in
              case else' of 
                NONE => if (checkInt(tytest, pos, "If condition") andalso checkUnit(tythen', pos, "Then condition")) then {exp = Translate.ifthenExp(exptest, expthen'), ty = Types.UNIT}
                        else {exp = Translate.nilExp(), ty = Types.UNIT}
	      | SOME(else'_exp) => 
                  let val {exp = expelse', ty = tyelse'} = trexp else'_exp
                  in
                    if checkInt(tytest, pos, "If condition") then 
                       if compareTypePair(tythen',tyelse', pos) then {exp = Translate.ifthenelseExp(exptest, expthen', expelse'), ty = tythen'}
                       else (err pos "Then-else type mismatch";{exp = Translate.nilExp(), ty = Types.UNIT})
                    else {exp = Translate.nilExp(), ty = Types.UNIT}
                  end
            end
	| trexp (A.WhileExp{test, body, pos}) =
            let val {exp = exptest, ty = tytest} = trexp test
                val done_label = SOME(Translate.newDoneLabel())
                val {exp = expbody, ty = tybody} = transExp(venv, tenv, level, done_label) body         
            in
               if (checkInt(tytest, pos, "While loop condition") andalso checkUnit(tybody, pos, "While loop body")) then {exp = Translate.whileExp(exptest, expbody, done_label), ty = Types.UNIT}
                else {exp = Translate.nilExp(), ty = Types.UNIT}
            end
	| trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
            let val {exp = explo, ty = tylo} = trexp lo
                val {exp = exphi, ty = tyhi} = trexp hi
		val access = Translate.allocLocal level (!escape)
                val new_venv = Symbol.enter(venv, var, Env.VarEntry{access=access,ty=Types.INT})    
                val done_label = SOME(Translate.newDoneLabel())
                val {exp = expbody, ty = tybody} = transExp(new_venv, tenv, level, done_label) body
            in
              if (checkInt(tylo, pos, "For loop low bound") andalso checkInt(tyhi, pos, "For loop high bound") andalso checkUnit(tybody, pos, "For loop body")) then {exp = Translate.forExp(Translate.simpleVar(access, level), exphi, explo, expbody, done_label), ty = Types.UNIT}
              else {exp = Translate.nilExp(), ty = Types.UNIT}
            end
	| trexp (A.BreakExp pos) = 
            if loop_done_label = NONE then (err pos "Break not inside a loop.";
                                           {exp = Translate.nilExp(), ty = Types.UNIT})
            else {exp = Translate.breakExp(loop_done_label), ty = Types.UNIT}
	| trexp (A.LetExp{decs, body, pos}) =
          let
	      fun transOne(oneDec, {venv=venv, tenv=tenv, explist=initlist})=
		  transDec(venv,tenv,oneDec, loop_done_label, level, initlist)
	      val {venv = venv', tenv = tenv', explist = explist'} = foldl transOne {venv=venv, tenv=tenv, explist=[]} decs
	      val {exp = bodyexp, ty= bodyty} = transExp(venv',tenv', level, loop_done_label) body
          in   
	       {exp = Translate.adhere(explist',bodyexp), ty = bodyty}
          end
	| trexp (A.ArrayExp{typ, size, init, pos})  =
            let val {exp = expsize, ty = tysize} = trexp size
                val {exp = expinit, ty = tyinit} = trexp init
                val tytyp = lookType(tenv, typ, pos)
            in
              if checkInt(tysize, pos, "Array size") then
                (case tytyp of 
                  Types.ARRAY(ty, unique) => if compareTypePair(ty, tyinit, pos) then {exp = Translate.arrayExp(expsize, expinit), ty = tytyp}
                                             else (err pos "Array-init type mismatch";{exp = Translate.nilExp(), ty =tytyp})
                 |_  => (err pos ("Undefined array: "^Symbol.name typ);
                        {exp = Translate.nilExp(), ty = Types.UNIT}))
              else {exp = Translate.nilExp(), ty = Types.UNIT}
            end
	in
	    trexp
	end
 
    and transVar (venv, tenv, level, loop_done_label) =
      let fun trvar (A.SimpleVar(id, pos)) =
                (case Symbol.look(venv, id) of
                   SOME (Env.VarEntry{access = access, ty = ty}) => {exp = Translate.simpleVar(access, level), ty = actual_ty ty}
                  | _  => (err pos ("Undefined variable: " ^ Symbol.name id);
                           {exp = Translate.nilExp(), ty = Types.UNIT}))
          | trvar (A.FieldVar(var, id, pos))  =
              let val {exp = expvar, ty = tyvar} = trvar var
              in
                case tyvar of
                  Types.RECORD(slist, unique) => let val fieldnames = map #1 slist
                                                     val fieldtypes = map #2 slist
                                                     val index = indexOf(id, fieldnames)
                                                 in
                                                    if index = ~1 then (err pos ("No such field in record");
                                                                        {exp = Translate.nilExp(), ty = Types.UNIT})
                                                    else {exp = Translate.fieldVar(expvar, index), ty = List.nth(fieldtypes, index)}
                                                 end
                  | _  => (err pos ("Undefined record");
                           {exp = Translate.nilExp(), ty = Types.UNIT})
               end
      | trvar (A.SubscriptVar(var, exp, pos))  = 
          let val {exp = expvar, ty = tyvar} = trvar var
              val {exp = expexp, ty = tyexp} = transExp(venv, tenv, level, loop_done_label) exp
          in  
            (checkInt(tyexp, pos, "Array subscript");
             case tyvar of
               Types.ARRAY(ty, unique) => {exp = Translate.subscriptVar(expvar, expexp), ty = ty}
             | _ => (err pos ("Undefined array");
                     {exp = (Translate.nilExp()), ty = Types.UNIT}))
          end
      in
         trvar
      end
	  
  and transDec (venv,tenv,A.VarDec{name,
                                  escape,
                                  typ = NONE,
                                  init,
                                  pos}, endLabel, level, explist)=
      let val tyinit = #ty(transExp(venv,tenv, level, endLabel) init)
	  val exp = #exp(transExp(venv,tenv,level, endLabel) init)
	  val access = Translate.allocLocal level (!escape)
	  val explist' = Translate.assignExp(Translate.simpleVar(access, level), exp)::explist
      in 
          if tyinit <> Types.NIL then (print "in NONE"; {tenv = tenv, venv = Symbol.enter(venv,name,Env.VarEntry{access=access, ty = tyinit}), explist = explist'})
          else (err pos ("Non-record variable can't be initializing nil expression: " ^ Symbol.name name);{tenv = tenv, venv = venv, explist = explist})
      end
    | transDec (venv,tenv,A.VarDec{name,typ=SOME(s,pos),init, pos=pos1, escape=escape},endLable,level,explist) =
      let val tyinit = #ty(transExp (venv, tenv,level, endLable) init)
          val tys = lookType(tenv, s, pos)
          val exp = #exp(transExp(venv,tenv,level,endLable) init)
	  val access = Translate.allocLocal level (!escape)
	  val explist' = Translate.assignExp(Translate.simpleVar(access, level), exp)::explist
      in
          if compareTypePair(tys, tyinit,  pos) then ({tenv = tenv, venv = Symbol.enter(venv, name, Env.VarEntry{access=access, ty = tys}), explist = explist'})
          else (err pos1 ("Variable init type mismatch: " ^ Symbol.name name);({tenv = tenv, venv = Symbol.enter(venv, name, Env.VarEntry{access=access, ty = tys}), explist = explist'}))
      end
    | transDec (venv, tenv, A.TypeDec vardecs, endLable,level,explist) =
      let
	  val names = map #name vardecs
	  val repeat = checkname names
	  val positions = map #pos vardecs
	  val types = map #ty vardecs
	  fun addnamet (name,env) = Symbol.enter(env,name,Types.NAME(name,ref(NONE)))
	  val tenv' = foldr addnamet tenv names
			    
	  fun update (((name,typ),pos),tenv')=
	      let val newtype = transTy (tenv',typ)
	      in
		  (case Symbol.look(tenv', name) of 
		       SOME(Types.NAME(s, tref)) => (tref := SOME(newtype); tenv')
		     | _ => (err pos "Error defining types"; tenv'))
	      end
	  fun finalize (((name,typ),pos),tenv'': Types.ty Symbol.table) =
	      let val counter = List.length(names)
		  fun determine (name:Symbol.symbol, count:int) =
		      let val counting : int ref= ref(count)			     
		      in
			  counting := count - 1;
			  if !counting < 0 then (err pos "Error with mutually recursive types";tenv'')
			  else (case Symbol.look(tenv'',name) of
				    NONE => (err pos "Error with NONE type";tenv'')
				   |SOME(Types.NAME(s,nref))=> (case !nref of
								    NONE => (err pos "Error with NONE type";tenv'')
								   |SOME(rtyp: Types.ty)=> (case rtyp of
												Types.NAME(n,aref) => (determine(n,!counting))
											       |_=>(Symbol.enter(tenv'',name,rtyp))))
				   | _ => tenv'')
		      end 
	      in
		  determine (name,counter)
	      end
	  val tenv'' = foldl update tenv' (ListPair.zip(ListPair.zip(names,types),positions))
      in
	  (if repeat
	   then (err (#pos (List.hd(vardecs))) "Duplicate type definition"; {tenv = tenv, venv=venv, explist=explist})
	   else
	       {tenv = foldr finalize tenv'' (ListPair.zip(ListPair.zip(names,types),positions)), venv = venv, explist=explist})
      end
    | transDec (venv, tenv, (Absyn.FunctionDec funcs),endlabel,level,explist) =
      let fun firstpass ({name=name, params=params, body=body, pos=pos, result=result},tempvenv) =
	      let val result_ty = case result of
                                      SOME(rt, position) => lookType(tenv, rt, pos)
				     |NONE => Types.UNIT 
		  fun transparam (field:A.field) =
		      let val name = #name field
			  val typ = #typ field
			  val pos= #pos field
		      in
			  {name = name, ty = lookType(tenv, typ, pos)}
		      end
		  val params' = map transparam params

		  fun getEscape(field:A.field)= !(#escape field)
		  val newLabel = Temp.newlabel()
		  val newLevel = Translate.newLevel {parent = level, name = newLabel, formals = (map getEscape params)}
	      in
		  Symbol.enter(tempvenv, name, Env.FunEntry{formals = map #ty params', result = result_ty, level = newLevel, label = newLabel})
	      end  
	  val venv' = foldr firstpass venv funcs
	  val funcnames = map #name funcs
	  val repeat = checkname funcnames
	  fun secondpass (venv,[]) = ()
	    | secondpass (venv,(fundec:Absyn.fundec)::func) =
	      let
		  val name = #name fundec
		  val params = #params fundec
		  val body = #body fundec
		  val pos = #pos fundec
		  val result = #result fundec
		  val tyret = case result of 
				  SOME(rt,p) => lookType(tenv,rt, p)
				| NONE => Types.UNIT
		  val funcentry = case Symbol.look(venv,name) of
				      SOME(Env.FunEntry funcentry) => funcentry
				     |_ => ErrorMsg.impossible "function not found in the environment" 
		  fun enterparam ({name = name, escape = escape, typ=typ, pos=pos}, (venv, access::rest)) =
	              let
			  val ty = lookType(tenv, typ, pos)
		      in
			  (Symbol.enter(venv,name,Env.VarEntry{access = access, ty=ty}),rest)
		      end
		    | enterparam ({name = name, escape=escape, typ=typ, pos=pos}, (venv,[])) =
		      let
			  val ty = lookType(tenv,typ,pos)
		      in
			  (ErrorMsg.impossible "Didn't allocate param earlier";
			   (Symbol.enter (venv, name, Env.VarEntry {access = Translate.allocLocal (#level funcentry) (!escape), ty = ty}), []))
		      end							    
										    
		  val (venv'',_) = foldr enterparam (venv', List.drop(Translate.formals (#level funcentry),0)) params 
		  val {exp=bodyexp , ty=bodytype} = transExp(venv'',tenv, (#level funcentry), endlabel) body
		  val result = Translate.procEntryExit({level = (#level funcentry), body = bodyexp})
	      in
		  
		  case compareTypePair(bodytype, tyret, pos) of
		      true =>  secondpass(venv,func)
		    | false => (err pos ("Function return type mismatch: " ^ Symbol.name name); secondpass(venv,func))
	      end
      in
	  (if repeat
	   then (err (#pos (List.hd(funcs))) "Duplicate function definition"; {tenv = tenv, venv=venv, explist = explist})
	   else
	       (secondpass(venv',funcs);
		{venv=venv', tenv = tenv, explist = explist}))
      end
    | transDec(venv,tenv, A.Initial(), endLabel, level, explist) = {venv = venv, tenv=tenv, explist = explist}
				  
	fun transProg(absyn) = 
	    let val reset = Translate.reset()
                val mainLevel = Translate.mainLevel
	        val mainLabel = SOME(Temp.newlabel())
		val result = transExp (Env.base_venv, Env.base_tenv, Translate.mainLevel, mainLabel) absyn; 
	    in
                Translate.procEntryExit({level = mainLevel, body = #exp result});
		Translate.getResult()
	    end
end
