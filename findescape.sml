structure FindEscape : sig val findEscape: Absyn.exp -> unit
                       end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table
  
  val baseDepth : depth = 0
  val baseEscEnv : escEnv = Symbol.empty

  structure A = Absyn 
  val err = ErrorMsg.error

  fun traverseVar(env:escEnv, d:depth, s:Absyn.var) : unit = 
    case s of
      A.SimpleVar(id, pos) => (case Symbol.look(env, id) of
                               SOME(d', esc) => if d' > d then esc := true
                                                else ()
			     | NONE => ())
     | A.FieldVar(var, id, pos) => traverseVar(env, d, var)
     | A.SubscriptVar(var, exp, pos) => (traverseVar(env, d, var);
                                         traverseExp(env, d, exp)) 
 
  and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit = 
    case s of
      A.NilExp => ()
    | A.IntExp n => ()
    | A.StringExp str => ()
    | A.VarExp var => traverseVar(env, d, var)
    | A.CallExp{func, args, pos} => foldl (fn (exp, _) => traverseExp(env, d, exp)) () args
    | A.OpExp{left, oper, right, pos} => (traverseExp(env, d, left);
                                            traverseExp(env, d, right))
    | A.RecordExp{fields, typ, pos} => foldl (fn ((_, exp, _), _) => traverseExp(env, d, exp)) () fields  
    | A.SeqExp expposList => foldl (fn((exp, _), _) => traverseExp(env, d, exp)) () expposList
    | A.AssignExp{var, exp, pos}=> (traverseVar(env, d, var);
                                      traverseExp(env, d, exp))  
    | A.IfExp{test, then', else', pos} => (traverseExp(env, d, test);
                                             traverseExp(env, d, then');
                                             (case else' of
                                               SOME(else'') => traverseExp(env, d, else'')
					     | NONE => ()))
    | A.WhileExp{test, body, pos} => (traverseExp(env, d, test);
                                      traverseExp(env, d, body))
    | A.ForExp{var, escape, lo, hi, body, pos} => let val new_env = Symbol.enter(env, var, (d, escape)) 
                                                  in
                                                    escape := false;
                                                    traverseExp(env, d, lo);
                                                    traverseExp(env, d, hi);
                                                    traverseExp(new_env, d, body)
                                                  end
    | A.BreakExp pos => ()
    | A.LetExp{decs, body, pos} => let val new_env = traverseDecs(env, d, decs)
                                   in
                                     traverseExp(new_env, d, body)
                                   end  
    |  A.ArrayExp{typ, size, init, pos}=> (traverseExp(env, d, size);
                                           traverseExp(env, d, init))
 
                                                       
    
    
  and traverseDecs(env, d, s:Absyn.dec list) : escEnv = 
    let fun parse_dec (dec, env) =
      case dec of
        A.FunctionDec fundecs => foldl (fn ({name, params, body, pos, result}, env) => let val new_env = (foldl (fn ({name, escape, ...}, env) => (escape := false;
                 Symbol.enter(env, name, (d + 1, escape)))) env params)
            in
               traverseExp(new_env, d + 1, body);
               env
            end) env fundecs
      | A.VarDec{name, escape, init, ...} => (escape := false;
                                             traverseExp(env, d, init);
                                             Symbol.enter(env, name, (d, escape)))
      | A.TypeDec(_) => env
    in
       foldl parse_dec env s 
    end
 
  fun findEscape(prog: Absyn.exp) : unit = 
    traverseExp(baseEscEnv, baseDepth, prog)
end
