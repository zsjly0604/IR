signature ENV =
sig
  type access
  datatype enventry =  VarEntry of {access: Translate.access, ty: Types.ty}
                     | FunEntry of {level: Translate.level,
                                    label: Temp.label,
                                    formals: Types.ty list, result : Types.ty}
  val base_tenv : Types.ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env :> ENV = 
struct
  type access = unit ref
  datatype enventry =  VarEntry of {access: Translate.access, ty: Types.ty}
                     | FunEntry of {level: Translate.level,
                                    label: Temp.label,
                                    formals: Types.ty list, result : Types.ty}
  fun enter ((symbol, entry), env) = Symbol.enter(env, symbol, entry)
  
  val base_tenv = foldl enter Symbol.empty [
                    (Symbol.symbol("int"), Types.INT),
                    (Symbol.symbol("string"), Types.STRING)
                   ]
  val base_venv = foldl enter Symbol.empty [
                    (Symbol.symbol("print"), FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "print", formals=[Types.STRING], result=Types.UNIT}),
                    (Symbol.symbol("flush"), FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "flush", formals=[], result=Types.UNIT}),
                    (Symbol.symbol("getchar"), FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "getchar",formals=[], result=Types.STRING}),
                    (Symbol.symbol("ord"),        FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "ord",formals=[Types.STRING], result=Types.INT}),
                    (Symbol.symbol("chr"),        FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "chr",formals=[Types.INT], result=Types.STRING}),
                    (Symbol.symbol("size"),       FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "size",formals=[Types.STRING], result=Types.INT}),
                    (Symbol.symbol("substring"),  FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "substring",formals=[Types.STRING,Types.INT,Types.INT], result=Types.STRING}),
                    (Symbol.symbol("concat"),     FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "concat",formals=[Types.STRING,Types.STRING], result=Types.STRING}),
                    (Symbol.symbol("not"),        FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "not",formals=[Types.INT], result=Types.INT}),
                    (Symbol.symbol("exit"),       FunEntry {level = Translate.mainLevel, label = Temp.namedlabel "exit",formals=[Types.INT], result=Types.UNIT})
                   ]
end
