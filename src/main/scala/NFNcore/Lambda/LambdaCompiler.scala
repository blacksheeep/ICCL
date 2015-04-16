package NFNcore.Lambda

case class environment(e: Int, pos: Int)

class LambdaCompiler {

  def apply(expr: Expr): List[KrivineInstruction] = {
    val expr_mapped = map_variables(expr, Map())
    return compile(expr_mapped)
  }
  
  /**
   * creates opcodes for the NFN Krivine Machine
   * @param expr Take an ast, created with the LambdaParser as input
   */
  def compile(expr: Expr) : List[KrivineInstruction] = {
    expr match{
      case Var(varname,varnum) => List(ACCESS(varname,varnum))
      case Lambda(arg, body) => GRAB(arg.name, arg.num) :: compile(body)
      case Apply(fun, body) => PUSH(compile(body)) :: compile(fun)
      case Const(v) => List(NUMBER(v))
      case Str(str) => List(STRING(str))
      case Name(str) => List(NFNName(str))
      case Lst(list) => List(LISTINST(list.map(p => compile(p))))
      case Call(fname, num, params) => List(CALLINST(NFNName(fname.name), num, params.map(p => compile(p))))
      case Ifelse(condition, fulfilled, notfulfilled) => List(IFELSEINST(compile(condition), compile(fulfilled), compile(notfulfilled)))
      case Function(name, numOfParams, startVarNum, expr, prog) => List(FUNCTIONINST(NFNName(name.name), numOfParams, startVarNum, compile(expr), compile(prog)))
      case _ => ???
    }
  }  

  
  //unique number for a variable.
  var num: Int = 0
   /**
   * give each parameter a unique (local) variable number to handle them in the environment
   */
  def map_variables(expr: Expr, env: Map[String, Int], insideFunction: Boolean = false): Expr = {
    expr match{
      case Var(v,_) => {
        if(insideFunction && v.startsWith("_")){
          val v2 = v.substring(1)
          val n = v2.toInt
          if(n > 0){
            return Var(v, n)
          }
        }
        if(env.contains(v)) {
          Var(v, env(v))
        }else{
          num = num + 1 //TODO may error message in some cases here?
          Var(v, num)
        }
      }
      case Lambda(arg, body) => {
        num = num + 1
        val arg_new = Var(arg.name, num)
        val env_new = env + (arg.name -> num)
        Lambda(arg_new, map_variables(body, env_new, insideFunction))
      }
      case Apply(fun, body) => {
        Apply(map_variables(fun, env, insideFunction), map_variables(body, env, insideFunction))
      }
      //Extensions
      case Const(i) => {
        Const(i)
      }
      case Str(s) => {
        Str(s)
      }
      case Name(s) => {
        Name(s)
      }
      case Lst(list) => {
        Lst(list.mapConserve { e => map_variables(e, env, insideFunction) })
      }
      //BuildIn Functions
      case Call(name, num, params) => {
        val params_new = params.map(p => map_variables(p, env, insideFunction))
        Call(name, num, params_new)
      }
      case Lookup(name) => {
        Lookup(name)
      }
      case Ifelse(condition, fulfilled, notfulfilled) => {
        Ifelse(map_variables(condition, env, insideFunction), map_variables(fulfilled, env, insideFunction), map_variables(notfulfilled, env, insideFunction))
      }
      case Function(name, numOfParams, _ , expr, prog) => {
        Function(name, numOfParams, num, map_variables(expr, env, insideFunction = true), map_variables(prog, env, insideFunction))
      }
      case _ => ???
    }
  }

}