package NFNcore.Lambda
import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.NFNNode

import scala.collection.mutable.Map

//Function inside the Krivine machine
case class Func(expr: List[KrivineInstruction], numOfParams: Int) 


class Krivine(nfnNode: NFNNode){
  
  //Function environment
  var funcEnv: Map[NFNName, Func] = Map()
  
  
  def apply(instructions: List[KrivineInstruction]): List[KrivineInstruction] = execute(instructions, List(), Map(), 0)
  
  def execute(instructions: List[KrivineInstruction], stack: List[List[KrivineInstruction]], 
      env: Map[Int, List[KrivineInstruction]], varoffset: Int) : List[KrivineInstruction] = {
    
    if(instructions.isEmpty) {
      return List(RESULT(""))
    }    
    DEBUGMSG(Debuglevel.DEBUG, "Executing Krivine Instruction: " + instructions.head.toString() + " varoffset: " + varoffset)
    instructions.head match {
      case ACCESS(varname, varnum) => {
        val offset = 0 //if(varname.startsWith("_")) varoffset else 0 //use varoffset only for function parameters //TODO problem with recursion, since old vars start also with _1
        val inst = env.getOrElse(varnum + offset, Nil)
        if(inst != Nil){
          return execute(inst ++ instructions.tail, stack, env, varoffset)
        }
        else{
          return execute(instructions.tail ++ List(VARIABLE(varname, varnum)), stack, env, varoffset)
        }
      }
      case GRAB(varname, varnum) => {
        return execute(instructions.tail, stack.tail, env += varnum -> stack.head, varoffset)
      }
      case PUSH(elm) => {
        return execute(instructions.tail, elm :: stack, env, varoffset)
      }
      case VARIABLE(name, varnum) => {
        if(!stack.isEmpty){ //TODO this is also required for other datatypes, do it generic? appliable?
          return List(VARIABLE(name, varnum)) ++ execute(stack.head, stack.tail, env, varoffset)
        }
        else {
          return List(VARIABLE(name, varnum))
        }
      }      
      case NUMBER(v) => {
        return List(NUMBER(v))
      }
      case STRING(s) => {
        return List(STRING(s))
      }
      case NFNName(comps) => {
        return List(NFNName(comps))
      }
      case LISTINST(l) => {
        return List(LISTINST(l))
      }
      case CALLINST(fname, num, params) => {
        if(funcEnv.contains(fname)){
          return lambdafunction(fname, params, env, this)
        }
        else{
          var buildin = new KrivineBuildIn(nfnNode)
          return buildin(fname, params, env, varoffset, this)
        }
      }
      case IFELSEINST(condition, fulfilled, notfulfilled) => {
        val res = execute(condition, stack, env, varoffset)
        var s = 0;
        res.head match{
          case NUMBER(s) => {
            if(s == 0){
              return execute(notfulfilled, stack, env, varoffset) //false
            }
            else{
              return execute(fulfilled, stack, env, varoffset) //true
            }
          }
        }

      }
      case FUNCTIONINST(name, numOfParams, startVarNum, expr, prog) => {
        funcEnv += name -> Func(expr, numOfParams)
        return execute(prog, stack, env, varoffset)
      }
      case _ => {
        return List(RESULT("Error"))
      }
    }
  }
  
  def lambdafunction(fname: NFNName, params: List[List[KrivineInstruction]], env: Map[Int, List[KrivineInstruction]], krivine: Krivine): List[KrivineInstruction] = {
    val func = funcEnv(fname)
    val numOfParams = func.numOfParams
    
    var fenv: Map[Int, List[KrivineInstruction]] = Map()
    fenv = fenv ++ env
    var i = 0
    val offset = if(fenv.isEmpty) 0 else fenv.keys.max
    for(i <- 0  to numOfParams - 1){ //TODO not functional, a bit "hacky"
      fenv +=  i + offset + 1 -> params(i)  //TODO how to do secure recursion with this numbers?
    }
    //link and prepare function code!
    val code = linkFunctionCode(func.expr, offset)
    return krivine.execute(code, List(), fenv, offset)
  }
  
  
  def linkFunctionCode(function: List[KrivineInstruction], offset: Int): List[KrivineInstruction] = {
    return function.map { 
      inst => inst match{
        case ACCESS(varname, varnum) => ACCESS(varname, (varnum + offset))
        //case GRAB(varname, varnum) => GRAB(varname, varnum+offset) //offset here required (may necessary to add this line to use abstraction in functions)?
        case PUSH(k) => PUSH(linkFunctionCode(k, offset))
        case CALLINST(fname, num, params) => CALLINST(fname, num, params.map { p => linkFunctionCode(p, offset) })
        case IFELSEINST(cond, fulfilled, notfulfilled) => IFELSEINST(linkFunctionCode(cond, offset), linkFunctionCode(fulfilled, offset), linkFunctionCode(notfulfilled, offset))
        //case FUNCTIONINST(fname, numOfParam, startParam, expr, prog) =>
        case _ => inst
      }
    }
  }
}

