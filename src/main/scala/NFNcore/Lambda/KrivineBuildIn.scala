package NFNcore.Lambda

import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.NFNNode

import scala.collection.mutable.Map

class KrivineBuildIn (nfnNode: NFNNode){
  
  var defines: Map[String, String] = Map()
  
  def apply(fname: NFNName, params: List[List[KrivineInstruction]], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine) : List[KrivineInstruction] = {
   buildin(fname, params, env, varoffset, krivine)
  }
  
  def buildin(fname: NFNName, params: List[List[KrivineInstruction]], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine) : List[KrivineInstruction] = {
     if(fname == NFNName(List("local", "add"))){
         return add(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(List("local", "eq"))){
         return eq(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(List("local", "gt"))){
         return gt(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(List("local", "head"))){
         return head(params.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(List("local", "tail"))){
         return tail(params.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(List("local", "length"))){
         return length(params.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(List("local", "prepend"))){
         return prepend(params.head, params.tail.head, env, varoffset, krivine)
     }
    else if(fname == NFNName(List("local", "checkCS"))){
       val res1 = krivine.execute(params.head, List(), env, varoffset)
       res1.head match{
         case n: NFNName => if (nfnNode.checkCS(n)) return List(NUMBER(1)) else return List(NUMBER(0))
         case _ => {
           println("Wrong argument")
         }
       }
       return null
     }
    else if(fname == NFNName(List("local", "grabCS"))){
       val res1 = krivine.execute(params.head, List(), env, varoffset)
       res1.head match{
         case n: NFNName => return List(nfnNode.grabCS(n).name) //TODO handle content not only name
         case _ => {
           println("Wrong argument")
         }
       }
       return null
     }
    else if(fname == NFNName(List("local", "pushCS"))){
      ???
     }
    else if(fname == NFNName(List("local", "checkPIT"))){
       val res1 = krivine.execute(params.head, List(), env, varoffset)
       res1.head match{
         case n: NFNName => if(nfnNode.checkPIT(n)) return List(NUMBER(1)) else return List(NUMBER(0))
         case _ => {
           println("Wrong argument")
         }
       }
       return null
     }
    else if(fname == NFNName(List("local", "grabPIT"))){
       val res1 = krivine.execute(params.head, List(), env, varoffset)
       res1.head match{
         case n: NFNName => return List(NUMBER(nfnNode.grabPIT(n)))
         case _ => {
           println("Wrong argument")
         }
       }
       return null
     }
    else if(fname == NFNName(List("local", "pushPIT"))){
      ???
     }
    else if(fname == NFNName(List("local", "checkFIB"))){
       val res1 = krivine.execute(params.head, List(), env, varoffset)
       res1.head match{
         case n: NFNName => if(nfnNode.checkFIB(n)) return List(NUMBER(1)) else return List(NUMBER(0))
         case _ => {
           println("Wrong argument")
         }
       }
     }
     else if(fname == NFNName(List("local", "grabFIB"))){
       val res1 = krivine.execute(params.head, List(), env, varoffset)
       res1.head match{
         case n: NFNName => return List(NUMBER(nfnNode.grabFIB(n)))
         case _ => {
           println("Wrong argument")
         }
       }
       return null
     }
     ???
  }
  
  def add(param1: List[KrivineInstruction], param2: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
    
    val res1 = krivine.execute(param1, List(), env, varoffset)
    val res2 = krivine.execute(param2, List(), env, varoffset)

    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: add " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
        
    (res1.head, res2.head) match {
      case (v1: Addable, v2: Addable) => {
        return List(v1 + v2)
      }
      case _ => ???
    }
  }
    
  def eq(param1: List[KrivineInstruction], param2: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
    val res1 = krivine.execute(param1, List(), env, varoffset)
    val res2 = krivine.execute(param2, List(), env, varoffset)
    
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: eq " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
  
    (res1.head, res2.head) match {
      case (v1: Comparable, v2: Comparable) => {
        return List(v1 == v2)
      }
      case _ => ???
    }
  }
  
  def gt(param1: List[KrivineInstruction], param2: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
    val res1 = krivine.execute(param1, List(), env, varoffset)
    val res2 = krivine.execute(param2, List(), env, varoffset)
    
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: gt " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
   
    (res1.head, res2.head) match {
      case (v1: Comparable, v2: Comparable) => {
        return List(v1 > v2)
      }
      case _ => ???
    }
  }

  def head(param1: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
     val res = krivine.execute(param1, List(), env, varoffset)
     
     DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: head " + res +" (" + param1 + ") " + varoffset)
   
     res.head match {
       case LISTINST(l) => return l.head
     }
  }
  
  def tail(param1: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
     val res = krivine.execute(param1, List(), env, varoffset)
     
     DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: tail " + res +" (" + param1 + ") " + varoffset)
     
     res.head match {
       case LISTINST(l) => return List(LISTINST(l.tail))
     }
  }
  
  def length(param1: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
     val res = krivine.execute(param1, List(), env, varoffset)
    
     DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: length " + res +" (" + param1 + ") " + varoffset)
     
     res.head match {
       case LISTINST(l) => return List(NUMBER(l.length))
     }
  }
  
  def prepend(param1: List[KrivineInstruction], param2: List[KrivineInstruction], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine): List[KrivineInstruction] = {
    val res1 = krivine.execute(param1, List(), env, varoffset)
    val res2 = krivine.execute(param2, List(), env, varoffset)
    
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: prepend " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
    
    (res1.head, res2.head) match {
      case (v1: KrivineInstruction, LISTINST(l)) => {
        return  List(LISTINST( List(v1) :: l))
      }
      case _ => ???
    }
  }
  
}