package NFNcore.Lambda

import Logging.DEBUGMSG
import Logging.Debuglevel

import scala.collection.mutable.{Map,
      SynchronizedMap, HashMap}

class KrivineBuildIn {
  
  var defines: Map[String, String] = Map()
  
  def apply(fname: String, params: List[List[KrivineInstruction]], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine) : List[KrivineInstruction] = {
   buildin(fname, params, env, varoffset, krivine)
  }
  
  def buildin(fname: String, params: List[List[KrivineInstruction]], env: Map[Int, List[KrivineInstruction]], varoffset: Int, krivine: Krivine) : List[KrivineInstruction] = {
     if(fname == "/local/add"){
         return add(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == "/local/eq"){
         return eq(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == "/local/gt"){
         return gt(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == "/local/head"){
         return head(params.head, env, varoffset, krivine)
     }
     else if(fname == "/local/tail"){
         return tail(params.head, env, varoffset, krivine)
     }
     else if(fname == "/local/length"){
         return length(params.head, env, varoffset, krivine)
     }
     else if(fname == "/local/prepend"){
         return prepend(params.head, params.tail.head, env, varoffset, krivine)
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