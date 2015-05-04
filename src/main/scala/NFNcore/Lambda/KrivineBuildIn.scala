package NFNcore.Lambda

import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.NFNNode
import NFNcore.Packets.{NFNContent, PacketCommand, NFNInterest}

import scala.collection.mutable.Map

class KrivineBuildIn (nfnNode: NFNNode){
  
  var defines: Map[String, String] = Map()
  
  def apply(fname: NFNName, params: Vector[Vector[KrivineInstruction]], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine) : Vector[KrivineInstruction] = {
   buildin(fname, params, env, varoffset, krivine)
  }
  
  def buildin(fname: NFNName, params: Vector[Vector[KrivineInstruction]], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine) : Vector[KrivineInstruction] = {
     if(fname == NFNName(Vector("local", "add"))){
         return add(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(Vector("local", "eq"))){
         return eq(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(Vector("local", "gt"))){
         return gt(params.head, params.tail.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(Vector("local", "head"))){
         return head(params.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(Vector("local", "tail"))){
         return tail(params.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(Vector("local", "length"))){
         return length(params.head, env, varoffset, krivine)
     }
     else if(fname == NFNName(Vector("local", "prepend"))){
         return prepend(params.head, params.tail.head, env, varoffset, krivine)
     }
    else if(fname == NFNName(Vector("local", "checkCS"))){
       val res1 = krivine.execute(params.head, Vector(), env, varoffset)
       res1.head match{
         case n: NFNName => if (nfnNode.checkCS(n)) return Vector(NUMBER(1)) else return Vector(NUMBER(0))
         case _ => {
           println("Wrong argument in checkCS")
         }
       }
       return null
     }
    else if(fname == NFNName(Vector("local", "grabCS"))){
       val res1 = krivine.execute(params.head, Vector(), env, varoffset)
       res1.head match{
         case n: NFNName => return Vector(NFNContentInst(nfnNode.grabCS(n))) //TODO handle content not only name
         case _ => {
           println("Wrong argument in grabCS")
         }
       }
       return null
     }
    else if(fname == NFNName(Vector("local", "pushCS"))){
       val content = krivine.execute(params.head, Vector(), env, varoffset)
       content.head match{
         case NFNContentInst(c) => {
           nfnNode.CS.put(c.name, c)
           return Vector(NOP())
         }
         case _ => {
           println("Wrong argument in pushCS")
         }
       }
     }
    else if(fname == NFNName(Vector("local", "checkPIT"))){
       val res1 = krivine.execute(params.head, Vector(), env, varoffset)
       res1.head match{
         case n: NFNName => if(nfnNode.checkPIT(n)) return Vector(NUMBER(1)) else return Vector(NUMBER(0))
         case _ => {
           println("Wrong argument")
         }
       }
       return null
     }
    else if(fname == NFNName(Vector("local", "grabPIT"))){ //TODO
       /*val res1 = krivine.execute(params.head, Vector(), env, varoffset)
       res1.head match{
         case n: NFNName => return Vector(NUMBER(nfnNode.grabPIT(n)))
         case _ => {
           println("Wrong argument in grabPIT")
         }
       }
       return null*/
     }
    else if(fname == NFNName(Vector("local", "pushPIT"))){
       val interest = krivine.execute(params.head, Vector(), env, varoffset)
       //val face = krivine.execute(params.tail.head, Vector(), env, varoffset)
       (interest.head) match {
         case (i: NFNName) => {
            nfnNode.PIT.put(i, krivine.krivineThread) ///TODO THINK ABOUT HOW PIT WORKS AND WHAT TO ADD TO PIT HERE. PIT has to maintain multiple interests for multiple threads --> list
           return Vector(NOP())
         }
         case _ => {
           println("Wrong argument in pushPIT")
         }
       }
     }
    else if(fname == NFNName(Vector("local", "checkFIB"))){
       val res1 = krivine.execute(params.head, Vector(), env, varoffset)
       res1.head match{
         case n: NFNName => if(nfnNode.checkFIB(n)) return Vector(NUMBER(1)) else return Vector(NUMBER(0))
         case _ => {
           println("Wrong argument in checkFIB")
         }
       }
     }
     else if(fname == NFNName(Vector("local", "grabFIB"))){
       val res1 = krivine.execute(params.head, Vector(), env, varoffset)
       res1.head match{
         case n: NFNName => return Vector(NUMBER(nfnNode.grabFIB(n)))
         case _ => {
           println("Wrong argument in grabFIB")
         }
       }
       return null
     }
    else if(fname == NFNName(Vector("local", "sendInterest"))){
       val name = krivine.execute(params.head, Vector(), env, varoffset)
       val facenum = krivine.execute(params.tail.head, Vector(), env, varoffset)
       (name.head, facenum.head) match{
         case (n: NFNName, fn: NUMBER) => {
           val comps = if(n.comps.head == "self") krivine.instructions else ???
           val interest = new NFNInterest(PacketCommand(comps), "interest", null)
           nfnNode.sendPacket(interest, fn.v)
           return Vector(NOP())
         }
         case(i: NFNInterestInst, fn: NUMBER) => {
           nfnNode.sendPacket(i.i, fn.v)
           return Vector(NOP())
         }
         case _ => {
           println("Wrong argument in sendInterest")
         }
       }
     }
    else if(fname == NFNName(Vector("local", "sendContent"))) {
       val content = krivine.execute(params.head, Vector(), env, varoffset)
       val facenum = krivine.execute(params.tail.head, Vector(), env, varoffset)

       (content.head, facenum.head) match {
         case (c: NFNContentInst, fn: NUMBER) => {
           nfnNode.sendPacket(c.c, fn.v)
           return Vector(NOP())
         }
         case(c: NFNContentInst, name: NFNName) => {
           println("name", name)
            if(name.comps.head == "origin"){
              nfnNode.sendPacket(c.c, krivine.krivineThread.originFace)
              return Vector(NOP())
            }
           else{
              DEBUGMSG(Debuglevel.DEBUG, "Error, unknown face in sendContent")
            }
         }
         case _ => {
           println("Wrong argument in sendContent")
         }
       }
     }
    else if(fname == NFNName(Vector("local", "wait"))){ //TODO as machine instruction? Wait for a number of interests arrived for this thread?
       DEBUGMSG(Debuglevel.DEBUG, "Thread is waiting")
       krivine.krivineThread.s.acquire()
       return Vector(NOP())
     }
    else if(fname == NFNName(Vector("local", "mkInterest"))){
       //return mkInterest(params.head, env, varoffset, krivine)
       return Vector(NFNInterestInst(NFNInterest(PacketCommand(params.head), "typ", null)))
    }
    else if(fname == NFNName(Vector("local", "mkContent"))){
      return mkContent(params.head, params.tail.head, env, varoffset, krivine)
    }
     ???
  }
  
  def add(param1: Vector[KrivineInstruction], param2: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
    
    val res1 = krivine.execute(param1, Vector(), env, varoffset)
    val res2 = krivine.execute(param2, Vector(), env, varoffset)

    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: add " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
        
    (res1.head, res2.head) match {
      case (v1: Addable, v2: Addable) => {
        return Vector(v1 + v2)
      }
      case _ => ???
    }
  }
    
  def eq(param1: Vector[KrivineInstruction], param2: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
    val res1 = krivine.execute(param1, Vector(), env, varoffset)
    val res2 = krivine.execute(param2, Vector(), env, varoffset)
    
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: eq " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
  
    (res1.head, res2.head) match {
      case (v1: Comparable, v2: Comparable) => {
        return Vector(v1 == v2)
      }
      case _ => ???
    }
  }
  
  def gt(param1: Vector[KrivineInstruction], param2: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
    val res1 = krivine.execute(param1, Vector(), env, varoffset)
    val res2 = krivine.execute(param2, Vector(), env, varoffset)
    
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: gt " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
   
    (res1.head, res2.head) match {
      case (v1: Comparable, v2: Comparable) => {
        return Vector(v1 > v2)
      }
      case _ => ???
    }
  }

  def head(param1: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
     val res = krivine.execute(param1, Vector(), env, varoffset)
     
     DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: head " + res +" (" + param1 + ") " + varoffset)
   
     res.head match {
       case LISTINST(l) => return l.head
     }
  }
  
  def tail(param1: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
     val res = krivine.execute(param1, Vector(), env, varoffset)
     
     DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: tail " + res +" (" + param1 + ") " + varoffset)
     res.head match {
       case LISTINST(l) => return Vector(LISTINST(l.tail))
     }
  }
  
  def length(param1: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
     val res = krivine.execute(param1, Vector(), env, varoffset)
    
     DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: length " + res +" (" + param1 + ") " + varoffset)
     
     res.head match {
       case LISTINST(l) => return Vector(NUMBER(l.length))
     }
  }
  
  def prepend(param1: Vector[KrivineInstruction], param2: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
    val res1 = krivine.execute(param1, Vector(), env, varoffset)
    val res2 = krivine.execute(param2, Vector(), env, varoffset)
    
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: prepend " + res1 +" (" + param1 + ") " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
    
    (res1.head, res2.head) match {
      case (v1: KrivineInstruction, LISTINST(l)) => {
        return  Vector(LISTINST( Vector(Vector(v1)) ++ l))
      }
      case _ => ???
    }
  }

  def mkInterest(param1: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {

    val res1 = krivine.execute(param1, Vector(), env, varoffset)
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: mkInterest " + res1 +" (" + param1 + ")  with varoffset: " + varoffset)

    res1 match{
      case n : Vector[KrivineInstruction] => {
        return Vector(NFNInterestInst(NFNInterest(PacketCommand(res1), "typ", null)))
      }
      case _ => ???
    }
  }

  def mkContent(param1: Vector[KrivineInstruction], param2: Vector[KrivineInstruction], env: Map[Int, Vector[KrivineInstruction]], varoffset: Int, krivine: Krivine): Vector[KrivineInstruction] = {
    val res1 = krivine.execute(param1, Vector(), env, varoffset)
    val res2 = krivine.execute(param2, Vector(), env, varoffset)
    DEBUGMSG(Debuglevel.DEBUG,"Performing buildin function: mkInterest " + res1 +" (" + param1 + ") + " + res2 +" (" + param2 + ") with varoffset: " + varoffset)
    println(res2)
    (res1.head, res2) match{
      case (n: NFNName, c: Vector[KrivineInstruction]) => {
        return Vector(NFNContentInst(NFNContent(n, "typ", null, c.toString())))
      }
      case _ => ???
    }
  }
  
}