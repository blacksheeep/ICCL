package NFNcore.Lambda

import NFNcore.Packets.{NFNInterest, NFNContent}

abstract class KrivineInstruction extends Serializable


/**
 * Interfaces for buildin operations
 */
trait Addable extends KrivineInstruction{
  def +(second: Addable): Addable
}
trait Comparable extends KrivineInstruction{
  def == (second: Comparable): Comparable
  def > (second: Comparable): Comparable
}

/**
 * Instruction Set of the Krivine Machine
 */
case class RESULT(res: String) extends KrivineInstruction
case class ACCESS(varname: String, varnum: Int) extends KrivineInstruction
case class GRAB(varname: String, varnum: Int) extends KrivineInstruction
case class PUSH(k: Vector[KrivineInstruction]) extends KrivineInstruction
case class NOP() extends KrivineInstruction
case class NUMBER(v: Int) extends KrivineInstruction with Addable with Comparable{
  override def + (second: Addable): Addable = {
    second match{
      case NUMBER(v2) => return NUMBER(v+v2)
    }
  }
  override def == (second: Comparable): Comparable = {
    second match{
      case NUMBER(v2) => return if(v == v2) NUMBER(1) else NUMBER(0)
    }
  }
  override def > (second: Comparable): Comparable = {
    second match{
      case NUMBER(v2) => return if(v > v2) NUMBER(1) else NUMBER(0)
    }
  }
}
case class STRING(str: String) extends KrivineInstruction
case class NFNName(comps: Vector[String]) extends KrivineInstruction
case class VARIABLE(name: String, varnum: Int) extends KrivineInstruction
case class LISTINST(list: Vector[Vector[KrivineInstruction]]) extends KrivineInstruction

case class CALLINST(fname: NFNName, num: Int, params: Vector[Vector[KrivineInstruction]]) extends KrivineInstruction {//TODO: fine gradulated operations, ifelse, send, excludeparameter etc.?
  def getKrivinInstruction() = ???
}

case class IFELSEINST(condition: Vector[KrivineInstruction], fullfiled: Vector[KrivineInstruction], notfulfilled: Vector[KrivineInstruction]) extends KrivineInstruction

case class FUNCTIONINST(name: NFNName, numOfParams: Int, startVarNum: Int, expr: Vector[KrivineInstruction], prog: Vector[KrivineInstruction]) extends KrivineInstruction


case class NFNInterestInst(i: NFNInterest) extends KrivineInstruction
case class NFNContentInst(c: NFNContent) extends KrivineInstruction

case class WAIT() extends KrivineInstruction
