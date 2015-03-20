package NFNcore.Lambda

abstract class KrivineInstruction


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
case class PUSH(k: List[KrivineInstruction]) extends KrivineInstruction
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
case class VARIABLE(name: String, varnum: Int) extends KrivineInstruction
case class LISTINST(list: List[List[KrivineInstruction]]) extends KrivineInstruction

case class CALLINST(fname: String, num: Int, params: List[List[KrivineInstruction]]) extends KrivineInstruction {//TODO: fine gradulated operations, ifelse, send, excludeparameter etc.
  def getKrivinInstruction() = ???
}
case class LOOKUPINST(name: String) extends KrivineInstruction //TODO: fine gradulated operations
case class IFELSEINST(condition: List[KrivineInstruction], fullfiled: List[KrivineInstruction], notfulfilled: List[KrivineInstruction]) extends KrivineInstruction

case class FUNCTIONINST(name: String, numOfParams: Int, startVarNum: Int, expr: List[KrivineInstruction], prog: List[KrivineInstruction]) extends KrivineInstruction