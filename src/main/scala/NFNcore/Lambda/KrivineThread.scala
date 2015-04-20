package NFNcore.Lambda

import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.NFNNode

/**
 * Created by blacksheeep on 15/04/15.
 */
class KrivineThread(prog: Vector[KrivineInstruction], nfnNode: NFNNode) extends Thread{
  val krivine = new Krivine(nfnNode)

  override def start(): Unit ={
    val res =  krivine(prog)
    DEBUGMSG(Debuglevel.DEBUG, "Computation Finished! Result: " + res)
  }
}
