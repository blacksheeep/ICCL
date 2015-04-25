package NFNcore.Lambda

import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.NFNNode

/**
 * Created by blacksheeep on 15/04/15.
 */
case class KrivineThread(prog: Vector[KrivineInstruction], nfnNode: NFNNode, originFace: Int) extends Thread{
  val krivine = new Krivine(nfnNode, this)
  val s: java.util.concurrent.Semaphore = new java.util.concurrent.Semaphore(0);

  override def start(): Unit ={
    val res =  krivine(prog)
    DEBUGMSG(Debuglevel.DEBUG, "Computation Finished! Result: " + res)
  }
}
