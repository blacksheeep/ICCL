package Socket

import java.io.ObjectOutputStream
import java.net.Socket

import Logging.{Debuglevel, DEBUGMSG}
import NFNcore.Messaging.TCPInterface
import NFNcore.Packets._

/**
 * Created by blacksheeep on 30/03/15.
 */
class TCPSocketThread(num: Int, static: Boolean, sock: TCPInterface, packethandler: (Packet, ObjectOutputStream) => Unit) extends Thread{
  var running = false


  def this(num: Int, static: Boolean, address: String, port: Int, packethandler: (Packet, ObjectOutputStream) => Unit) = {
    this(num, static, new TCPInterface(address, port, num), packethandler)
  }

  override def run(): Unit = {
    running = true;
    try {
      while (running) {
        val pkt = sock.receivePacket()
        packethandler(pkt, sock.out)

      }
    }catch{
      case e: Exception => {
        DEBUGMSG(Debuglevel.DEBUG, "Exit Thread: " + Thread.currentThread().getId)
        return
      }
    }
  }
}
