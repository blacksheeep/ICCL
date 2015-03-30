package Socket

import java.io.ObjectOutputStream

import NFNcore.Messaging.TCPServerInterface
import NFNcore.Packets._

/**
 * Created by blacksheeep on 30/03/15.
 */
class TCPSocketServerThread(port: Int, packethandler: (Packet, ObjectOutputStream) => Unit) extends Thread{
  val sock: TCPServerInterface = new TCPServerInterface(port)
  var running = false

  override def run(): Unit = {
    running = true;

    while(running){
      val (pkt, out) = sock.receivePacket()

      packethandler(pkt, out)

    }
  }
}
