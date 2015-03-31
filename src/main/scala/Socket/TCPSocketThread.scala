package Socket

import java.io.ObjectOutputStream

import NFNcore.Messaging.TCPInterface
import NFNcore.Packets._

/**
 * Created by blacksheeep on 30/03/15.
 */
class TCPSocketThread(num: Int, address: String, port: Int, prefix: List[String], packethandler: (Packet, ObjectOutputStream) => Unit) extends Thread{
  val sock = new TCPInterface(address, port, prefix, num)
  var running = false

  override def run(): Unit = {
    running = true;

    while(running){
      val pkt = sock.receivePacket()
      packethandler(pkt, sock.out)

    }
  }
}
