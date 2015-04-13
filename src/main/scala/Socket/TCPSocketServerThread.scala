package Socket

import java.io.ObjectOutputStream

import NFNcore.Messaging.{TCPInterface, TCPServerInterface}
import NFNcore.Packets._

/**
 * Created by blacksheeep on 30/03/15.
 */
class TCPSocketServerThread(port: Int, packethandler: (Packet, ObjectOutputStream, Int) => Unit) extends Thread{
  val sock: TCPServerInterface = new TCPServerInterface(port)

  var faces: List[TCPSocketThread] = List()
  var running = false

  var nextFaceNum = 0;

  override def run(): Unit = {
    running = true
    while(running){
      val (pkt, tcpInterface) = sock.receivePacket()
      val newface = new TCPSocketThread(nextFaceNum, false, tcpInterface, packethandler)
      faces = newface :: faces
      nextFaceNum += 1
      newface.start()
      packethandler(pkt, tcpInterface.out, -1)

    }
  }
}
