package Socket

import java.io.ObjectOutputStream
import java.net.Socket

import Logging.{Debuglevel, DEBUGMSG}
import NFNcore.Messaging.TCPInterface
import NFNcore.Packets._

/**
 * Created by blacksheeep on 30/03/15.
 */
class TCPSocketThread(num: Int, static: Boolean, sock: TCPInterface, packethandler: (Packet, ObjectOutputStream, Int) => Unit) extends Thread{
  var running = false


  def this(num: Int, static: Boolean, address: String, port: Int, packethandler: (Packet, ObjectOutputStream, Int) => Unit) = {
    this(num, static, new TCPInterface(address, port, num), packethandler)
  }

  def getInterface(): TCPInterface ={
    return sock
  }

  override def run(): Unit = {
    running = true;
    try {
      while (running) {
        try {
          val pkt = sock.receivePacket()
          packethandler(pkt, sock.out, num)
        }catch{
          case e: Exception => { }
        }

      }
    }catch{
      case e: Exception => {
        DEBUGMSG(Debuglevel.DEBUG, "Exit Thread: " + Thread.currentThread().getId)
        println(e.getMessage)
        e.printStackTrace()
        return
      }
    }
  }

  def sendPacket(pkt: Packet) = {
    sock.sendPacket(pkt)
  }
}
