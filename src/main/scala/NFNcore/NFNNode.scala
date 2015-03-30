package NFNcore

import java.io.ObjectOutputStream

import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.Messaging.{Interface, TCPInterface, TCPServerInterface}
import NFNcore.Packets.{NFNContent, NFNManagement, Packet}
import Socket.{TCPSocketThread, TCPSocketServerThread}

/**
 * Created by blacksheeep on 26/03/15.
 */
class NFNNode(serverport: Int){
  //TODO PIT
  //TODO FIB
  //TODO CS

  var faces: List[TCPSocketThread] = List()
  val reciveface = new TCPSocketServerThread(serverport, handlePacket)


  def mgmt(packet: NFNManagement): Unit ={
    if(packet.command == "newface"){
      val targetaddress = packet.params(0)
      val targetport = packet.params(1).toInt
      val newface = new TCPSocketThread(targetaddress, targetport, List("hello"), handlePacket)
      newface.start()
      faces = newface :: faces

      DEBUGMSG(Debuglevel.DEBUG, "Installed Face successfull")
    }
  }

  def handlePacket(packet: Packet, reply: ObjectOutputStream): Unit = {
    println("handle packet", packet.toString)

    packet match{
      case p: NFNManagement => mgmt(p)
      case _ => reply.writeObject(new NFNContent(List("hello", "world"), "test", Nil, Nil))
    }
    reply.writeObject(new NFNContent(List("hallo", "welt"), "test", Nil, Nil))
  }

  def mainloop(): Unit ={
    var finish: Boolean = false
    reciveface.start()
    faces.foreach(f => f.start())

  }
}
