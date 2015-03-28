package NFNcore

import java.io.ObjectOutputStream

import Logging.{Debuglevel, DEBUGMSG}
import NFNcore.Messaging.{Interface, TCPInterface, TCPServerInterface}
import NFNcore.Packets.{NFNManagement, NFNContent, NFNInterest, Packet}

/**
 * Created by blacksheeep on 26/03/15.
 */
class NFNNode(serverport: Int){
  //TODO PIT
  //TODO FIB
  //TODO CS

  var faces: List[Interface] = List()
  val reciveface = new TCPServerInterface(serverport)


  def mgmt(packet: NFNManagement): Unit ={
    if(packet.command == "newface"){
      val targetaddress = packet.params(0)
      val targetport = packet.params(1).toInt
      val newface = new TCPInterface(targetaddress, targetport, List("hello"))
      faces = newface :: faces

      DEBUGMSG(Debuglevel.DEBUG, "Installed Face successfull")
    }
  }

  def handlePacket(packet: Packet, reply: ObjectOutputStream) = {
    println("handle packet", packet.toString)

    packet match{
      case p: NFNManagement => mgmt(p)
      case _ => reply.writeObject(new NFNContent(List("hello", "world"), "test", Nil, Nil))
    }
    reply.writeObject(new NFNContent(List("hallo", "welt"), "test", Nil, Nil))

  }

  def mainloop(): Unit ={
    var finish: Boolean = false
    while(!finish){
      val (pkt, out) = reciveface.receivePacket()
      handlePacket(pkt, out)
      faces.foreach(f => handlePacket(f.receivePacket(), f.out))

    }
  }
}
