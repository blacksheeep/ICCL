package NFNcore

import java.io.ObjectOutputStream

import Logging.{DEBUGMSG, Debuglevel}
import NFNcore.Messaging.{Interface, TCPInterface, TCPServerInterface}
import NFNcore.Packets.{NFNInterest, NFNContent, NFNManagement, Packet}
import Socket.{TCPSocketThread, TCPSocketServerThread}
import java.util.concurrent.ConcurrentHashMap

/**
 * Created by blacksheeep on 26/03/15.
 */
class NFNNode(serverport: Int){


  var CS: ConcurrentHashMap[List[String], NFNContent] = new ConcurrentHashMap()
  var FIB: ConcurrentHashMap[List[List[String]], Int] = new ConcurrentHashMap()
  var PIT: ConcurrentHashMap[NFNInterest, List[Int]] = new ConcurrentHashMap()
  var nextFaceNum = 0;

  var faces: List[TCPSocketThread] = List()
  val reciveface = new TCPSocketServerThread(serverport, handlePacket)


  def mgmt(packet: NFNManagement): Unit ={
    DEBUGMSG(Debuglevel.INFO, "handle mgmt")
    if(packet.command == "newface"){ //address port face
      val targetaddress = packet.params(0)
      val targetport = packet.params(1).toInt
      val newface = new TCPSocketThread(nextFaceNum, targetaddress, targetport, List("hello"), handlePacket)
      nextFaceNum +=1

      newface.start()
      faces = newface :: faces

      DEBUGMSG(Debuglevel.DEBUG, "Successfully installed new Face")
    }
    else if(packet.command == "addcontent"){
      val name = packet.params(0).split("/").toList
      val content = packet.params(1)
      CS.put(name, NFNContent(name,"type", Nil, content))

      DEBUGMSG(Debuglevel.DEBUG, "Successfully added Content" + NFNContent(name,"type", Nil, content).toString)
    }
  }

  def checkCS(name: List[String]): Boolean = { //todo: prefix matching?
    if (CS.containsKey(name)) return true else return false
  }

  def grabCS(name: List[String]): NFNContent = {
    return CS.get(name)
  }

  def handleInterest(packet: NFNInterest, reply: ObjectOutputStream) = {
    DEBUGMSG(Debuglevel.INFO, "handle interest message" + packet.toString)

    println(packet.name, CS.toString)
    if(checkCS(packet.name)){
      reply.writeObject(grabCS(packet.name))
    }
    else ???
  }

  def handlePacket(packet: Packet, reply: ObjectOutputStream): Unit = {
    DEBUGMSG(Debuglevel.INFO, "handle packet" + packet.toString)

    packet match{
      case m: NFNManagement => mgmt(m)
      case i: NFNInterest => handleInterest(i, reply)
    }
    //reply.writeObject(new NFNContent(List("hallo", "welt"), "test", Nil, ""))
  }

  def mainloop(): Unit ={
    var finish: Boolean = false
    reciveface.start()
    faces.foreach(f => f.start())

  }
}
