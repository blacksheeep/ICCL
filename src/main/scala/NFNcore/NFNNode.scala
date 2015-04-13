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
  var FIB: ConcurrentHashMap[List[String], Int] = new ConcurrentHashMap()
  var PIT: ConcurrentHashMap[List[String], List[Int]] = new ConcurrentHashMap()



  val reciveface = new TCPSocketServerThread(serverport, handlePacket)


  def mgmt(packet: NFNManagement, reply: ObjectOutputStream): Unit ={
    DEBUGMSG(Debuglevel.INFO, "handle mgmt")
    if(packet.command == "newface") { //address port face
      val targetaddress = packet.params(0)
      val targetport = packet.params(1).toInt
      val newface = new TCPSocketThread(reciveface.nextFaceNum, true, targetaddress, targetport, handlePacket)

      DEBUGMSG(Debuglevel.DEBUG, "Successfully installed new Face with ID " + reciveface.nextFaceNum)

      reply.writeObject(NFNManagement("newface", List("faceid", reciveface.nextFaceNum.toString)))
      reciveface.nextFaceNum += 1

      newface.start()
      reciveface.faces = newface :: reciveface.faces

    }
    else if(packet.command == "prefixreg"){ // faceid name
      val faceid = packet.params(0).toInt
      val name = packet.params(1).split("/").toList
      FIB.put(name, faceid)

      DEBUGMSG(Debuglevel.DEBUG, "Successfully added prefix " + name + " to face with faceid " + faceid)
      reply.writeObject(NFNManagement("prefixreg", List("successful")))

    }
    else if(packet.command == "addcontent"){
      val name = packet.params(0).split("/").toList
      val content = packet.params(1)
      CS.put(name, NFNContent(name,"type", Nil, content))

      DEBUGMSG(Debuglevel.DEBUG, "Successfully added Content" + NFNContent(name,"type", Nil, content).toString)
      reply.writeObject(NFNManagement("addcontent", List("successful")))
    }
  }

  def checkCS(name: List[String]): Boolean = { //todo: prefix matching?
    if (CS.containsKey(name)) return true else return false
  }

  def grabCS(name: List[String]): NFNContent = {
    return CS.get(name)
  }

  def checkFIB(name: List[String]): Boolean = {
    if (FIB.containsKey(name)) return true else return false
  }

  def grabFIB(name : List[String]): Int = {
    return FIB.get(name)
  }

  def checkPIT(interest: List[String]): Boolean ={
    return if (PIT.containsKey(interest)) return true else return false
  }

  def grabPIT(interest: List[String]) : List[Int] = {
    return PIT.get(interest)
  }

  def pushPIT(interest: List[String], incommingFace: Int): Unit = {
    if(PIT.containsKey(interest)){
      val entry = PIT.get(interest)
      PIT.put(interest, incommingFace :: entry)
    }
    else{
      PIT.put(interest, List(incommingFace))
    }
  }

  def handleInterest(packet: NFNInterest, reply: ObjectOutputStream, incommingFace: Int) = {
    DEBUGMSG(Debuglevel.INFO, "handle interest message" + packet.toString)

    println(packet.name, CS.toString)

    if(checkCS(packet.name)){
      reply.writeObject(grabCS(packet.name))
      DEBUGMSG(Debuglevel.DEBUG, "Interest was replied")
    }
    else if(checkFIB(packet.name)){
      val face = grabFIB(packet.name)
      val interest = new NFNInterest(packet.name, "interest", null)
      pushPIT(interest.name, incommingFace)
      sendPacket(interest, face)
      DEBUGMSG(Debuglevel.DEBUG, "Interest received on face " + incommingFace + " was forwarded to face " + face)
    }
  }

  def handleContent(content: NFNContent, reply: ObjectOutputStream) : Unit = {
    if(checkPIT(content.name)){
      val outface = grabPIT(content.name)
      sendPacket(content, outface(0)) //TODO for all entries of outface
      DEBUGMSG(Debuglevel.DEBUG, "Handle Content, forwarded to face " + outface.toString )
    }else{
      DEBUGMSG(Debuglevel.DEBUG, "No Matching PIT entry")
    }
  }

  def sendPacket(interest: Packet, outface: Int): Unit ={

    val face = reciveface.faces.find(p => p.getInterface().num == outface)


    //val face = reciveface.faces(outface) //TODO check real face number or faces have to be map with id -> face
    face match{
      case Some(t) => t.sendPacket(interest)
      case _ => {DEBUGMSG(Debuglevel.DEBUG, "No matching interface found")}
    }

  }

  def handlePacket(packet: Packet, reply: ObjectOutputStream, incommingFace: Int): Unit = {
    DEBUGMSG(Debuglevel.INFO, "handle packet" + packet.toString)
    packet match{
      case m: NFNManagement => mgmt(m, reply)
      case i: NFNInterest => handleInterest(i, reply, incommingFace)
      case c: NFNContent => handleContent(c, reply)
      case _ => reply.writeObject(new NFNContent(List("dummy"), "test", Nil, ""))
    }

  }

  def mainloop(): Unit ={
    var finish: Boolean = false
    reciveface.start()
    reciveface.faces.foreach(f => f.start())

  }
}
