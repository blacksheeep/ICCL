package NFNcore.Messaging


import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.{InetAddress, ServerSocket, Socket}

import NFNcore.Packets._



class PacketEncoder{

  def encodePacket(pkt: Packet): String = {
    return pkt.toString
  }

  val NFNInterestRegex = "NFNInterest\\(List\\(.*,.*\\),.*, \\)"

  def decodePacket(str: String): Packet = {
    //sometimes you have to replace the template with NFNInterest or NFNContent to compile it successful
    /*str.unpickle[Packet] match {
      case i: NFNInterest => i
      case c: NFNContent => c
    }
    println("Unknown Packettype")*/
    ???
  }
}

sealed trait Interface{
  val num: Int
  val out: ObjectOutputStream

  def sendPacket(packet: Packet)

  def receivePacket(): Packet
}

case class TCPInterface(address: String, port: Int, prefix: List[String], num: Int) extends Interface {
  val sock: Socket = new Socket(InetAddress.getByName(address), port)
  lazy val in = new ObjectInputStream(sock.getInputStream())
  val out = new ObjectOutputStream(sock.getOutputStream())


  override def sendPacket(pkt: Packet) = {
    out.writeObject(pkt)
  }

  override def receivePacket(): Packet = {
    val pkt = in.readObject() match {
      case i: NFNInterest => i
      case c: NFNContent => c
      case m: NFNManagement => m
      case _ => ???
    }
    return pkt
  }
}

case class TCPServerInterface(port: Int) {

  val server = new ServerSocket(port)

  def sendPacket(pkt: Packet, out: ObjectOutputStream) = {
    out.writeObject(pkt)
  }

  //maintain list with reply sockets for sending a reply msg

  def receivePacket(): (Packet, ObjectOutputStream) = {
    val sock = server.accept()
    val in = new ObjectInputStream(sock.getInputStream())
    val out = new ObjectOutputStream(sock.getOutputStream())

    val pkt = in.readObject() match {
      case i: NFNInterest => return (i, out)
      case c: NFNContent => return (c, out)
      case m: NFNManagement => return (m, out)
      case _ => ???
    }
    ???
  }
}
