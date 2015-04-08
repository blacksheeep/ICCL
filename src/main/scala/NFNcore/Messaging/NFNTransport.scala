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

case class TCPInterface(sock: Socket, num: Int) extends Interface {

  def this(address: String, port: Int, num: Int) = {
    this(new Socket(InetAddress.getByName(address), port), num)
  }

  lazy val in = new ObjectInputStream(sock.getInputStream())
  val out = new ObjectOutputStream(sock.getOutputStream())


  override def sendPacket(pkt: Packet) = {
    out.writeObject(pkt)
    out.flush()
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

  def receivePacket(): (Packet, TCPInterface) = {
    val sock = server.accept()

    val tcpInterface = new TCPInterface(sock, 0)


    val pkt = tcpInterface.receivePacket() match {
      case i: NFNInterest => return (i, tcpInterface)
      case c: NFNContent => return (c, tcpInterface)
      case m: NFNManagement => return (m, tcpInterface)
      case _ => ???
    }
    ???
  }
}
