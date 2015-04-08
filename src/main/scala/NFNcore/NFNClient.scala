package NFNcore

import NFNcore.Messaging.TCPInterface
import NFNcore.Packets.{NFNManagement, NFNInterest}

/**
 * Created by blacksheeep on 26/03/15.
 */

case class NFNClientConfig(targetport: Int = 9000)

object NFNClient extends App{
  val parser = new scopt.OptionParser[NFNClientConfig]("scopt") {
    head("scopt", "3.x")
    opt[Int]('p', "port") action { (x, c) => c.copy(targetport = x) }
  }

  parser.parse(args, NFNClientConfig()) match {
    case Some(config) => {
      val face = new TCPInterface("localhost", config.targetport, 0)

      //install face
      //face.sendPacket(NFNManagement("newface", List("localhost", "10001")))

      face.sendPacket(NFNManagement("addcontent", List("hallo/welt", "halloweltdata")))
      //val reply1 = face.receivePacket()
      //println(reply1)

      Thread.sleep(2000)

      println("Sending packet: ")
      face.sendPacket(NFNInterest(List("hallo", "welt"),"test", Nil))

      println("Packet sent, waiting for reply")
      val reply2 = face.receivePacket()

      println(reply2)
      face.sock.close()
    }
    case None => ???
  }


}
