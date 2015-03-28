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
      val face = new TCPInterface("localhost", config.targetport, List("hallo", "welt"))

      //face.sendPacket(NFNInterest(List("hallo", "welt"),"test", Nil))

      //install face
      face.sendPacket(NFNManagement("newface", List("localhost", "10001")))

      println("Packet sent, waiting for reply")
      val reply = face.receivePacket()

      println(reply)
    }
    case None => ???
  }


}
