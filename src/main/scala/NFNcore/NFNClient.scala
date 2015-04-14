package NFNcore

import NFNcore.Messaging.TCPInterface
import NFNcore.Packets.{NFNName, PacketCommand, NFNManagement, NFNInterest}

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

      val face2 = new TCPInterface("localhost", 10001, 0)
      face2.sendPacket(NFNManagement("addcontent", List("hallo/welt", "halloweltdata")))
      val reply1 = face2.receivePacket()
      println(reply1)

      val face = new TCPInterface("localhost", config.targetport, 0)

      //install face
      face.sendPacket(NFNManagement("newface", List("localhost", "10001")))
      val reply2 = face.receivePacket()
      println(reply2)


      face.sendPacket(NFNManagement("prefixreg", List("1", "hallo/welt")))
      val reply3 = face.receivePacket()
      println(reply3)

      Thread.sleep(2000)

      face.sendPacket(NFNInterest(NFNName(List("hallo", "welt")),"test", Nil))
      val reply4 = face.receivePacket()
      println(reply4)


      Thread.sleep(5000)

      face.sendPacket(NFNInterest(NFNName(List("hallo", "welt")),"test", Nil))

      val reply5 = face.receivePacket()
      println(reply5)

    }
    case None => ???
  }


}
