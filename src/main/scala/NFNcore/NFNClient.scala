package NFNcore

import NFNcore.Lambda._

import NFNcore.Messaging.TCPInterface
import NFNcore.Packets._

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
      val parser = new LambdaParser
      val compiler = new LambdaCompiler

      //add content
      face.sendPacket(NFNManagement("addcontent", List("hallo/welt", "halloweltdata")))
      val res_mgmt = face.receivePacket()
      println(res_mgmt)

      Thread.sleep(1000)

      //val prog = "add 1 2"
      val prog = "call 2 /local/sendContent; /hallo/welt; 0"

      val src = parser.applyDict(prog)
      val ast = parser.parse(src)

      println(ast)

      val opcode = compiler(ast.get)

      val pc = new PacketCommand(opcode)


      //val name = PacketCommand2(Vector(NFNName2(Vector("foo")), NFNName2(Vector("bla"))))

      val interest = new NFNInterest(pc, "interest", null)
      println(interest)
      face.sendPacket(interest)

      val res = face.receivePacket()
      println(res)




      /*val face2 = new TCPInterface("localhost", 10001, 0)
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
      println(reply5)*/

    }
    case None => ???
  }


}
