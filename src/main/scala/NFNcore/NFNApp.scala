package NFNcore
/**
 * Created by blacksheeep on 24/03/15.
 */


case class NFNAppConfig(receiveport: Int = 9000)

object NFNApp extends App{

  val parser = new scopt.OptionParser[NFNAppConfig]("scopt") {
    head("scopt", "3.x")
    opt[Int]('s', "server") action { (x, c) => c.copy(receiveport = x) }
  }

  parser.parse(args, NFNAppConfig()) match{
    case Some(config) => {

      val nfnNode = new NFNNode(config.receiveport)
      nfnNode.mainloop()
    }
    case None => ???
  }
}
