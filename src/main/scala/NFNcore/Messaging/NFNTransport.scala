package NFNcore.Messaging


import NFNcore.Packets._

import scala.pickling.Defaults._, scala.pickling.json._


class NFNTransport{

  def encodePacket(pkt: Packet): String = {
    pkt match {
      case i: NFNInterest => i.pickle.value
      case c: NFNContent => c.pickle.value

    }
  }

  def decodePacket(str: String): Packet = {
    str.unpickle[Packet] match { //sometimes you have to replace the template with NFNInterest or NFNContent to compile it successful
      case i: NFNInterest => i
      case c: NFNContent => c
    }
  }
}



