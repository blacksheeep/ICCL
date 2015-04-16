package NFNcore.Packets

import NFNcore.Lambda.{NFNName, KrivineInstruction}

sealed trait Packet extends java.io.Serializable

case class Selector(name:String, value:String)

case class NFNInterest(name: PacketCommand, InterestType: String, selector: List[Selector]) extends Packet{
  
}

case class NFNContent(name: NFNName, ContentType: String, metadata: List[Selector], data: String) extends Packet{
  
}

case class NFNManagement(command: String, params: List[String]) extends Packet{

}


/**
 * Data structure representations
 */
case class PacketCommand(commands: List[KrivineInstruction]) {}