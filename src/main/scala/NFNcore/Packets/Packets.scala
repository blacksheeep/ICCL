package NFNcore.Packets

import NFNcore.Lambda.{NFNName, KrivineInstruction}

sealed trait Packet extends java.io.Serializable

case class Selector(name:String, value:String) extends Serializable

case class NFNInterest(name: PacketCommand, InterestType: String, selector: List[Selector]) extends Packet

case class NFNContent(name: NFNName, ContentType: String, metadata: List[Selector], data: String) extends Packet

case class NFNManagement(command: String, params: List[String]) extends Packet

/**
 * Data structure representations
 */
case class PacketCommand(commands: Vector[KrivineInstruction]) extends Serializable

case class NFNName2(name: Vector[String])

case class PacketCommand2(commands: Vector[NFNName2]) extends Serializable