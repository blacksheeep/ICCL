package NFNcore.Packets

sealed trait Packet

case class Selector(name:String, value:String)

case class NFNInterest(name: Array[String], InterestType: String, selector: Array[Selector]) extends Packet{
  
}

case class NFNContent(name: Array[String], ContentType: String, metadata: Array[Selector], data: Array[Byte]) extends Packet{
  
}
