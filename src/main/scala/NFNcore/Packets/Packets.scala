package NFNcore.Packets

sealed trait Packet

case class Selector(name:String, value:String)

case class NFNInterest(name: List[String], InterestType: String, selector: List[Selector]) extends Packet{
  
}

case class NFNContent(name: List[String], ContentType: String, metadata: List[Selector], data: List[Byte]) extends Packet{
  
}
