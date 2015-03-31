package NFNcore.Packets

sealed trait Packet extends java.io.Serializable

case class Selector(name:String, value:String)

case class NFNInterest(name: List[String], InterestType: String, selector: List[Selector]) extends Packet{
  
}

case class NFNContent(name: List[String], ContentType: String, metadata: List[Selector], data: String) extends Packet{
  
}

case class NFNManagement(command: String, params: List[String]) extends Packet{

}
