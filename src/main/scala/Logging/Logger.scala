package Logging

object Debuglevel extends Enumeration{
  type Debuglevel = Value
  val NONE, INFO, DEBUG, TRACE = Value
  
}

import Logging.Debuglevel._

object DEBUGMSG {
  val DEBUGLEVEL = Debuglevel.TRACE
  
  def apply(level: Debuglevel, msg: String) = {
    debugprint(level, msg)
  }
  
  def debugprint(level: Debuglevel, msg: String) = {
    val timestamp = System.currentTimeMillis().toDouble / 1000000000000.0
    
    if(level.id <= DEBUGLEVEL.id){
      println("[" + "%.4f".format(timestamp) + "] [" + level.toString() + "]: \t" + msg)
    }
  }
}