package me.abarrow.ScalaSubNet.mal

class MALEntry (val id:Int, val name:String, val score:Int, val userStatus:Int) {
  override def toString():String = {
    name + " ID:" + id + " Score:" + score + " usetStatus:[" + MALIDs.statuses(userStatus) + "]"
  }
  
}