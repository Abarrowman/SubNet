

package me.abarrow.ScalaSubNet

class SubtitleTrack(val trackId:Int, val codecId:String, val trackName:Option[String], val trackLang:Option[String]) {
  
  override def toString():String = {
    trackId + " " + codecId + " " + trackName.getOrElse("") + "" + trackLang.getOrElse("")
  }
  
}