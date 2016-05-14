package me.abarrow.ScalaSubNet

import java.io.File
import me.abarrow.ScalaSubNet.utils.FileUtils

class WordContext (val wordListFolder:String, val wordTypes:Set[String]) {
  val unknownWordType = "unknown"

  val wordTypesAndUnknown = wordTypes + unknownWordType
  
  
  val wordList: Set[(String, Set[String])] = {
    wordTypes.map { x => (x, loadWordList(wordListFolder + File.separatorChar + x + ".txt")) }
  }
  
  private def loadWordList(path: String): Set[String] = {
    val fileContent = FileUtils.openUTF8Text(path)
    FileUtils.splitApartWords(fileContent).map { s => s.toLowerCase() }.toSet
  }
  
}