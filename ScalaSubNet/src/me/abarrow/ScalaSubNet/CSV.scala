package me.abarrow.ScalaSubNet

import scala.collection.mutable.ArrayBuffer
import me.abarrow.ScalaSubNet.utils.FileUtils

class CSV(val header: Array[String]) {
  val rows = new ArrayBuffer[Array[String]]()

  def addRow(row: Array[String]): Unit = {
    if (row.length != header.length) {
      //TODO: handle quoted strings to avoid splitting on commas
      throw new IllegalArgumentException("Row length " + row.length + " is not the same as header length " + header.length)
    }
    this.synchronized {
      rows.append(row)
    }
  }

  def addRow(entry: Map[String, String]): Unit = {
    val row = Array.fill[String](header.length)("")
    entry.foreach { f =>
      val idx = header.indexOf(f._1)
      if (idx == -1) {
        throw new IllegalArgumentException("Column " + f._1 + " not found in header.")
      }
      row(idx) = f._2
    }
    addRow(row)
  }
  
  

  def encode(sep: String = ",", newline: String = "\n"): String = {
    val output: StringBuilder = new StringBuilder()
    output ++= CSV.joinAndEscape(header, sep, newline)
    output ++= newline
    rows.foreach { x =>
      output ++= CSV.joinAndEscape(x, sep, newline)
      output ++= newline
    }
    output.toString()
  }
  
  def save(path:String, sep: String = ",", newline: String = "\n"):Unit = {
    FileUtils.saveUTF8Text(path, encode(sep, newline))
  }
  
  def toMaps(): Array[Map[String, String]] = {
    rows.map { x => header.zip(x).toMap }.toArray
  }
}

object CSV {
  
  def decode(csvText:String, sep:String = ",", newline:String = "\n"): CSV = {
    val cleanText = csvText.replace("\r", "")
    val rows:ArrayBuffer[ArrayBuffer[String]] = new ArrayBuffer[ArrayBuffer[String]]()
    var i = 0
    var startIdx = 0
    var quoted = false
    var wasQuoted = false
    val textLen = cleanText.length()
    var row:ArrayBuffer[String] = new ArrayBuffer[String]()
    var cell:Option[String] = None
    while(i < textLen) {
      val c = cleanText(i)
      if (quoted) {
        if (c == '\"') {
          if (i + 1 == textLen) {
            //this is the end of the file and the row
            row.append(cleanText.substring(startIdx, i))
            rows.append(row)
          } else if (cleanText(i + 1) != '"'){
            quoted = false;
          } else {
            i += 1
          }
        }
      } else {
        if (c == newline.charAt(0)) {
          if (wasQuoted) {
            row.append(cleanText.substring(startIdx, i - 1))
            wasQuoted = false
          } else {
            row.append(cleanText.substring(startIdx, i))
          }
          rows.append(row);
          row = new ArrayBuffer[String]()
          startIdx = i + 1
        } else if (c == sep.charAt(0)) {
          if (wasQuoted) {
            row.append(cleanText.substring(startIdx, i - 1))
            wasQuoted = false
          } else {
            row.append(cleanText.substring(startIdx, i))
          }
          startIdx = i + 1
        } else {
          if (wasQuoted) {
              throw new IllegalArgumentException("Invalid CSV")
          }
          if (c == '\"') {
            if (startIdx != i) {
              throw new IllegalArgumentException("Invalid CSV")
            }
            quoted = true
            wasQuoted = true
            startIdx += 1
          }
        }
      }
      i+=1
    }
    if (quoted) {
      throw new IllegalArgumentException("Invalid CSV")
    } else if (i > startIdx) {
      if (wasQuoted) {
        row.append(cleanText.substring(startIdx, i - 1))
      } else {
        row.append(cleanText.substring(startIdx, i))
      }
      rows.append(row)
    }
    if (rows.length == 0) {
      throw new IllegalArgumentException("Invalid CSV")
    }
    
    val decoded = new CSV(rows(0).toArray.map(unescapeAsNeeded))
    rows.slice(1, rows.size).foreach { x =>
      decoded.addRow(x.toArray.map(unescapeAsNeeded))
    }
    decoded
  }
  def load(filePath:String, sep:String = ",", newline:String = "\n"): CSV = {
    decode(FileUtils.openUTF8Text(filePath), sep, newline)
  }
  
  private def joinAndEscape(input:Array[String],  sep:String = ",", newline:String = "\n"):String = {
    input.map(x => escapeAsNeeded(x, sep, newline)).reduce((a, b) => a + sep + b)
  }
  
  private def escapeAsNeeded(input:String, sep:String = ",", newline:String = "\n"):String = {
    if (input.contains(newline) || input.contains(sep) || input.contains('\"')) {
      "\"" + input.replace("\"", "\"\"") + "\""
    } else {
      input
    }
  }
  
  private def unescapeAsNeeded(input:String):String = input.replace("\"\"", "\"")
  
}