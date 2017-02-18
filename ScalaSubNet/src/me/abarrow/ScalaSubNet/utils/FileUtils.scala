package me.abarrow.ScalaSubNet.utils

import scala.io.Source
import scala.io.Codec
import java.io.File
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream


object FileUtils {
  
  def createDirsIfNeeded(folder: String) : Unit = {
    val dirFile = new File(folder);
    if (dirFile.exists()) {
      if (!dirFile.isDirectory()) {
        throw new IllegalArgumentException("Supplied folder is in fact a file!")
      }
    } else {
      dirFile.mkdirs();
    }
  }

  def getFilesInFolder(folder: String): Array[String] = {
    restrictToFilesPaths(new File(folder).listFiles())
  }

  def getDeepFiles(path:String): Array[String] = {
    if ( new File(path).isFile()) {
      Array(path)
    } else {
      getDeepFilesInFolder(path)
    }
  }
  
  def getDeepFilesInFolder(folder: String): Array[String] = {
    restrictToFilesPaths(innerGetDeepFilesInFolder(new File(folder)))
  }

  def restrictToFilesPaths(files: Array[File]): Array[String] = {
    files.filter { x => x.isFile() }.map { x => x.getAbsolutePath }
  }

  def innerGetDeepFilesInFolder(folder: File): Array[File] = {
    val children = Option(folder.listFiles()).getOrElse(Array())
    children ++ children.filter { x => x.isDirectory() }.flatMap(innerGetDeepFilesInFolder)
  }

  def splitApartWords(input: String): Array[String] = {
    input.replaceAll("(\\s|-)", " ").split(" ").filter { x => !x.equals("") }
  }

  def innerEvalDeepFilesInFolder[A](folder: File, evaluate: (File) => Option[A],
    evaluation: Option[A] = None): Array[(File, A)] = {
    val children = Option(folder.listFiles()).getOrElse(Array())
    val fileChildren = children.filter { x => x.isFile() }
    val evaledFileChildren: Array[(File, A)] = if (evaluation.isEmpty) {
      fileChildren.map { x => (x, evaluate(x)) }.filter { x => x._2.isDefined }.map { x => (x._1, x._2.get) }
    } else {
      fileChildren.map { x => (x, evaluation.get) }
    }

    evaledFileChildren ++ children.filter { x => x.isDirectory() }.flatMap {
      x => innerEvalDeepFilesInFolder(x, evaluate, if (evaluation.isEmpty) {
        evaluate(x)
      } else {
        evaluation
      })
    }
  }

  def evaluateDeepFilesInFolder[A](folder: String, evaluate: (File) => Option[A]): Array[(String, A)] = {
    val root = new File(folder)
    innerEvalDeepFilesInFolder(root, evaluate, evaluate(root)).map{ x => (x._1.getAbsolutePath, x._2) }
  }

  def openUTF8Text(path: String): String = {
    val src = Source.fromFile(path)(Codec.UTF8)
    val str = src.mkString
    src.close()
    str
  }
  
  def saveUTF8Text(path:String, text:String):Unit = {
    saveUTF8File(new File(path), text)
  }
  
  def saveUTF8File(file:File, text:String):Unit = {
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"))
    try {
      bw.write(text) 
    } finally {
      bw.close()
    }
  }

  def getFileNameAndExtension(path: String): (String, String) = {
    val fileNameExt = path.substring(path.lastIndexOf(File.separatorChar) + 1)
    val lastDot = fileNameExt.lastIndexOf('.')
    if (lastDot != -1) {
      (fileNameExt.substring(0, lastDot), fileNameExt.substring(lastDot + 1))
    } else {
      (fileNameExt, "")
    }
  }
  
}