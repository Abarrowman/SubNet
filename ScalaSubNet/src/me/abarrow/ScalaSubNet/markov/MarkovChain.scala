package me.abarrow.ScalaSubNet.markov
import scala.util.Random

class MarkovChain(val nodes: Map[String, MarkovNode]) {
  def getNode(word: String): Option[MarkovNode] = {
    nodes get word
  }
  def nextNode(current: MarkovNode, rand: Random = new Random()): Option[MarkovNode] = {
    val nextWord = current.next(rand)
    if (nextWord.isEmpty) None else getNode(nextWord.get)
  }
  def produce(wordCount: Int, rand: Random = new Random()): String = {
    val nodeArray = nodes.toArray
    var node = Option(nodeArray(rand.nextInt(nodeArray.length))._2)
    var count = 0
    var output = ""
    while (node.isDefined && (count < wordCount)) {
      output += node.get.word + " "
      node = nextNode(node.get, rand)
      count += 1
    }
    return output
  }

}

object MarkovChain {
  def build(words: Array[String]): MarkovChain = {
    var markovBuilderNodes = collection.mutable.Map[String, MarkovNodeBuilder]()
    words.reduce { (x, y) =>
      var wordX = markovBuilderNodes.getOrElseUpdate(x, new MarkovNodeBuilder(x))
      var wordY = markovBuilderNodes.getOrElseUpdate(y, new MarkovNodeBuilder(y))
      wordX.add(wordY)
      y
    }

    val markovNodes: Map[String, MarkovNode] = collection.immutable.Map() ++
      markovBuilderNodes.map(f => (f._1, MarkovNode.build(f._2)))

    new MarkovChain(markovNodes)
  }
}