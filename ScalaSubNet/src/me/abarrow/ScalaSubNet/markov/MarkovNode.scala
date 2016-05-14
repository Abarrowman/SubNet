package me.abarrow.ScalaSubNet.markov

import scala.util.Random

class MarkovNode(val word: String, val total: Int, val neighbours: List[(String, Int)]) {
  def next(r: Random): Option[String] = {
    if (total == 0) {
      return None
    }
    val cutOff = r.nextInt(total)
    Some(neighbours.find(p => p._2 > cutOff).get._1);
  }
}

class MarkovNodeBuilder(val word: String) {
  var neighbours = collection.mutable.Map[MarkovNodeBuilder, Int]()
  var total = 0
  def add(other: MarkovNodeBuilder): Unit = {
    neighbours(other) = neighbours.getOrElse(other, 0) + 1
    total += 1
  }

}

object MarkovNode {
  implicit def build(builder: MarkovNodeBuilder): MarkovNode = {
    builder.neighbours.mapValues { x => x / (builder.total).toDouble }
    var cumTotal = 0
    val neighbours = builder.neighbours.toList.map { f =>
      cumTotal += f._2
      (f._1.word, cumTotal)
    }
    /*neighbours.foreach{ f => {
      println(builder.word + " -> " + f._1 + " " + f._2)
    }}*/
    new MarkovNode(builder.word, builder.total, neighbours)
  }
}