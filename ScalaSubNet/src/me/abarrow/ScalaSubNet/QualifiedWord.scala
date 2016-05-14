

package me.abarrow.ScalaSubNet

class QualifiedWord(val rawChars: String, val word: String, val hasPunct: Boolean,
    val endsSentence: Boolean, val punct: Option[Char], val isCapital: Boolean,
    val possessive: Boolean) {
}

object QualifiedWord {
  val endingPunct = Set('.', '!', '?')
  val nonEndingPunct = Set(',', ';', ':')
  val keepPunc = Set('\'', '-', ''')
  val allPunc = endingPunct ++ nonEndingPunct ++ keepPunc
  val capitalLets = Set('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  val lowerLets = capitalLets.map { x => x.toLower }
  val lets = capitalLets ++ lowerLets
  val filterChars = lets ++ allPunc
  val keepChars = lets ++ keepPunc

  def apply(raw: String): QualifiedWord = {
    val chars = raw.replaceAll("’", "'").filter { x => filterChars(x) }
    if (chars.length() == 0) {
      return null
    }
    val lastChar = chars.last
    val endsSentence = endingPunct(lastChar)
    val hasPunct = allPunc(lastChar)
    val punct: Option[Char] = if (hasPunct) Option(lastChar) else None;
    val punctStripped = chars.filter({ x => keepChars(x) })
    if (punctStripped.length() == 0) {
      return null
    }
    val isCapital = capitalLets(punctStripped.charAt(0))
    val word = punctStripped.toLowerCase()
    var finalWord = word
    var possessive = false
    if (word.endsWith("'s")) {
      possessive = true
      finalWord = word.substring(0, word.length() - 2)
    } else if (word.endsWith("'")) {
      possessive = true
      finalWord = word.substring(0, word.length() - 1)
    }
    if (finalWord.length() == 0) {
      return null
    }
    new QualifiedWord(chars, finalWord, hasPunct, endsSentence, punct, isCapital, possessive)
  }
}