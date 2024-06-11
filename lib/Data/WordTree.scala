// package Data.WordTree

import collection.mutable.Map
import scala.reflect.Selectable.reflectiveSelectable


/** Data linked to a word  */
class WordProperties(val frequency : Int, val info : List[String]):
end WordProperties

/** We represent non-existent words with nullProperties */
var nullProperties = WordProperties(0, List())

/** The tree needs words along with their frequency and
  * informations. We also need for future process.
  * The trees take CountedWords as inputs and we give CountedWords
  * as output when giving similar words
  */
class CountedWord(val word : String, val freqNInfo : WordProperties):
  def tail() =
    CountedWord(word.tail, freqNInfo)
end CountedWord

/** Words are spelled out on each branches of the tree where
  * nodes represent ends of (maybe non-existent) words
  *
  * @param properties of nul frequency if the word doesn't exists)$
  * @param branches to each valid following leter is associated a new tree
  */
class WordTree(private var properties : WordProperties, branches : Map[Char, WordTree]):
  /** Predicate to know if a word exist in the tree */
  def exists(character : String) : Boolean =
    branches.get(character.head) match
      case None => !character.nonEmpty
      case Some(followedUp) => followedUp.exists(character.tail)

  /** Return the properties of a given word if it exists, the null word otherwise */
  def propertiesOf(character : String) : WordProperties =
    branches.get(character.head) match
      case None => if character.nonEmpty then properties else nullProperties
      case Some(followedUp) => followedUp.propertiesOf(character.tail)
  
  /** Insertion of a word in a tree
    *
    * @param cword
    */
  def insert(cword : CountedWord) : WordTree =
    cword.word.isEmpty() match
      case true => properties = cword.freqNInfo
      case false => (if !branches.contains(cword.word.head) then branches(cword.word.head) = nullTree)
                    branches(cword.word.head).insert(cword.tail())
    this

  /** Gives similar words from a suffixe as CountedWord.
    *
    * @param d, the maximal (Hamming) distance of a word to be considered as similar 
    * @param prefixe, the actual prefixe (initialized as "")
    * @param typed, the typed word
    * @return A list of neighbour words
    */
  def getSimilarWord(d : Int, prefixe : String, typed : String) : List[CountedWord] =
    var wordProperties = this.propertiesOf(typed)
    if (typed.isEmpty() || d == 0) && wordProperties.frequency != 0 then
      List(CountedWord(prefixe ++ typed, wordProperties))
    else
      var w = typed.head; var ws = typed.tail
      var searchSimilarWords =
        ((l : Char, subTree : WordTree) =>
          if l != w then subTree.getSimilarWord(d - 1, prefixe :+ l, ws)
          else subTree.getSimilarWord(d, prefixe :+ w, ws)).tupled
      branches.foldRight(List())((forkedWord, acc) => searchSimilarWords(forkedWord) ++ acc)
  
end WordTree

/** We represent the tree associated to the void dictionary with nullTree */
var nullTree = WordTree(nullProperties, Map())

/** Insertion of  a list of words in a tree */
def listToTree(l : List[CountedWord]) : WordTree =
  l.foldRight(nullTree)((cword, tree) => tree.insert(cword))


def tupleToWordProperties =
  ((freq: Int, info : List[String]) => WordProperties(freq, info)).tupled

def tupleToCountedWord =
  ((key : String, wordProperties : (Int, List[String])) =>
    CountedWord(key, tupleToWordProperties(wordProperties))).tupled

/** Transform a map of CountedWords in a Tree */
def mapToTree(m : Map[String, (Int, List[String])]) : WordTree =
  m.foldRight(nullTree)((mapEntry, tree) => tree.insert(tupleToCountedWord(mapEntry)))

@main def hellow() =
  var w = WordProperties(2, List())
  var a = ""
