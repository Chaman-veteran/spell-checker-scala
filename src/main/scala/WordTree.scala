package Data.WordTree

import collection.mutable.Map
import scala.reflect.Selectable.reflectiveSelectable
import com.fasterxml.jackson.module.scala.deser.overrides


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
  def tail =
    CountedWord(word.tail, freqNInfo)
  
  override def toString() : String = this.word
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
    character match
      case "" => this.properties.frequency > 0
      case _ => branches.get(character.head) match
                  case None => !character.nonEmpty
                  case Some(followedUp) => followedUp.exists(character.tail)

  /** Return the properties of a given word if it exists, the null word otherwise */
  def propertiesOf(character : String) : WordProperties =
    character match
      case "" => properties
      case _ => branches.get(character.head) match
                case None => nullProperties
                case Some(followedUp) => followedUp.propertiesOf(character.tail)
  
  /** Insertion of a word in a tree
    *
    * @param cword
    */
  def insert(cword : CountedWord) : WordTree =
    cword.word.isEmpty() match
      case true => properties = cword.freqNInfo
      case false => (if !branches.contains(cword.word.head) then branches(cword.word.head) = nullTree)
                    branches(cword.word.head).insert(cword.tail)
    this

  /** Gives similar words from a suffixe as CountedWord.
    *
    * @param d, the maximal (Hamming) distance of a word to be considered as similar 
    * @param prefixe, the actual prefixe (initialized as "")
    * @param typed, the typed word
    * @return A list of neighbour words
    */
  def getSimilarSuffixes(d : Int, prefixe : String, typed : String) : List[CountedWord] =
    var wordProperties = this.propertiesOf(typed)
    if (typed.isEmpty() || d == 0) && wordProperties.frequency != 0 then
      List(CountedWord(prefixe ++ typed, wordProperties))
    else
      var w = typed.head; var ws = typed.tail
      var searchSimilarWords =
        ((l : Char, subTree : WordTree) =>
          if l != w then subTree.getSimilarSuffixes(d - 1, prefixe :+ l, ws)
          else subTree.getSimilarSuffixes(d, prefixe :+ w, ws)).tupled
      branches.foldRight(List())((forkedWord, acc) => searchSimilarWords(forkedWord) ++ acc)
  
  /** Gives similar words of a given one as a list of CountedWords */
  def getSimilarWords(d : Int, typed : String) : List[CountedWord] =
    this.getSimilarSuffixes(d, "", typed)

  /** Gives the tree associated to a prefix (i.e. the tree of possible suffixes) */
  def possibleSuffixes(prefixe : String) : WordTree =
    if prefixe == "" then this
    else branches.get(prefixe.head) match
        case None => nullTree
        case Some(subTree) => subTree.possibleSuffixes(prefixe.tail)
  
  /** Gives all words made out of possible suffixes obtained by visiting the actual subtree */
  def nextPossibilities(prefixe : String) : List[CountedWord] =
    def getPossibilities(char : Char, subTree : WordTree) : List[CountedWord] =
      subTree.nextPossibilities(prefixe :+ char).filter((c => c.freqNInfo.frequency != 0))
    branches.foldRight(List(CountedWord(prefixe, properties)))
                      ((cword, accum) => getPossibilities.tupled(cword) ++ accum)
  
  /** Gives all possible suffixes to complete a word */
  def giveSuffixe(prefixe : String) : List[CountedWord] =
    this.possibleSuffixes(prefixe).nextPossibilities(prefixe)
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
def mapToTree(m : scala.collection.immutable.Map[String, (Int, List[String])]) : WordTree =
  m.foldRight(nullTree)((mapEntry, tree) => tree.insert(tupleToCountedWord(mapEntry)))
