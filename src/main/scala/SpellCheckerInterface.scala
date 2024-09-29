package SCI

import Data.WordTree.*
import java.nio.file.{Files, Paths}
import scala.math.{min}
import upickle.default._


/**
  * Gives the zone of index of near characters from the index of the one given
  *
  * @param i
  * @param j
  * @return
  */
def nearIndices(i : Int, j : Int) : List[(Int, Int)] =
  for row <- List(i - 1, i, i + 1)
      column <- List(j - 1, j, j + 1)
      if (row, column) != (i, j)
  yield (row, column)


def imap[A, B](l : List[A]) : (((A, Int)) => B) => List[B] =
  l.zip(0 until l.length).map

def mapOption[A, B](f : A => Option[B], l : List[A]) : List[B] =
  l.map(f).foldLeft(List())((acc, elt) =>
                              elt match
                                case None => acc
                                case Some(value) => acc.appended(value)
                            )

/**
  * Used Keyboard, the only one supported for now is QWERTY
  */
type KeyboardT = List[List[Char]]
class Keyboard(var keyboard : KeyboardT):
  var keyboardWithPerimeter : List[(Char, List[Char])] = nearChars

  def getOption(i : (Int, Int)) : Option[Char] =
    if i._1 < keyboard.length && i._1 > 0 && i._2 < keyboard(i._1).length && i._2 > 0 then
      Some(keyboard(i._1)(i._2))
    else
      None

  /**
    * Give the matrix of neighboorhood in place of the character of the keyboard 
    *
    * @return
    */
  def charsPerimeter : List[List[List[Char]]] =
    imap(this.keyboard)
        ((row, indR) =>
          imap(row)((_, indT) =>
                      mapOption(this.getOption, nearIndices(indR, indT))
                    ))
  
  /**
    * Given a keyboard and a perimeter, associate between each characters and his neighbors
    *
    * @param perimeter
    * @return
    */
  def associateNearChars(perimeter : List[List[List[Char]]]) : List[(Char, List[Char])] =
    this.keyboard.zip(perimeter)
                 .map((rowK, rowP) => rowK.zip(rowP))
                 .foldLeft(List())((acc, array) => acc ++ array.toList)

  /**
    * Gives the neighboors of all characters
    *
    * @return
    */
  def nearChars : List[(Char, List[Char])] = 
    this.associateNearChars(this.charsPerimeter)

  /**
    * Returns the perimeter of a character, as per nearChars's definition of "neighbor"
    *
    * @param c
    */
  def inPerimeterOf(c : Char) : List[Char] =
    keyboardWithPerimeter.find(p => p._1 == c) match
      case None => List()
      case Some(v) => v._2
end Keyboard

/**
  * QWERTY Keyboard used to get neighboors leters from the on typed 
  */
val keyboardSrcEn : KeyboardT = ujson.read(Files.readString(Paths.get("layouts/qwerty.json"))).arr.toList
                                     .map(row => row.arr.toList.map(_("label").str.head))
val keyboardEn : Keyboard = Keyboard(keyboardSrcEn)

/**
  * Calcul of the distance between two words (modifed Hamming's distance)
  *
  * @param x
  * @param y
  */
def strDiff(x : String, y : String) : Int = (x, y) match
  case (_, "") => x.length()
  case ("", _) => y.length()
  case _ if x.head == y.head => strDiff(x.tail, y.tail)
  case _ =>
    val perimeterOfy : List[Char]= keyboardEn.inPerimeterOf(y.head)
    val tailDiff : Int = strDiff(x.tail, y.tail)
    val perimeterDiff : Int = tailDiff - (if (perimeterOfy.exists(_ == x.head)) 1 else 0)
    2 + min(perimeterDiff, min(strDiff(x.tail, y), strDiff(x, y.tail)))

/**
  * Complete user's input
  *
  * @param tree
  * @param prefixe
  * @return
  */
def completeWord(tree : WordTree, prefixe : String) : List[CountedWord] =
    tree.giveSuffixe(prefixe).sortBy[Int](_.freqNInfo.frequency).take(10)

def associateDistance(word : String, neighbor : CountedWord) : (Int, CountedWord) =
  (strDiff(word, neighbor.word), neighbor)

/**
  * Correct user's input
  *
  * @param tree
  * @param word
  */
def correctWord(tree : WordTree, word : String) : List[CountedWord] =
  tree.getSimilarWords(2, word)
      .map(associateDistance(word, _)).sortBy(_._1)
      .take(10).map(_._2)
