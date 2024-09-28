package SPI

import Data.WordTree.*


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


// def imap[A, B](l : Array[A]) : (((A, Int)) => B) => Array[B] =
//   l.zip(0 until l.length).map

def mapOption[A, B](f : A => Option[B], l : List[A]) : List[B] =
  l.map(f).foldLeft(List())((acc, elt) =>
                              elt match
                                case None => acc
                                case Some(value) => acc.appended(value)
                            )

/**
  * Used Keyboard, the only one supported for now is QWERTY
  */
type KeyboardT = Array[Array[Char]]
class Keyboard(var keyboard : KeyboardT):
  def getOption(i : (Int, Int)) : Option[Char] =
    if i._1 < keyboard.length && i._2 < keyboard(i._1).length then
      Some(keyboard(i._1)(i._2))
    else
      None

  /**
    * Give the matrix of neighboorhood in place of the character of the keyboard 
    *
    * @return
    */
  // def charsPerimeter : Array[Array[List[Char]]] =
  //   imap(this.keyboard)
  //       ((row, indR) =>
  //         imap(row)((indT, _) =>
  //                     mapOption(this.getOption, nearIndices(indR, indT))
  //                   ))
  
  def associateNearChars(perimeter : Array[Array[List[Char]]]) : List[(Char, List[Char])] =
    this.keyboard.zip(perimeter)
                 .map((rowK, rowP) => rowK.zip(rowP))
                 .foldLeft(List())((acc, array) => acc ++ array.toList)

  // def nearChars : List[(Char, List[Char])] = 
  //   this.associateNearChars(this.charsPerimeter)
end Keyboard

// def strDiff(x : String, y : String) = (x, y) match
//   case (_, "") => x.length()
//   case ("", _) => y.length()
//   case _ if x.head == y.head => strDiff(x.tail, y.tail)
//   case _ => 

/**
  * Complete user's input
  *
  * @param tree
  * @param prefixe
  * @return
  */
def completeWord(tree : WordTree, prefixe : String) : List[CountedWord] =
    tree.giveSuffixe(prefixe).sortBy[Int](_.freqNInfo.frequency).take(10)
