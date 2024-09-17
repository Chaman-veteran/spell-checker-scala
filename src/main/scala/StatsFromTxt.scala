package StatsFromTxt

import java.nio.file.{Files, Paths, Path}
import scala.collection.JavaConverters._
import scala.collection.mutable.{Map}

def insertDown[T](elem : (Int, T), list : List[(Int, T)])
                        : List[(Int, T)] =
    if (elem(0) > list.head(0)) then list.prepended(elem)
    else insertDown(elem, list.tail)
    list

def addValue(newWord : String,
            recordedWords : (Int, List[(Int, String)]))
            : (Int, List[(Int, String)]) =
  val nbSeen = 1 + (recordedWords(1).find((_, word) => word == newWord) match
                      case None => 0
                      case Some((encountered, _)) => encountered)
  (recordedWords(0) + 1, insertDown((nbSeen, newWord), recordedWords(1)))

def fromOption[B](opt : Option[B], b : B): B = opt match
  case None => b
  case Some(ob) => ob

class Dictionary(var language : String):
  type V = (Int, List[(Int, String)])
  type K = String
  var map : Map[K, V]= Map.empty

  /** Fetch words from a file */
  def getWords(filePath : Path) : List[String] =
    val src = Files.readString(filePath)
    src.split(" ").toList

  /** Assemble all words from different dictionaries */
  def getFiles() : List[String] =
    val srcs = Files.list(Paths.get("Dictionaries/" + language)).toList
    srcs.asScala.toList.foldLeft(List())((acc, path) => acc ++ getWords(path))

  def insertWithAddValue(k : K, w : String) : Unit =
    val prev_value  = map.getOrElse(k, (1, List()))
    val new_value   = addValue(w, prev_value)
    map.update(k, new_value)

  def getFreqnNext(wordList : List[String]) : Unit =
    wordList match
      case Nil => ()
      case w :: ws =>
        this.insertWithAddValue(w, fromOption(ws.headOption, ""))
        this.getFreqnNext(ws)

end Dictionary
