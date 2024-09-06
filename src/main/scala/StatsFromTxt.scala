package StatsFromTxt

import java.nio.file.{Files, Paths, Path}
import scala.collection.JavaConverters._

def insertDown(elem : Tuple2[Int, String], list : List[Tuple2[Int, String]])
              : List[Tuple2[Int, String]] =
    if (elem(0) > list.head(0)) then list.prepended(elem)
    else insertDown(elem, list.tail)
    list

def addValue(newWord : Tuple2[Int, List[Tuple2[Int, String]]],
            recordedWords : Tuple2[Int, List[Tuple2[Int, String]]])
            : Tuple2[Int, List[Tuple2[Int, String]]] =
  val nbSeen = 1 + (recordedWords(1).find((_, word) => word == newWord(1).head(1)) match
                      case None => 0
                      case Some((encountered, _)) => encountered)
  (recordedWords(0) + newWord(0), insertDown((nbSeen, newWord(1).head(1)), recordedWords(1)))

class Dictionary(var language : String):
  /** Fetch words from a file */
  def getWords(filePath : Path) : List[String] =
    val src = Files.readString(filePath)
    src.split(" ").toList

  /** Assemble all words from different dictionaries */
  def getFiles() : List[String] =
    val srcs = Files.list(Paths.get("Dictionaries/" + language)).toList
    srcs.asScala.toList.foldLeft(List())((acc, path) => acc ++ getWords(path))

end Dictionary
