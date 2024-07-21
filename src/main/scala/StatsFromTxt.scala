package StatsFromTxt

import java.nio.file.{Files, Paths, Path}
import scala.collection.JavaConverters._

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
