package StatsFromTxt

import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}
import com.fasterxml.jackson.databind.json.JsonMapper
import java.nio.file.{Files, Paths, Path}
import scala.collection.JavaConverters._
import scala.collection.mutable.{Map}

val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build() :: ClassTagExtensions

def insertDown[T](elem : (Int, T), list : List[(Int, T)])
                        : List[(Int, T)] =
    if (elem(0) > list.head(0)) then list.prepended(elem)
    else insertDown(elem, list.tail)
    list

def fromOption[B](opt : Option[B], b : B): B = opt match
  case None => b
  case Some(ob) => ob

/**
  * Merge two same words by suming the frequencies and following words 
  * to the ones already recorded 
  *
  * @param newWord
  * @param recordedWords
  * @return
  */
def addValue(newWord : String,
            recordedWords : (Int, List[(Int, String)]))
            : (Int, List[(Int, String)]) =
  val nbSeen = 1 + (recordedWords._2.find((_, word) => word == newWord) match
                      case None => 0
                      case Some((encountered, _)) => encountered)
  //^^^ nbSeen is the number of occurences of nextWord following the current word
  (recordedWords._1 + 1, insertDown((nbSeen, newWord), recordedWords._2))

class Dictionary(var language : String):
  type V = (Int, List[(Int, String)])
  type K = String
  var map : Map[K, V] = Map.empty

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

  /**
    * Map a word to his frequence and the next words with the probabilities associated
    *
    * @param wordList
    */
  def getFreqnNext(wordList : List[String]) : Unit =
    wordList match
      case Nil => ()
      case w :: ws =>
        this.insertWithAddValue(w, fromOption(ws.headOption, ""))
        this.getFreqnNext(ws)
  
  /**
    * Function to get the following words in sorted order
    *
    * @return
    */
  def getResultingMap() : Map[K, (Int, List[String])] = 
    map.map((k, v) => (k, (v._1, v._2.take(3).map((_, s) => s))))

  /**
    * Fetch statistics as a Map object
    *
    * @return
    */
  def getStatsFromFile() : Map[K, (Int, List[String])] =
    this.getFreqnNext(this.getFiles())
    this.getResultingMap()
  
  /**
    * Serialize the map associating words to their properties in a file
    */
  def serializeMap() : Unit = 
    val computedMap = this.getStatsFromFile()
    val resultFile = Paths.get("SerializedStatistics/result").toFile()
    mapper.writeValue(resultFile, computedMap)

end Dictionary
