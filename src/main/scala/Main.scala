import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}
import com.fasterxml.jackson.databind.json.JsonMapper
import java.nio.file.{Files, Paths}
import io.StdIn.{readChar, readByte}

import Data.WordTree.*
import SCI.*

/**
  * The user can either ask for completion or correction
  */
trait Query:
    var word : String
    def fmap(f : String => String) : Query =
        this.word = f(this.word)
        this

case class Complete(var word : String) extends Query
case class Correct(var word : String) extends Query

val spaces = Array(' ', '\n', '\t', '\f', '\r')

/**
  * Give the input of the user
  *
  * @return
  */
def getWord() : Query = 
  try readChar() match
    case '\t' => Complete("")
    case c if spaces.contains(c) => Correct("") 
    case c => getWord().fmap(_.prepended(c))
  catch
    case _ : java.lang.NumberFormatException => Correct("")


/**
  * Prompts a new word as long as the word to correct is not empty
  *
  * @param tree
  */
def prompt(tree : WordTree) : Unit = while true do
    print("> ")
    scala.Console.flush()
    getWord() match
        case Complete(prefixe) => println()
                                  println(completeWord(tree, prefixe))
        case Correct("") => return
        case Correct(word) => println(correctWord(tree, word))

/**
  * Entrypoint into the spell-checker
  */
@main def main() : Unit =
    val inputFreq = Files.readString(Paths.get("SerializedStatistics/result"))
    val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build() :: ClassTagExtensions
    val myMap : Map[String, (Int, List[String])] = mapper.readValue[Map[String, (Int, List[String])]](inputFreq)
    val dictionaryTree = mapToTree(myMap)
    println("Type enter to correct a word or tab to complete it.")
    println("Type a word:")
    prompt(dictionaryTree)
