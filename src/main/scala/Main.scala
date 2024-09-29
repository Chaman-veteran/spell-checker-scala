import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}
import com.fasterxml.jackson.databind.json.JsonMapper
import java.nio.file.{Files, Paths}
import io.StdIn.{readChar}

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
def getWord() : Query = readChar() match
    case '\t' => Complete("")
    case c if spaces.contains(c) => Correct("") 
    case c => getWord().fmap(_.prepended(c))

// def prompt(tree : WordTree) : Unit = while true do
//     print("> ")
//     scala.Console.flush()
//     getWord() match
//         case Complete(word) => println()
//                                 print(tree.)
// TODO: SpellCheckerInterface


@main def main() : Unit =
    val src = Files.readString(Paths.get("SerializedStatistics/result"))
    val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build() :: ClassTagExtensions
    println(keyboardSrcEn)
    // val myMap = mapper.readValue[Map[String, List[String]]](src)
    // print(src)
