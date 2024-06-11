import scala.io.StdIn.readLine
import scala.io.Source.fromFile
import upickle.default.read

@main def main() : Unit =
    var inputFreq = fromFile("SerializedStatistics/result")
    println("Hello, World!")
