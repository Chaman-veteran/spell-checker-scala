import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}
import com.fasterxml.jackson.databind.json.JsonMapper
import java.nio.file.{Files, Paths}

import Data.WordTree.*


@main def main() : Unit =
    val src = Files.readAllBytes(Paths.get("SerializedStatistics/result")).toString()
    val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build() :: ClassTagExtensions
    // val myMap = mapper.readValue[Map[String, List[String]]](src)
    print(src)
