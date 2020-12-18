import akka.actor.{Actor, ActorSystem, Props}
import scala.collection.mutable.Map
import scala.io.Source

// actor class for counting characters in a string
// receives a string message and returns a message to the sender with a map of all the characters and the number of occurences
class CounterActor() extends Actor{
  override def receive : Receive = {
    case s: String => {
      var map: Map[Char,Int] = collection.mutable.Map[Char,Int]()
      for (c: Char <- s) {
        if (c.isLetter) {

          var letter: Char = c
          if (letter.isUpper)
            letter = letter.toLower
          if (map.contains(letter))
            map(letter) += 1
          else
            map(letter) = 1
        }
      }
      sender() ! map
      context.stop(self)
    }
  }
}

// actor class for reading a file, allocating actors to count the characters and merging the maps produced by each child actor
// receives string message with filename to read
// receives map  message and merges this with its "master" map, if a map is received for every line prints the map and terminates the actor system
class ControllerActor extends Actor{
  var numLines: Int= 0
  var allLinesSent: Boolean = false
  var numResponses: Int= 0
  var charMap: Map[Char,Int] = collection.mutable.Map[Char,Int]()

  override def receive : Receive = {
    case s: String => {
      val lines = Source.fromFile(s).getLines.toList
      for (line <- lines) {
        var act = context.actorOf(Props[CounterActor])
        act ! line
        numLines += 1
      }
      allLinesSent = true
    }
    case m: Map[Char,Int] => {
      for (key <- m.keySet) {
        if (charMap.contains(key))
          charMap(key) += m.getOrElse(key,0)
        else
          charMap(key) = m.getOrElse(key,0)
      }
      numResponses += 1
      if (numResponses == numLines && allLinesSent) {
        println(charMap)
        context.system.terminate()
      }
    }
  }
}

// main function, starts the actor system and sends the filename to the controller actor
object CharCounter {
  def main(args: Array[String]) = {
    var sys = ActorSystem("system")
    var actor = sys.actorOf(Props[ControllerActor])
    actor ! "file.txt"

  }
}