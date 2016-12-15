package Speedrun

import org.json4s._
import org.json4s.jackson.JsonMethods._

object SpeedCommands {

  def search(contents: String): String = {
    val gameName = contents.split(" ").tail.map(_.capitalize).mkString(" ")
    val game = API.getGame(gameName)
    s"The abbreviation for $gameName is '${game.abbreviation}'"
  }

  def categories(contents: String): String = {
    var gameName: String = contents.split(" ", 2)(1)
    val id = {
      if (gameName.contains('"')){
        gameName = gameName.replace("\"", "").split(" ").map(_.capitalize)
          .mkString(" ")
        API.getGame(gameName).id
      } else{ gameName }
    }
    val categories = API.getCategories(id)
    val names = categories.map(_.name).mkString(" | ")
    s"Here's a list of categories for ${gameName.replace("\"", "")} : $names"
  }

  // !wr "Chrono Trigger" Any%
  def time(contents: String, place: Int): String ={
    // returns gameID, catName
    def parseArgs: (String, String) = {
      if (contents.contains('"')) {
        val args = contents.split("\"", 3)
        val id = API.getGame(args(1)).id
        // removing the front space
        val catName = args(2).tail
        (id, catName)
      } else {
        val args = contents.split(" ", 3)
        val id = args(1)
        val catName = args(2)
        (id, catName)
      }
    }
    val (id, catName) = parseArgs
    val run = API.getRun(id, catName, place)
    def runToMSG(r: Run): String = {
      val names = r.getPlayers.mkString(" & ")
      val time = r.times.getTime
      s"the WR is $time by $names"
    }
    val response = {
    if (run == None) "That category doesn't seem to exist"
    else runToMSG(run.get)
    }
    response
  }

  def fetchPlace(contents: String) = {
    val placeStr = contents.split(" ")(0)
    val placeNum = placeStr.slice(1, placeStr.length - 2).toInt
    time(contents, placeNum)
  }

}
