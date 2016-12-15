package Speedrun

import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class Link(rel: String, uri: String)

case class Category(id: String, name: String, weblink: String,
                    `type`: String, rules: String, players: Map[String, Any],
                    miscellaneous: Boolean, links: List[Link]){

  def getLeaderboard(gameid: String): String = {
    s"http://www.speedrun.com/api/v1/leaderboards/$gameid/category/$id"
  }
}

case class ReducedCategory(id: String, name: String,
                           variable: Option[Map[String, String]],
                           leaderboard: String)

case class CategoryData(data: List[Category])

//values: {zdbx1h88: {label: 150cc}}
case class Values(values: Map[String, Map[String, Any]])

case class Choice(value: String, label: String, parent: String)

case class Variable(id: String, name: String, category: Option[String],
                    scope: Map[String, String], mandatory: Boolean,
                    `user-defined`: Boolean, obsoletes: Boolean,
                    values: Values, default: Option[String],
                    `is-subcategory`: Boolean, links: List[Link])

case class VariableData(data: List[Variable]) {
  // returns true if a category has subcategory variables
  def categoryIncluded(catID: String): Boolean = {
      val subCats = data.filter(_.`is-subcategory`)
      val validCats = subCats.map(_.category.get)
      validCats.contains(catID)
    }

  def getChoices(catID: String): List[Choice] = {
    val subCats = data.filter(_.`is-subcategory`)
    val catVariables = subCats.filter(_.category.get == catID)
    val values = catVariables.map(_.values.values)
    // id -> Map[stuff] becomes id -> choice(id, label)
    val choiceMap = values.map(_.transform(
      (k, v) => Choice(k, v("label").toString, catVariables(0).id)))
    val choices = choiceMap(0).values.toList
    choices
  }
}

case class Game(id: String, names: Map[String, String],
                abbreviation: String,
                weblink: String, released: Int, `release-date`: String,
                ruleset: Map[String, Any], romhack: Boolean,
                gametypes: List[Any], platforms: List[String],
                regions: List[String], genres: List[String],
                engines: List[String], developers: List[String],
                moderators: Map[String, String], created: String,
                assets: Map[String, Any], links: List[Link])

case class GameData(data: List[Game], pagination: Map[String, Any])

case class UserData(id: String, names: Map[String, Option[String]],
                    weblink: String, `name-style`: Map[String, Any],
                    role: String, signup: String, location: Map[String, Any],
                    regions: Any, twitch: Option[Map[String, Any]],
                    hitbox: Option[Map[String, Any]],
                    youtube: Option[Map[String, Any]],
                    speedrunslive: Option[Map[String, Any]],
                    links: List[Link])

case class Time(primary: String, primary_t: Int, realtime: String,
                realtime_t: Int, realtime_noloads: String,
                realtime_noloads_t: Int, ingame: Option[String],
                ingame_t: Int) {
  // returns a formatted string with the time
  def getTime: String = {
    val seconds = primary_t % 60
    val fullminutes = primary_t / 60
    val hours = fullminutes / 60
    val minutes = fullminutes % 60
    s"$hours:$minutes:$seconds"
  }
}
case class Run(id: String, weblink: String, game: String,
               level: Option[String], category: String,
               videos: Option[Map[String, Any]], comment: String,
               status: Map[String, String],
               players: List[Map[String, String]],
               date: String, submitted: Option[String],
               times: Time, system: Map[String, Any],
               splits: Option[Any],values: Map[String, String],
               links: List[Link]){

  def hasVariable(variable: Option[Map[String, String]]): Boolean = {
    val parent = variable.get("parent")
    val value = variable.get("value")
    val b = (values.get(parent).get == value)
    b
  }

  def getPlayers: List[String] = {
    def getName(player: Map[String, String]): String = {
      if (player("rel") == "guest") player("name")
      else {
        implicit val formats = DefaultFormats
        val url = player("uri")
        val json = parse(Source.fromURL(url).mkString)
          .extract[Map[String, UserData]]
        val userData = json("data")
        userData.names("international").get
      }
    }
    players.map(getName)
  }
}
// place starts at 1
case class RunData(place: Int, run: Run)

case class Leaderboard(weblink: String, game: String, category: String,
                       level: Option[String], platform: Option[String],
                       region: Option[String], emulators: Option[Boolean],
                       `video-only`: Boolean, timing: String,
                       values: Map[String, Any], runs: List[RunData],
                       links: List[Link])

case class LBData(data: Leaderboard)

object API extends App {

  // gets a Game object from a full game name: "Chrono Trigger"
  def getGame(name: String): Game = {
    implicit val formats = DefaultFormats
    val append = name.split(" ").mkString("%20")
    val url = "http://www.speedrun.com/api/v1/games?name=" + append
    val gameData = parse(Source.fromURL(url).mkString).extract[GameData]
    gameData.data(0)
  }

  def getVariables(gameid: String): VariableData = {
    implicit val formats = DefaultFormats
    val url = s"http://www.speedrun.com/api/v1/games/$gameid/variables"
    parse(Source.fromURL(url).mkString).extract[VariableData]
  }

  def getCategories(gameid: String): List[ReducedCategory] = {
    implicit val formats = DefaultFormats
    val catURL = s"http://www.speedrun.com/api/v1/games/$gameid/categories"
    val json = parse(Source.fromURL(catURL).mkString).extract[CategoryData]
    val categories = json.data
    val variableData = getVariables(gameid)
    // has to be a list, because subcategories create multiple
    // ReducedCategories from just one object
    def reduceCategory(cat: Category): List[ReducedCategory] = {
      val lb = cat.getLeaderboard(gameid)
      if (variableData.categoryIncluded(cat.id)) {
        val choices = variableData.getChoices(cat.id)
        def choicetoReducedCat(c: Choice): ReducedCategory = {
          val name = cat.name + " - " + c.label
          val map = Map("parent" -> c.parent, "value" -> c.value)
          ReducedCategory(cat.id, name, Some(map), lb)
        }
        choices.map(choicetoReducedCat)
      } else { List(ReducedCategory(cat.id, cat.name, None, lb)) }
    }
    categories.flatMap(reduceCategory)
  }

  def getRun(gameid: String, cName: String, place: Int):Option[Run] = {
    val categories = getCategories(gameid)
    val requestedCat = categories.filter(_.name == cName)
    def fetchRun(category: ReducedCategory, p: Int): Run = {
      implicit val formats = DefaultFormats
      val url = category.leaderboard
      val data = parse(Source.fromURL(url).mkString).extract[LBData]
      val runs = data.data.runs
      val variable = category.variable
      val validRuns = {
        if (variable != None) {
          runs.filter(_.run.hasVariable(variable))
        } else runs
      }
      val runCount = validRuns.length
      val index = {
        if (1 <= p && p <= runCount) p - 1
        else if (p > runCount) runCount - 1
        else if(p == 0) 0
        else if (p < 0 && (runCount + p) >= 0 ) runCount + p
        else 0
      }
      validRuns(index).run
    }
    if (requestedCat.length == 0) None
    else Some(fetchRun(requestedCat(0), place))
  }
}
