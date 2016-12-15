package Bot
//clientid: doagwgbcqub6tyno2jjhh5nfey59t9
import org.pircbotx.{Configuration, PircBotX}
import org.pircbotx.hooks.ListenerAdapter
import org.pircbotx.hooks.types.GenericMessageEvent
import Commands.Generics
import Speedrun.SpeedCommands

class Listener extends ListenerAdapter {
  // calls different methods based on the contents of a message
  // an empty string will not be sent later
  def getResponse(contents: String): String = {
    val command = contents.split(" ")(0)
    val response = command match {
      case "!hello" => "Heyo"
      case "!poke" => Generics.poke
      case "!search" => SpeedCommands.search(contents)
      case "!categories" => SpeedCommands.categories(contents)
      case "!wr" => SpeedCommands.time(contents, 1)
      case s if s.matches("""!\d+[nsrt][tdh]|!-\d+[nsrt][tdh]""") => {
        SpeedCommands.fetchPlace(contents)
      } // i.e. !1st || !-1st
      //case s if s.matches("""(!\w+)""") => "that was a command"
      case other => ""
    }
    response
  }

  // uses getResponse to generate a string to respond with
  override def onGenericMessage(event: GenericMessageEvent): Unit = {
    val contents = event.getMessage
    val response = getResponse(contents)
    event.respondWith(response)
  }
}


object Main extends App {
  val config = new Configuration.Builder()
    .setAutoNickChange(false)
    .setOnJoinWhoEnabled(false)
    .setCapEnabled(true)

    .addServer("irc.twitch.tv")
    .setName(name)
    .setServerPassword(password)
    .addAutoJoinChannel("#cronokirby")
    .addListener(new Listener())
    .buildConfiguration()
  val bot = new PircBotX(config)
  bot.startBot()
}
