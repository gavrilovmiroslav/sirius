
trait OsirisConsoleCommand extends (Interpreter => Option[Interpreter]);

trait OsirisConsole {
    val commands = scala.collection.mutable.Map[String, OsirisConsoleCommand]();
    def repl(interp: Interpreter): Option[Interpreter]
}

case object OsirisExitCommand extends OsirisConsoleCommand {
  override def apply(i: Interpreter): Option[Interpreter] = None
}

case class OsirisListCommand(console: OsirisConsole) extends OsirisConsoleCommand {
  override def apply(i: Interpreter): Option[Interpreter] = {
    println()
    i.program.map(println(_))
    println()
    console.repl(i)
  }
}