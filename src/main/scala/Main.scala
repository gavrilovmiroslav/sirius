import scala.io.Source

object Main extends OsirisConsole {
    commands += "exit" -> OsirisExitCommand;
    commands += "list" -> OsirisListCommand(this);

    def main(args: Array[String]): Unit = {
        if (args.length < 1) {
            println("\tError: Missing script location");
            return;
        }

        val source = args.map(file => Source.fromFile(file).getLines.mkString).mkString("\n\n");
        val interp = new Interpreter(source);

        repl(interp);
    }

    def repl(interp: Interpreter): Option[Interpreter] = {
        print("OSR> ");
        val input = scala.io.StdIn.readLine();

        if (commands.contains(input.toLowerCase())) {
            return commands(input.toLowerCase())(interp);
        }

        interp.prompt(input) match {
            case Right(value) => println(s"\tOK! ${value}");
            case Left(value) => println(s"\tError: ${value}");
        }

        repl(interp)
    }
}

