
class Interpreter(source: String) {
    private val parser = new OsirisParser();
    val program = parser.parseProgram(source);
    
    def prompt(input: String): Either[String, String] = {
        if (program.isDefined) {
            parser.runMaybe(parser.action_statements, input) match {
                case None => Left((s"Unrecognized input: ${input}"))
                case Some(Affirm(fact)) => Right(s"Positive ${fact}")
                case Some(Negate(fact)) => Right(s"Negative ${fact}")
            }
        } else {
            Left(s"Program not valid.")
        }
    }
}