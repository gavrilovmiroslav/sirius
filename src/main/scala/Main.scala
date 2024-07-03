import scala.io.Source

object SetCypher extends CypherScript {
    object SetCypherParser extends OsirisParser {
        def set = fact ~ "IF" ~ rep1sep(condition, "OR") ^^ {
            case f ~ _ ~ conds => (f, conds)
        }

        def apply(s: String) = 
            parse(set, s) match {
                case Success(result, _) => Some(result)
                case _ => None
            }
    }

    def checkForUnbounds(fact: Fact, conditions: Seq[Condition]): Set[String] = {
        var boundVars = fact.arguments.collect { case f: Variable => f }.map(_.name).toSet
        var conditionVars = conditions.collect { case f: FactCheck => f }.flatMap { _.fact.arguments }
                            .collect { case f: Variable => f }.map(_.name).toSet
        conditionVars diff boundVars
    }

    def rewrite(code: String): Seq[OsirisStatement] = {
        SetCypherParser(code) match {
            case Some((fact, conditions)) => {
                var diff = checkForUnbounds(fact, conditions)
                if (diff.isEmpty) {
                    var queryName = s"QRY_${fact.name}"
                    Interpreter.CURRENT.remappings += (fact.name -> queryName)
                    conditions.map(cond => Query(Fact(queryName, fact.arguments), Seq(cond), Seq(OsirisGrammar.noop)))
                } else {
                    println(s"\nERROR: New unbound variables introduced: ${diff}! Quitting!\n")
                    Seq()
                }
            }
            case None => Seq()
        }
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length < 1) {
            println("\tError: Missing script location");
            return;
        }

        val cyphers = Map(
            ("SET", SetCypher)
        )

        val source = args.map(file => Source.fromFile(file).getLines.mkString).mkString("\n\n");
        val interp = new Interpreter(source, cyphers);
        interp.result.foreach { println }
    }
}

