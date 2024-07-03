
trait CypherScript {
    def rewrite(code: String): Seq[OsirisStatement]
}

object Interpreter {
    var CURRENT: Interpreter = null
}

class Interpreter(source: String, cyphers: Map[String, CypherScript]) {
    val remappings = scala.collection.mutable.Map[String, String]()
    private val parser = new OsirisParser();

    Interpreter.CURRENT = this

    val result: Seq[OsirisStatement] = parser.parseProgram(source) match {
        case Some(program) => {
            program.flatMap { _ match {
                case Cypher(name, code) if cyphers.contains(name) => cyphers(name).rewrite(code)

                case Cypher(name, code) => throw new Exception(s"Cypher ${name} not found.")

                case IfThen(conds, actions) => {
                    val newConds = conds.map {
                        case FactCheck(Fact(name, arguments)) if remappings.contains(name) => FactCheck(Fact(remappings(name), arguments))
                        case other => other
                    }

                    val newActions = actions.map {
                        case Affirm(Fact(name, arguments)) if remappings.contains(name) => Affirm(Fact(remappings(name), arguments))
                        case Negate(Fact(name, arguments)) if remappings.contains(name) => Negate(Fact(remappings(name), arguments))
                        case other => other
                    }

                    Seq(IfThen(newConds, newActions))
                }

                case Query(fact, conds, actions) => {
                    val newConds = conds.map {
                        case FactCheck(Fact(name, arguments)) if remappings.contains(name) => FactCheck(Fact(remappings(name), arguments))
                        case other => other
                    }

                    val newActions = actions.map {
                        case Affirm(Fact(name, arguments)) if remappings.contains(name) => Affirm(Fact(remappings(name), arguments))
                        case Negate(Fact(name, arguments)) if remappings.contains(name) => Negate(Fact(remappings(name), arguments))
                        case other => other
                    }

                    Seq(Query(fact, newConds, newActions))
                }

                case Proc(fact, conds, actions) => {
                    val newConds = conds.map {
                        case FactCheck(Fact(name, arguments)) if remappings.contains(name) => FactCheck(Fact(remappings(name), arguments))
                        case other => other
                    }

                    val newActions = actions.map {
                        case Affirm(Fact(name, arguments)) if remappings.contains(name) => Affirm(Fact(remappings(name), arguments))
                        case Negate(Fact(name, arguments)) if remappings.contains(name) => Negate(Fact(remappings(name), arguments))
                        case other => other
                    }

                    Seq(Proc(fact, newConds, newActions))
                }
            } }
        }

        case None => throw new Exception("Program not right")
    }
}