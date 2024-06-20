
import scala.util.parsing.combinator._
import TypeCasting.isCastable
import TypeCasting.castValue

class OsirisParser extends RegexParsers {
    override def skipWhitespace = true
    override val whiteSpace = "[ \t\r\f\n]+".r

    def raw_word: Parser[String] = """[a-zA-Z0-9_\-]+""".r ^^ { _.toString }
    def raw_digits: Parser[String] = """(0|[1-9]\d*)""".r ^^ { _.toString }

    def variable: Parser[Variable] = """[a-zA-Z0-9_]+""".r ^^ { word =>
        Variable(word.toString, None)
    }

    def guid: Parser[Literal] =
        raw_word.filter(_.length >= 36).flatMap(word => {
        """[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}""".r
            .findFirstMatchIn(word.takeRight(36)).map(a =>
                success(Literal(a.toString, Some(OsirisGUID))))
                    .getOrElse(failure("input done fucked up"))
            })

    def number: Parser[Literal] = ("-"?) ~ raw_digits ^^ { 
        case None ~ value => Literal(value, Some(OsirisInteger64))
        case Some(_) ~ value => Literal(s"-${value}", Some(OsirisInteger64))
    }

    def real: Parser[Literal] = number ~ "." ~ raw_digits ^^ { case a ~ _ ~ b => 
        Literal(s"${a.value}.${b}", Some(OsirisReal))
    }

    def string: Parser[Literal] = """"[^"]*"""".r ^^ { str =>
        Literal(str.substring(1, str.length - 1), Some(OsirisString))
    }

    def typename_string = "STRING" ^^ { _ => OsirisString }
    def typename_real = "REAL" ^^ { _ => OsirisReal }
    def typename_int64 = "INTEGER64" ^^ { _ => OsirisInteger64 }
    def typename_int = "INTEGER" ^^ { _ => OsirisInteger }

    def typename_guid_character = "CHARACTERGUID" ^^ { _ => OsirisGUIDCharacter }
    def typename_guid_item = "ITEMGUID" ^^ { _ => OsirisGUIDItem }
    def typename_guid_trigger = "TRIGGERGUID" ^^ { _ => OsirisGUIDTrigger }
    def typename_guid_spline = "SPLINEGUID" ^^ { _ => OsirisGUIDSpline }
    def typename_guid_level_template = "LEVELTEMPLATEGUID" ^^ { _ => OsirisGUIDLevelTemplate }
    def typename_guid_string = "GUIDSTRING" ^^ { _ => OsirisGUID }

    def typename_guids: Parser[OsirisType] = 
        typename_guid_character | typename_guid_item | typename_guid_trigger |
        typename_guid_spline | typename_guid_level_template | typename_guid_string

    def typename: Parser[OsirisType] = 
        typename_guids | typename_string | typename_real | typename_int64 | typename_int

    def typecast: Parser[Value] = ("(" ~> typename <~ ")") ~ value ^^ {
        case (cast ~ Literal(value, None)) => TypeCast(cast, Literal(value, None))
        case (cast ~ Literal(value, Some(typ))) => TypeCast(cast, Literal(castValue(cast, typ, value), Some(typ)))
        case (cast ~ Variable(name, typ)) => TypeCast(cast, Variable(name, typ))
        case (cast ~ TypeCast(typ, value)) => TypeCast(cast, TypeCast(typ, value))
        case (cast ~ OsirisError(log)) => OsirisError(log :+ "Tried to typecast error expression")
    }

    def literal: Parser[Literal] = 
        guid | real | number | string

    def value: Parser[Value] = 
        typecast | literal | variable

    def eq: Parser[ComparisonOperator] = "==" ^^ { _ => CompEq }
    def ne: Parser[ComparisonOperator] = "!=" ^^ { _ => CompNe }
    def le: Parser[ComparisonOperator] = "<=" ^^ { _ => CompLe }
    def lt: Parser[ComparisonOperator] = "<"  ^^ { _ => CompLt }
    def ge: Parser[ComparisonOperator] = ">=" ^^ { _ => CompGe }
    def gt: Parser[ComparisonOperator] = ">"  ^^ { _ => CompGt }

    def comparison_operator = eq | ne | le | lt | ge | gt
    def comparison = value ~ comparison_operator ~ value ^^ { case lhs ~ op ~ rhs => 
        Comparison(lhs, rhs, op)
    }

    def argument = typecast | value

    def fact = variable ~ "(" ~ repsep(argument, ",") ~ ")" ^^ { case name ~ _ ~ args ~ _ => 
        Fact(name.name, args.toSeq)
    }

    def condition: Parser[Condition] = (comparison | fact) ^^ {
        case f@Fact(_, _) => FactCheck(f)
        case c@Comparison(_, _, _) => c
    }

    def affirm: Parser[OsirisAction] = fact ^^ { case f => Affirm(f) }
    def negate: Parser[OsirisAction] = "NOT" ~> fact ^^ { case f => Negate(f) }
    def action = negate | affirm

    def if_then = "IF" ~> rep1sep(condition, "AND") ~ "THEN" ~ rep1sep(action, ";") <~ ";" ^^ { case conds ~ _ ~ actions =>
        IfThen(conds.toSeq, actions.toSeq)
    }

    def query = "QRY" ~> fact ~ (("AND" ~> rep1sep(condition, "AND"))?) ~ "THEN" ~ rep1sep(action, ";") <~ ";" ^^ { 
        case fact ~ conds ~ _ ~ actions => 
            Query(fact, conds.map(_.toSeq).getOrElse(Seq()), actions.toSeq)
    }

    def proc = "PROC" ~> fact ~ (("AND" ~> rep1sep(condition, "AND"))?) ~ "THEN" ~ rep1sep(action, ";") <~ ";" ^^ { 
        case fact ~ conds ~ _ ~ actions => 
            Proc(fact, conds.map(_.toSeq).getOrElse(Seq()), actions.toSeq)
    }

    def action_statements: Parser[OsirisAction] = (affirm | negate) <~ ";"
    def statement = if_then | query | proc | action_statements

    def block: Parser[Block] = (raw_word <~ "{") ~ rep(statement) <~ "}" ^^ {
        case name ~ statements => {
            Block(name, statements)
        }
    }

    def goal: Parser[Goal] = raw_word ~ "{" ~ rep(block) <~ "}" ^^ { 
        case name ~ _ ~ blocks => Goal(name, blocks.map(b => (b.name, b)).toMap)
    }

    def program: Parser[Program] = rep(goal) ^^ { Program(_) }

    def run[T](p: Parser[T], s: String): T = {
        parse(p, s) match {
            case Success(matched, _) => matched
            case Failure(msg,_) => throw new Exception(s"FAILURE: $msg")
            case Error(msg,_) => throw new Exception(s"ERROR: $msg")
        }
    }

    def runMaybe[T](p: Parser[T], s: String): Option[T] = {
        parse(p, s) match {
            case Success(matched, _) => Some(matched)
            case Failure(msg,_) => None
            case Error(msg,_) => None
        }
    }

    def parseProgram(s: String): Option[Program] = {
        parse(program, s) match {
            case Success(result, _) => Some(result)
            case _ => None
        }
    }
}
