import TypeCasting.isCastable
import TypeCasting.castValue

sealed trait OsirisType {
    override def toString(): String
}

case object OsirisBottom extends OsirisType {
    override def toString(): String = "#BOTTOM#"
}
case object OsirisInteger extends OsirisType {
    override def toString(): String = "Integer"
}
case object OsirisInteger64 extends OsirisType {
    override def toString(): String = "Integer64"
}
case object OsirisReal extends OsirisType {
    override def toString(): String = "Real"
}
case object OsirisString extends OsirisType {
    override def toString(): String = "String"
}
case object OsirisGUID extends OsirisType {
    override def toString(): String = "GUIDSTRING"
}
case object OsirisGUIDCharacter extends OsirisType {
    override def toString(): String = "CHARACTERGUID"
}
case object OsirisGUIDItem extends OsirisType {
    override def toString(): String = "ITEMGUID"
}
case object OsirisGUIDTrigger extends OsirisType {
    override def toString(): String = "TRIGGERGUID"
}
case object OsirisGUIDSpline extends OsirisType {
    override def toString(): String = "SPLINEGUID"
}
case object OsirisGUIDLevelTemplate extends OsirisType {
    override def toString(): String = "LEVELTEMPLATEGUID"
}

sealed trait ComparisonOperator {
    override def toString(): String
}
case object CompEq extends ComparisonOperator {
    override def toString(): String = "=="
}
case object CompNe extends ComparisonOperator {
    override def toString(): String = "!="
}
case object CompGt extends ComparisonOperator {
    override def toString(): String = ">="
}
case object CompLt extends ComparisonOperator {
    override def toString(): String = "<="
}
case object CompGe extends ComparisonOperator {
    override def toString(): String = ">"
}
case object CompLe extends ComparisonOperator {
    override def toString(): String = "<"
}

case class Fact(name: String, arguments: Seq[Value]) {
    override def toString(): String = s"${name}(${arguments.mkString(", ")})"
}

object TypeCasting {
    def isCastable(lhs: OsirisType, rhs: OsirisType): Boolean = {
        (lhs, rhs) match {
            case (_, _) if lhs == rhs => true

            case (OsirisInteger, OsirisInteger64)
                | (OsirisInteger64, OsirisInteger)
                | (OsirisReal, OsirisInteger)
                | (OsirisReal, OsirisInteger64)
                | (OsirisInteger, OsirisReal)
                | (OsirisInteger64, OsirisReal)
                | (OsirisGUIDCharacter, OsirisGUID)
                | (OsirisGUIDItem, OsirisGUID)
                | (OsirisGUIDTrigger, OsirisGUID)
                | (OsirisGUIDSpline, OsirisGUID)
                | (OsirisGUIDLevelTemplate, OsirisGUID)
                | (OsirisGUID, OsirisGUIDCharacter)
                | (OsirisGUID, OsirisGUIDItem)
                | (OsirisGUID, OsirisGUIDTrigger)
                | (OsirisGUID, OsirisGUIDSpline)
                | (OsirisGUID, OsirisGUIDLevelTemplate) => true

            case _ => false
        }
    }

    def castValue(from: OsirisType, into: OsirisType, v: String): String = {
        (from, into) match {
            case (OsirisReal, OsirisInteger) => v.toDouble.round.toInt.toString
            case (OsirisReal, OsirisInteger64) => v.toDouble.round.toInt.toString
            case _ => v
        }
    }
}

sealed trait Value {
    def actualValue: String
    def valueType: OsirisType

    def changeValue(newValue: String): Value
    override def toString(): String
}

case class TypeCast(typ: OsirisType, value: Value) extends Value {
    def actualValue: String = value.actualValue
    def valueType = typ
    def changeValue(newValue: String): Value = TypeCast(typ, value.changeValue(newValue))

    def cast: Value = {
        (value, typ) match {
            case (Literal(v, None), _) => Literal(v, Some(typ))
            case (Literal(v, Some(t1)), t2) if isCastable(t1, t2) => Literal(TypeCasting.castValue(t1, t2, v), Some(t2))
            
            case (Variable(n, None), _) => Variable(n, Some(typ))
            case (Variable(n, Some(t1)), t2) if isCastable(t1, t2) => Variable(n, Some(t2))

            case (OsirisError(log), _) => OsirisError(log)
            case _ => OsirisError(Seq("Typecast to wrong type"))
        }
    }

    override def toString(): String = s"(${typ})${value}"
}

case class Literal(value: String, typ: Option[OsirisType]) extends Value {
    def actualValue: String = value
    def valueType = typ.getOrElse(OsirisBottom)
    def changeValue(newValue: String): Value = Literal(newValue, typ)
    override def toString(): String = s"${value}"
}

case class Variable(name: String, typ: Option[OsirisType]) extends Value {
    def actualValue: String = name
    def valueType = typ.getOrElse(OsirisBottom)
    def changeValue(newValue: String): Value = Literal(newValue, typ)
    override def toString(): String = s"${name}"
}

case class OsirisError(message: Seq[String]) extends Value {
    def actualValue: String = "#ERROR"
    def valueType = OsirisBottom
    def changeValue(newValue: String): Value = this
    override def toString(): String = s"ERROR: ${message.mkString("\n")}"
}

sealed trait Condition {
    override def toString(): String
}
case class FactCheck(fact: Fact) extends Condition {
    override def toString(): String = fact.toString()
}

case class Comparison(value: Value, other: Value, op: ComparisonOperator) extends Condition {
    override def toString(): String = s"${value} ${op} ${other}"
}

sealed trait OsirisStatement {
    override def toString(): String
}

sealed trait OsirisAction extends OsirisStatement
case class Affirm(fact: Fact) extends OsirisAction {
    override def toString(): String = s"${fact}"
}
case class Negate(fact: Fact) extends OsirisAction {
    override def toString(): String = s"NOT ${fact}"
}

case class IfThen(cond: Seq[Condition], action: Seq[OsirisAction]) extends OsirisStatement {
    override def toString(): String = s"IF ${cond.mkString(" AND ")} THEN ${action.mkString("; ")};"
}
case class Query(fact: Fact, cond: Seq[Condition], action: Seq[OsirisAction]) extends OsirisStatement {
    override def toString(): String = {
        val conds = if (cond.length > 0) " AND " + cond.mkString(" AND ") else ""
        s"QRY ${fact} ${conds} THEN ${action.mkString("; ")};"
    }
}
case class Proc(fact: Fact, cond: Seq[Condition], action: Seq[OsirisAction]) extends OsirisStatement {
    override def toString(): String = {
        val conds = if (cond.length > 0) " AND " + cond.mkString(" AND ") else ""
        s"PROC ${fact} ${conds} THEN ${action.mkString("; ")};"
    }
}

case class Block(name: String, statements: Seq[OsirisStatement]) {
    override def toString(): String = {
        val stmts = statements.map(s => "    " + s.toString()).mkString(";\n");
        s"  ${name} {\n${stmts};\n  }"
    }
}

case class Goal(name: String, blocks: Map[String, Block]) {
    override def toString(): String = {
        val blks = blocks.map { case (name, b) => b.toString() + "\n" }.mkString("\n");
        s"// ${name}:\n\n${blks}"
    }
}

case class Program(goals: Seq[Goal]) {
    override def toString(): String = {
        goals.map(_.toString).mkString("\n")
    }
}