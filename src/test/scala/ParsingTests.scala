import org.scalatest.funsuite.AnyFunSuite
import scala.util.parsing.combinator._

class ParsingTestsSuite extends AnyFunSuite {
    test("Parsing numbers") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.number, "3") == 
                Literal("3", Some(OsirisInteger64)))

        assert(
            parser.run(parser.number, "-3") == 
                Literal("-3", Some(OsirisInteger64)))
    }
    
    test("Parsing reals") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.real, "3.14") == 
                Literal("3.14", Some(OsirisReal)))
        assert(
            parser.run(parser.real, "-3.3") == 
                Literal("-3.3", Some(OsirisReal)))
    }

    test("Parsing GUIDs") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.guid, "Player_Ifan_ad9a3327-4456-42a7-9bf4-7ad60cc9e54f") ==
                Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))
        )
        assert(
            parser.run(parser.guid, "ad9a3327-4456-42a7-9bf4-7ad60cc9e54f") ==
                Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))
        )
        assert(
            parser.run(parser.guid, "CHARACTERGUID_S_Player_Ifan_ad9a3327-4456-42a7-9bf4-7ad60cc9e54f") ==
                Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))
        )
    }

    test("Parsing string literals") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.string, "\"hello world\"") ==
                Literal("hello world", Some(OsirisString))
        )
    }

    test("Parsing literals") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.literal, "\"hello world\"") ==
                Literal("hello world", Some(OsirisString))
        )
        assert(
            parser.run(parser.literal, "CHARACTERGUID_S_Player_Ifan_ad9a3327-4456-42a7-9bf4-7ad60cc9e54f") ==
                Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))
        )
        assert(
            parser.run(parser.literal, "-3.3") == 
                Literal("-3.3", Some(OsirisReal)))
        assert(
            parser.run(parser.literal, "-3") == 
                Literal("-3", Some(OsirisInteger64)))
    }

    test("Parsing typecasts") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.typecast, "(INTEGER)8") ==
                TypeCast(OsirisInteger, Literal("8", Some(OsirisInteger64)))
        )
        assert(
            parser.run(parser.typecast, "(CHARACTERGUID)ad9a3327-4456-42a7-9bf4-7ad60cc9e54f") ==
                TypeCast(OsirisGUIDCharacter, Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID)))
        )
        assert(
            parser.run(parser.typecast, "(CHARACTERGUID)34") ==
                TypeCast(OsirisGUIDCharacter, Literal("34", Some(OsirisInteger64)))
        )
        assert(
            parser.run(parser.typecast, "(CHARACTERGUID)(INTEGER)34") ==
                TypeCast(OsirisGUIDCharacter, TypeCast(OsirisInteger, Literal("34", Some(OsirisInteger64))))
        )
    }

    test("Casting types") {
        val parser = new OsirisParser();
        assert(
            TypeCast(OsirisInteger, Literal("8", Some(OsirisInteger64))).cast ==
                Literal("8", Some(OsirisInteger))
        )
        assert(
            TypeCast(OsirisInteger64, Literal("8", Some(OsirisInteger))).cast ==
                Literal("8", Some(OsirisInteger64))
        )
        assert(
            TypeCast(OsirisInteger, Literal("8.34", Some(OsirisReal))).cast ==
                Literal("8", Some(OsirisInteger))
        )
        assert(
            TypeCast(OsirisGUIDCharacter, Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))).cast ==
                Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUIDCharacter))
        )
    }

    test("Parsing comparisons") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.comparison, "8 == 5.3") ==
                Comparison(
                    Literal("8", Some(OsirisInteger64)), 
                    Literal("5.3", Some(OsirisReal)), CompEq)
        )
        assert(
            parser.run(parser.comparison, "8 < 5") ==
                Comparison(
                    Literal("8", Some(OsirisInteger64)), 
                    Literal("5", Some(OsirisInteger64)), CompLt)
        )
    }

    test("Parsing facts") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.fact, "DB_MyPrefix_Fruit(\"Apple\");") ==
                Fact("DB_MyPrefix_Fruit", Seq(Literal("Apple", Some(OsirisString))))
        )
        assert(
            parser.run(parser.fact, "DB_MyPrefix_Fruit(\"Apple\", 3.14);") ==
                Fact("DB_MyPrefix_Fruit", Seq(
                    Literal("Apple", Some(OsirisString)),
                    Literal("3.14", Some(OsirisReal)),
                ))
        )
        assert(
            parser.run(parser.fact, "DB_MyPrefix_Fruit(\"Apple\", X);") ==
                Fact("DB_MyPrefix_Fruit", Seq(
                    Literal("Apple", Some(OsirisString)),
                    Variable("X", None)
                ))
        )
        assert(
            parser.run(parser.fact, "DB_MyPrefix_Fruit(\"Apple\", (CHARACTERGUID)_X);") ==
                Fact("DB_MyPrefix_Fruit", Seq(
                    Literal("Apple", Some(OsirisString)),
                    TypeCast(OsirisGUIDCharacter, Variable("_X", None))
                ))
        )
        assert(
            parser.run(parser.fact, "DB_Overview_Origins(CHARACTERGUID_S_Player_Ifan_ad9a3327-4456-42a7-9bf4-7ad60cc9e54f);") ==
                Fact("DB_Overview_Origins", Seq(
                    Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))
                ))
        )
        assert(
            parser.run(parser.fact, "DB_Overview_Origins((CHARACTERGUID)CHARACTERGUID_S_Player_Ifan_ad9a3327-4456-42a7-9bf4-7ad60cc9e54f, \"IFAN\");") ==
                Fact("DB_Overview_Origins", Seq(
                    TypeCast(OsirisGUIDCharacter, Literal("ad9a3327-4456-42a7-9bf4-7ad60cc9e54f", Some(OsirisGUID))),
                    Literal("IFAN", Some(OsirisString))
                ))
        )
    }

    test("Parsing if block") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.if_then, "IF FooBar(3) THEN Noop(1);") ==
                IfThen(
                    Seq(FactCheck(Fact("FooBar", Seq(Literal("3", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("Noop", Seq(Literal("1", Some(OsirisInteger64))))))
                )
        )
        assert(
            parser.run(parser.if_then, "IF FooBar(3) AND BooFar(2) THEN Noop(1);") ==
                IfThen(
                    Seq(
                        FactCheck(Fact("FooBar", Seq(Literal("3", Some(OsirisInteger64))))),
                        FactCheck(Fact("BooFar", Seq(Literal("2", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("Noop", Seq(Literal("1", Some(OsirisInteger64))))))
                )
        )
        assert(
            parser.run(parser.if_then, "IF FooBar(3) AND BooFar(2) THEN Noop(1); NOT Noop(2);") ==
                IfThen(
                    Seq(
                        FactCheck(Fact("FooBar", Seq(Literal("3", Some(OsirisInteger64))))),
                        FactCheck(Fact("BooFar", Seq(Literal("2", Some(OsirisInteger64)))))),
                    Seq(
                        Affirm(Fact("Noop", Seq(Literal("1", Some(OsirisInteger64))))),
                        Negate(Fact("Noop", Seq(Literal("2", Some(OsirisInteger64)))))
                    )
                )
        )
    }

    test("Parsing query") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.query, "QRY CharacterIsIncapacitated((CHARACTERGUID)_Char) AND HasActiveStatus(_Char, \"FROZEN\", 1) THEN DB_NOOP(1);") ==
                Query(
                    Fact("CharacterIsIncapacitated", Seq(TypeCast(OsirisGUIDCharacter, Variable("_Char", None)))),
                    Seq(FactCheck(Fact("HasActiveStatus", Seq(Variable("_Char", None), Literal("FROZEN", Some(OsirisString)), Literal("1", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("DB_NOOP", Seq(Literal("1", Some(OsirisInteger64))))))
                )
        )
    }

    test("Parsing procs") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.proc, "PROC PROC_Overview_TeleportAlive((CHARACTERGUID)_Char, (GUIDSTRING)_Destination) AND CharacterIsDead(_Char, 1) THEN CharacterResurrect(_Char);") ==
                Proc(
                    Fact("PROC_Overview_TeleportAlive", Seq(TypeCast(OsirisGUIDCharacter, Variable("_Char", None)), TypeCast(OsirisGUID, Variable("_Destination", None)))),
                    Seq(FactCheck(Fact("CharacterIsDead", Seq(Variable("_Char", None), Literal("1", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("CharacterResurrect", Seq(Variable("_Char", None)))))
                )
        )
    }

    test("Parsing statements") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.statement, "IF FooBar(3) THEN Noop(1);") ==
                IfThen(
                    Seq(FactCheck(Fact("FooBar", Seq(Literal("3", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("Noop", Seq(Literal("1", Some(OsirisInteger64))))))
                )
        )
        assert(
            parser.run(parser.statement, "IF FooBar(3) AND BooFar(2) THEN Noop(1);") ==
                IfThen(
                    Seq(
                        FactCheck(Fact("FooBar", Seq(Literal("3", Some(OsirisInteger64))))),
                        FactCheck(Fact("BooFar", Seq(Literal("2", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("Noop", Seq(Literal("1", Some(OsirisInteger64))))))
                )
        )
        assert(
            parser.run(parser.statement, "IF FooBar(3) AND BooFar(2) THEN Noop(1); NOT Noop(2);") ==
                IfThen(
                    Seq(
                        FactCheck(Fact("FooBar", Seq(Literal("3", Some(OsirisInteger64))))),
                        FactCheck(Fact("BooFar", Seq(Literal("2", Some(OsirisInteger64)))))),
                    Seq(
                        Affirm(Fact("Noop", Seq(Literal("1", Some(OsirisInteger64))))),
                        Negate(Fact("Noop", Seq(Literal("2", Some(OsirisInteger64)))))
                    )
                )
        )
        assert(
            parser.run(parser.statement, "QRY CharacterIsIncapacitated((CHARACTERGUID)_Char) AND HasActiveStatus(_Char, \"FROZEN\", 1) THEN DB_NOOP(1);") ==
                Query(
                    Fact("CharacterIsIncapacitated", Seq(TypeCast(OsirisGUIDCharacter, Variable("_Char", None)))),
                    Seq(FactCheck(Fact("HasActiveStatus", Seq(Variable("_Char", None), Literal("FROZEN", Some(OsirisString)), Literal("1", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("DB_NOOP", Seq(Literal("1", Some(OsirisInteger64))))))
                )
        )
        assert(
            parser.run(parser.statement, "PROC PROC_Overview_TeleportAlive((CHARACTERGUID)_Char, (GUIDSTRING)_Destination) AND CharacterIsDead(_Char, 1) THEN CharacterResurrect(_Char);") ==
                Proc(
                    Fact("PROC_Overview_TeleportAlive", Seq(TypeCast(OsirisGUIDCharacter, Variable("_Char", None)), TypeCast(OsirisGUID, Variable("_Destination", None)))),
                    Seq(FactCheck(Fact("CharacterIsDead", Seq(Variable("_Char", None), Literal("1", Some(OsirisInteger64)))))),
                    Seq(Affirm(Fact("CharacterResurrect", Seq(Variable("_Char", None)))))
                )
        )
    }
    
    test("Parsing blocks") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.block, "INIT { DoThat(1); }") ==
                Block("INIT", Seq(
                    Affirm(Fact("DoThat", Seq(Literal("1", Some(OsirisInteger64)))))
                ))
        )
        assert(
            parser.run(parser.block, "INIT { NOT DoThis(2); DoThat(1); IF DoThat(_X) THEN Okay(1); }") ==
                Block("INIT", Seq(
                    Negate(Fact("DoThis", Seq(Literal("2", Some(OsirisInteger64))))),
                    Affirm(Fact("DoThat", Seq(Literal("1", Some(OsirisInteger64))))),
                    IfThen(Seq(FactCheck(Fact("DoThat", Seq(Variable("_X", None))))), Seq(Affirm(Fact("Okay", Seq(Literal("1", Some(OsirisInteger64)))))))
                ))
        )
    }

    test("Parsing cypher") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.cypher, "[ HELLO world here i am]") ==
                Cypher("HELLO", "world here i am")
        );
    }

    test("Parsing goals") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.goal, "DoSomething { INIT { DoThat(1); } KB { DoThis(1); } EXIT { NOT DoThis(1); } }") ==
                Goal("DoSomething", Map(
                    ("INIT", Block("INIT", Seq(
                        Affirm(Fact("DoThat", Seq(Literal("1", Some(OsirisInteger64)))))
                    ))),
                    ("KB", Block("KB", Seq(
                        Affirm(Fact("DoThis", Seq(Literal("1", Some(OsirisInteger64)))))
                    ))),
                    ("EXIT", Block("EXIT", Seq(
                        Negate(Fact("DoThis", Seq(Literal("1", Some(OsirisInteger64)))))
                    ))),
                )
        ))

        assert(
            parser.run(parser.goal, """
            DoSomething {
                INIT { DoThat(1); }
                KB { DoThis(1); }
                EXIT { NOT DoThis(1); }
            }""") ==
                Goal("DoSomething", Map(
                    ("INIT", Block("INIT", Seq(
                        Affirm(Fact("DoThat", Seq(Literal("1", Some(OsirisInteger64)))))
                    ))),
                    ("KB", Block("KB", Seq(
                        Affirm(Fact("DoThis", Seq(Literal("1", Some(OsirisInteger64)))))
                    ))),
                    ("EXIT", Block("EXIT", Seq(
                        Negate(Fact("DoThis", Seq(Literal("1", Some(OsirisInteger64)))))
                    ))),
                ))
        )
    }

    test("Parsing program") {
        val parser = new OsirisParser();
        assert(
            parser.run(parser.program, """
            main {
                INIT { DoThat(1); }
                KB { DoThis(1); }
                EXIT { NOT DoThis(1); }
            }
            
            sub {
                KB { DoSomething(0); }
            }""") ==
                Program(Seq(
                    Goal("main", Map(
                        ("INIT", Block("INIT", Seq(
                            Affirm(Fact("DoThat", Seq(Literal("1", Some(OsirisInteger64)))))
                        ))),
                        ("KB", Block("KB", Seq(
                            Affirm(Fact("DoThis", Seq(Literal("1", Some(OsirisInteger64)))))
                        ))),
                        ("EXIT", Block("EXIT", Seq(
                            Negate(Fact("DoThis", Seq(Literal("1", Some(OsirisInteger64)))))
                        ))),
                    )),
                    Goal("sub", Map(
                        ("KB", Block("KB", Seq(
                            Affirm(Fact("DoSomething", Seq(Literal("0", Some(OsirisInteger64)))))
                        ))),
                    )
                )))
        )
    }
}