package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ExprsFixturesImpl]] */
final class ExprsSpec extends MacroSuite {

  group("trait typed.Exprs") {

    group("methods: Expr.{plainPrint, prettyPrint, plainAST, prettyAST}, expected behavior") {
      import ExprsFixtures.testExprPrinters

      test("should print expression") {
        testExprPrinters("test value") <==> Data.map(
          "Expr.plainPrint" -> Data("\"test value\""),
          "Expr.prettyPrint" -> Data("\"test value\""),
          "Expr.plainAST" -> Data(
            if (LanguageVersion.byHearth.isScala3) "Inlined(None, Nil, Literal(StringConstant(\"test value\")))"
            else """Literal(
                  |  Constant("test value")
                  |)""".stripMargin
          ),
          "Expr.prettyAST" -> Data(
            if (LanguageVersion.byHearth.isScala3) "Inlined(None, Nil, Literal(StringConstant(\"test value\")))"
            else """Literal(
                  |  Constant("test value")
                  |)""".stripMargin
          )
        )
      }
    }

    group("methods: Expr.summonImplicit, expected behavior") {
      import ExprsFixtures.testExprSummoning

      test("should summon implicit of available") {
        testExprSummoning[List[Int]] <==> Data.map("Expr.summonImplicit" -> Data("<failed to summon>"))
        testExprSummoning[DummyImplicit] <==> Data.map(
          "Expr.summonImplicit" -> Data("scala.DummyImplicit.dummyImplicit")
        )
      }
    }

    group("methods: Expr.upcast, expected behavior") {
      import ExprsFixtures.testExprUpcasting

      test("should upcast expression in compile time if it's possible") {
        testExprUpcasting[Any, String]("string") <==> Data.map("Expr.upcast" -> Data("<failed to upcast>"))
        testExprUpcasting[List[Int], Seq[Any]](List(1, 2, 3)) <==> Data.map(
          "Expr.upcast" -> Data(
            if (LanguageVersion.byHearth.isScala3) "scala.List.apply[scala.Int](1, 2, 3)"
            else "(scala.`package`.List.apply[Int](1, 2, 3): Seq[Any])"
          )
        )
      }
    }

    group("methods: Expr.suppressUnused, expected behavior") {
      import ExprsFixtures.testSuppressUnused

      test("should suppress unused expression") {
        val expr = 1

        testSuppressUnused(expr)
      }
    }

    group("type MatchCase") {

      test("method MatchCase.matchType should allow pattern-matching by the type") {
        import ExprsFixtures.testMatchCaseTypeMatch

        def expand(example: examples.enums.ExampleSealedTrait): Data = testMatchCaseTypeMatch(example)

        expand(examples.enums.ExampleSealedTrait.ExampleSealedTraitClass(1)) <==> Data.map(
          "name" -> Data("ExampleSealedTraitClass"),
          "type" -> Data("hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass"),
          "matchCase" -> Data("examplesealedtraitclass")
        )
        expand(examples.enums.ExampleSealedTrait.ExampleSealedTraitObject) <==> Data.map(
          "name" -> Data("ExampleSealedTraitObject"),
          "type" -> Data("hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type"),
          "matchCase" -> Data("examplesealedtraitobject")
        )
      }

      test("method MatchCase.partition should allow branching MatchCase in a macro") {
        import ExprsFixtures.testMatchCasePartition

        // Tests whether the result build using these methods compiles
        @scala.annotation.nowarn // suppress "unreachable code" error
        def run = testMatchCasePartition[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
        run
      }

      test("method MatchCase.traverse should allow traversing MatchCase in a macro") {
        import ExprsFixtures.testMatchCaseTraverse

        // Tests whether the result build using these methods compiles
        @scala.annotation.nowarn // suppress "unreachable code" error
        def run = testMatchCaseTraverse[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
        run
      }
    }

    group("type Scoped") {

      test(
        "methods Scoped.{createVal, createVar, createLazy, createDef, use} should create a scoped block with a definition, and allow using it"
      ) {
        import ExprsFixtures.testScopeCreateAndUse

        // Tests whether the result build using these methods compiles and correct
        testScopeCreateAndUse ==> Data(2 + 20 + 300 + 4000)
      }

      test("methods Scoped.{partition, close} should allow branching and closing a scoped block") {
        import ExprsFixtures.testScopePartitionAndClose

        // Tests whether the result build using these methods compiles and correct
        testScopePartitionAndClose[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
      }

      test("methods Scoped.{traverse, close} should allow traversing and closing a scoped block") {
        import ExprsFixtures.testScopeTraverseAndClose

        // Tests whether the result build using these methods compiles and correct
        testScopeTraverseAndClose[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
      }
    }

    group("type LambdaBuilder") {

      test("methods LambdaBuilder.{of1, of2, ..., of21, of22, build} should allow building a lambda") {
        import ExprsFixtures.testLambdaBuilderOfNAndBuild

        testLambdaBuilderOfNAndBuild ==> Data.map(
          "of1" -> Data(2 + 1),
          "of2" -> Data(2 * 3 + 1),
          "of3" -> Data(2 * 3 * 5 + 1),
          "of4" -> Data(2 * 3 * 5 * 7 + 1),
          "of5" -> Data(2 * 3 * 5 * 7 * 11 + 1),
          "of6" -> Data(2 * 3 * 5 * 7 * 11 * 13 + 1),
          "of7" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 + 1),
          "of8" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 + 1),
          "of9" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 + 1),
          "of10" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 + 1),
          "of11" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 + 1),
          "of12" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 + 1),
          "of13" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 + 1),
          "of14" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 + 1),
          "of15" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 + 1),
          "of16" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 + 1),
          "of17" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 + 1),
          "of18" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 + 1),
          "of19" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 + 1),
          "of20" -> Data(
            2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 * 71 + 1
          ),
          "of21" -> Data(
            2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 * 71 * 73 + 1
          ),
          "of22" -> Data(
            2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 * 71 * 73 * 79 + 1
          )
        )
      }

      test("methods LambdaBuilder.buildWith should allow building a lambda") {
        import ExprsFixtures.testLambdaBuilderBuildWith

        testLambdaBuilderBuildWith ==> Data(2 + 1)
      }

      test("methods LambdaBuilder.partition should allow branching LambdaBuilder in a macro") {
        import ExprsFixtures.testLambdaBuilderPartition

        testLambdaBuilderPartition[Int](5) ==> Data(5 + 2)
      }

      test("methods LambdaBuilder.traverse should allow traversing LambdaBuilder in a macro") {
        import ExprsFixtures.testLambdaBuilderTraverse

        testLambdaBuilderTraverse[Int](5) ==> Data(5 + 2)
      }
    }
  }

  group("type ExprCodec") {

    test(
      "methods ExprCodec.{toExpr, fromExpr} should allow converting between expressions and values for expressions supporting bidirectional transformation"
    ) {
      import ExprsFixtures.testBidirectionalCodecs

      testBidirectionalCodecs <==> Data.map(
        "null" -> Data.map(
          "encoded" -> Data("null"),
          "decoded" -> Data("null")
        ),
        "unit" -> Data.map(
          "encoded" -> Data("()"),
          "decoded" -> Data("()")
        ),
        "boolean" -> Data.map(
          "encoded" -> Data("true"),
          "decoded" -> Data("true")
        ),
        "byte" -> Data.map(
          "encoded" -> Data("1"),
          "decoded" -> Data("1")
        ),
        "short" -> Data.map(
          "encoded" -> Data("1"),
          "decoded" -> Data("1")
        ),
        "int" -> Data.map(
          "encoded" -> Data("1"),
          "decoded" -> Data("1")
        ),
        "long" -> Data.map(
          "encoded" -> Data("1L"),
          "decoded" -> Data("1")
        ),
        "float" -> Data.map(
          "encoded" -> Data("1.0f"),
          "decoded" -> Data("1.0")
        ),
        "double" -> Data.map(
          "encoded" -> Data("1.0"),
          "decoded" -> Data("1.0")
        ),
        "char" -> Data.map(
          "encoded" -> Data("\'a\'"),
          "decoded" -> Data("a")
        ),
        "string" -> Data.map(
          "encoded" -> Data("\"a\""),
          "decoded" -> Data("a")
        ),
        "Class" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.Predef.classOf[Int]"
            else "scala.Predef.classOf[scala.Int]"
          ),
          "decoded" -> Data("int") // TODO: 3
        ),
        "ClassTag" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.reflect.classTag[Int]"
            else "scala.reflect.ClassTag.apply[scala.Int](scala.Predef.classOf[scala.Int])"
          ),
          "decoded" -> Data("Int")
        )
      )
    }

    test(
      "methods ExprCodec.{toExpr, fromExpr} should allow converting between expressions and values for expressions supporting one-way transformation"
    ) {
      import ExprsFixtures.testOneWayCodecs

      testOneWayCodecs <==> Data.map(
        "Array[Int]" -> Data.map(
          "encoded" -> Data(if (LanguageVersion.byHearth.isScala2_13) "scala.Array(1)" else "scala.Array.apply(1, )")
        ),
        "Seq[Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.collection.immutable.Seq(1)"
            else "scala.Seq.apply[scala.Int](1)"
          )
        ),
        "List[Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.collection.immutable.List(1)"
            else "scala.List.apply[scala.Int](1)"
          )
        ),
        "Nil" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.collection.immutable.Nil" else "scala.Nil"
          )
        ),
        "Vector[Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.collection.immutable.Vector(1)"
            else "scala.Vector.apply[scala.Int](1)"
          )
        ),
        "Map[Int, Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.collection.immutable.Map(scala.Tuple2(1, 1))"
            else "scala.Predef.Map.apply[scala.Int, scala.Int](scala.Tuple2.apply[scala.Int, scala.Int](1, 1))"
          )
        ),
        "Set[Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.collection.immutable.Set(1)"
            else "scala.Predef.Set.apply[scala.Int](1)"
          )
        ),
        "Option[Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.Some(1)" else "scala.Some.apply[scala.Int](1)"
          )
        ),
        "Some[Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.Some(1)" else "scala.Some.apply[scala.Int](1)"
          )
        ),
        "None" -> Data.map(
          "encoded" -> Data(if (LanguageVersion.byHearth.isScala2_13) "scala.None" else "scala.None")
        ),
        "Either[Int, Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.util.Left(1)"
            else "scala.Left.apply[scala.Int, scala.Int](1)"
          )
        ),
        "Left[Int, Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.util.Left(1)"
            else "scala.Left.apply[scala.Int, scala.Int](1)"
          )
        ),
        "Right[Int, Int]" -> Data.map(
          "encoded" -> Data(
            if (LanguageVersion.byHearth.isScala2_13) "scala.util.Right(1)"
            else "scala.Right.apply[scala.Int, scala.Int](1)"
          )
        )
      )
    }
  }
}
