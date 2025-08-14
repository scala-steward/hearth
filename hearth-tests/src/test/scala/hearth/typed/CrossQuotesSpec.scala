package hearth
package typed

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3)
  */
final class CrossQuotesSpec extends MacroSuite {

  group("CrossQuotes macro/plugin") {

    group("for Type.of") {

      test("should work for simple types") {
        CrossQuotesFixtures.simpleType <==> "scala.Int"
      }

      test("should work for generic types") {
        CrossQuotesFixtures.genericType[Int] <==> "scala.collection.immutable.List[scala.Int]"
      }

      test("should work for unsanitized types") {
        CrossQuotesFixtures.unsanitizedType <==> "scala.collection.immutable.ListMap[scala.Int, java.lang.String]"
      }
    }

    group("for Expr.quote+Expr.splice") {

      test("should work for simple expressions") {
        CrossQuotesFixtures.simpleExpr <==> "3"
      }

      test("should work for generic expressions") {
        CrossQuotesFixtures.genericExpr(4) <==> "4"
      }

      test("should work for unsanitized expressions") {
        CrossQuotesFixtures.unsanitizedExpr <==> "ListMap(1 -> 2)"
      }

      test("should work for nested expressions") {
        CrossQuotesFixtures.nestedExpr <==> "42"
      }
    }
  }
}
