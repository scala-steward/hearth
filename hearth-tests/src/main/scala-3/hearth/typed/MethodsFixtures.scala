package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class MethodsFixtures(q: Quotes) extends MacroCommonsScala3(using q), MethodsFixturesImpl

object MethodsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testConstructorsExtraction[A]: Data = ${ testConstructorsExtractionImpl[A] }
  private def testConstructorsExtractionImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructorsExtraction[A]

  inline def testMethodsExtraction[A](inline excluding: String*): Data = ${
    testMethodsExtractionImpl[A]('{ excluding })
  }
  private def testMethodsExtractionImpl[A: Type](excluding: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodsExtraction[A](excluding)

  inline def testCallNoInstanceIntMethod[A](inline methodName: String)(inline params: Int*): Int = ${
    testCallNoInstanceIntMethodImpl[A]('{ methodName }, 'params)
  }
  private def testCallNoInstanceIntMethodImpl[A: Type](methodName: Expr[String], params: Expr[Seq[Int]])(using
      q: Quotes
  ): Expr[Int] =
    new MethodsFixtures(q).testCallNoInstanceIntMethod[A](methodName)(params)

  inline def testCallInstanceIntMethod[A](inline instance: A)(inline methodName: String)(inline params: Int*): Int = ${
    testCallInstanceIntMethodImpl[A]('{ instance }, 'methodName, 'params)
  }
  private def testCallInstanceIntMethodImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String],
      params: Expr[Seq[Int]]
  )(using q: Quotes): Expr[Int] =
    new MethodsFixtures(q).testCallInstanceIntMethod[A](instance)(methodName)(params)
}
