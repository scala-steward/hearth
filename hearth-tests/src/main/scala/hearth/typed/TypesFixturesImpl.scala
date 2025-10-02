package hearth
package typed

import hearth.data.Data

/** Fixtured for testing [[TypesSpec]] and [[TypedJvmSpec]]. */
trait TypesFixturesImpl { this: MacroTypedCommons =>

  // Type methods

  def testNamesPrinters[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.shortName" -> Data(Type[A].shortName),
      "Type.fcqn" -> Data(Type[A].fcqn),
      "Type.plainPrint" -> Data(Type[A].plainPrint),
      "Type.prettyPrint" -> Data(removeAnsiColors(Type[A].prettyPrint))
    )
  )

  def testClassOfType[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.classOfType" -> Data(Type[A].getRuntimeClass.fold("not on classpath")(_.toString))
    )
  )

  def testPosition[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.position" -> Data(Type[A].position.map(_.prettyPrint).getOrElse("<no position>"))
    )
  )

  def testChildren[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.directChildren" -> Type[A].directChildren
        .map(children => Data(children.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no direct children>")),
      "Type.exhaustiveChildren" -> Type[A].exhaustiveChildren
        .map(children => Data(children.toListMap.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no exhaustive children>"))
    )
  )

  def testAnnotations[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.annotations" -> Data.list(Type.annotations[A].map(value => Data(value.prettyPrint))*)
    )
  )

  def testFlags[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.isPrimitive" -> Data(Type[A].isPrimitive),
      "Type.isArray" -> Data(Type[A].isArray),
      "Type.isJvmBuiltIn" -> Data(Type[A].isJvmBuiltIn),
      "Type.isAbstract" -> Data(Type[A].isAbstract),
      "Type.isFinal" -> Data(Type[A].isFinal),
      "Type.isClass" -> Data(Type[A].isClass),
      "Type.isTypeSystemSpecial" -> Data(Type[A].isTypeSystemSpecial),
      "Type.notJvmBuiltInClass" -> Data(Type[A].notJvmBuiltInClass),
      "Type.isPlainOldJavaObject" -> Data(Type[A].isPlainOldJavaObject),
      "Type.isJavaBean" -> Data(Type[A].isJavaBean),
      "Type.isSealed" -> Data(Type[A].isSealed),
      "Type.isJavaEnum" -> Data(Type[A].isJavaEnum),
      "Type.isJavaEnumValue" -> Data(Type[A].isJavaEnumValue),
      "Type.isCase" -> Data(Type[A].isCase),
      "Type.isObject" -> Data(Type[A].isObject),
      "Type.isVal" -> Data(Type[A].isVal),
      "Type.isCaseClass" -> Data(Type[A].isCaseClass),
      "Type.isCaseObject" -> Data(Type[A].isCaseObject),
      "Type.isCaseVal" -> Data(Type[A].isCaseVal),
      "Type.isAvailable(Everywhere)" -> Data(Type[A].isAvailable(Everywhere)),
      "Type.isAvailable(AtCallSite)" -> Data(Type[A].isAvailable(AtCallSite))
    )
  )

  def testChildrenFlags[A: Type]: Expr[Data] = Expr(
    Type[A].directChildren
      .map { children =>
        Data(children.view.map { case (name, child) =>
          import child.Underlying as Child
          name -> Data.map(
            "Type.isSealed" -> Data(Child.isSealed),
            "Type.isJavaEnum" -> Data(Child.isJavaEnum),
            "Type.isJavaEnumValue" -> Data(Child.isJavaEnumValue),
            "Type.isCase" -> Data(Child.isCase),
            "Type.isObject" -> Data(Child.isObject),
            "Type.isVal" -> Data(Child.isVal),
            "Type.isCaseClass" -> Data(Child.isCaseClass),
            "Type.isCaseObject" -> Data(Child.isCaseObject),
            "Type.isCaseVal" -> Data(Child.isCaseVal),
            "Type.isAvailable(Everywhere)" -> Data(Child.isAvailable(Everywhere)),
            "Type.isAvailable(AtCallSite)" -> Data(Child.isAvailable(AtCallSite))
          )
        }.toMap)
      }
      .getOrElse(Data("<no direct children>"))
  )

  def testComparisons[A: Type, B: Type]: Expr[Data] = Expr(
    Data.map(
      "<:<" -> Data(Type[A] <:< Type[B]),
      "=:=" -> Data(Type[A] =:= Type[B])
    )
  )

  // TypeCodecs

  def testBidirectionalCodecs: Expr[Data] = {
    def roundtrip[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      val decoded = Type.unapply(encoded)
      Data.map(
        "encoded" -> Data(encoded.plainPrint),
        "decoded" -> Data(decoded.fold("not decoded") { s =>
          val result = s"$s"
          result.indexOf('@') match {
            case -1    => result
            case index => result.substring(0, index)
          }
        })
      )
    }
    Expr(
      Data.map(
        "null" -> roundtrip(null),
        "unit" -> roundtrip(()),
        "boolean" -> roundtrip(true),
        "byte" -> roundtrip(1.toByte),
        "short" -> roundtrip(1.toShort),
        "int" -> roundtrip(1),
        "long" -> roundtrip(1L),
        "float" -> roundtrip(1.toFloat),
        "double" -> roundtrip(1.toDouble),
        "char" -> roundtrip('a'),
        "string" -> roundtrip("a"),
        "module" -> roundtrip(scala.Predef)(using TypeCodec.ModuleCodec)
      )
    )
  }

  def testOneWayCodecs: Expr[Data] = {
    def oneWay[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      Data.map(
        "encoded" -> Data(encoded.plainPrint)
      )
    }
    implicit val a: Type["a"] = aStringType
    implicit val b: Type["b"] = bStringType
    Expr(
      Data.map(
        """Array["a"]""" -> oneWay[Array["a"]](Array["a"]("a")),
        """Seq["a"]""" -> oneWay[Seq["a"]](Seq["a"]("a")),
        """List["a"]""" -> oneWay[List["a"]](List["a"]("a")),
        "Nil" -> oneWay[Nil.type](Nil),
        """Vector["a"]""" -> oneWay[Vector["a"]](Vector["a"]("a")),
        """Map["a", "b"]""" -> oneWay[Map["a", "b"]](Map["a", "b"](("a": "a", "b": "b"))),
        """Set["a"]""" -> oneWay[Set["a"]](Set["a"]("a")),
        """Option["a"]""" -> oneWay[Option["a"]](Option["a"]("a")),
        """Some["a"]""" -> oneWay[Some["a"]](Some["a"]("a")),
        "None" -> oneWay[None.type](None),
        """Either["a", "b"]""" -> oneWay[Either["a", "b"]](Left["a", "b"]("a")),
        """Left["a", "b"]""" -> oneWay[Left["a", "b"]](Left["a", "b"]("a")),
        """Right["a", "b"]""" -> oneWay[Right["a", "b"]](Right["a", "b"]("b"))
      )
    )
  }

  // types using in fixtures

  private val aStringType = Type.of["a"]
  private val bStringType = Type.of["b"]
}
