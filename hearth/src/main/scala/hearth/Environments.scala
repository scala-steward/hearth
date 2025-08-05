package hearth

import hearth.compat.*

trait Environments {

  /** Platform-specific position representation (`c.universe.Position` in 2, `quotes.reflect.Position` in 3). */
  type Position

  val Position: PositionModule
  trait PositionModule { this: Position.type =>

    def current: Position

    def file(pos: Position): Option[java.nio.file.Path]
    def offset(pos: Position): Int
    def line(pos: Position): Int
    def column(pos: Position): Int
  }
  implicit class PositionMethods(private val position: Position) {
    def file: Option[java.nio.file.Path] = Position.file(position)
    def offset: Int = Position.offset(position)
    def line: Int = Position.line(position)
    def column: Int = Position.column(position)
  }

  implicit lazy val PositionOrdering: Ordering[Position] =
    Ordering[String].on[Position](_.file.toString).orElseBy(_.offset)

  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    lazy val currentPosition: Position = Position.current

    def currentScalaVersion: ScalaVersion
    lazy val currentLanguageVersion: LanguageVersion = currentScalaVersion.toLanguageVersion
    lazy val isScala2_12: Boolean = currentLanguageVersion == LanguageVersion.Scala2_12
    lazy val isScala2_13: Boolean = currentLanguageVersion == LanguageVersion.Scala2_13
    lazy val isScala3: Boolean = currentLanguageVersion == LanguageVersion.Scala3

    def XMacroSettings: List[String]

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing

    /** Pass position like "File.scala:12" or "File.scala:12:34", and it will check if current expansion matches it.
      *
      * Useful for debugging macros, when we don't want to print details for every single test case but just one.
      *
      * @param compilationLogPosition
      *   position as seen in the compilation log
      */
    def isExpandedAt(compilationLogPosition: String): Boolean = compilationLogPosition match {
      case fileLineRegex(fileName, line) =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(fileName))
        val actualLine = currentPosition.line + adjustLine
        val lineMatches = scala.util.Try(line.toInt).toOption.contains(actualLine)
        fileMatches && lineMatches
      case fileLineColumnRegex(fileName, line, column) =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(fileName))
        val actualLine = currentPosition.line + adjustLine
        val lineMatches = scala.util.Try(line.toInt).toOption.contains(actualLine)
        val columnMatches = scala.util.Try(column.toInt).toOption.contains(currentPosition.column)
        fileMatches && lineMatches && columnMatches
      case _ => false
    }
    private val adjustLine = if (isScala3) 1 else 0 // Scala 3 i 0-indexed, or decremented for some reason?
    private val fileLineRegex = """^(.+):(\d+)$""".r
    private val fileLineColumnRegex = """^(.+):(\d+):(\d+)$""".r
  }

  val CrossQuotes: CrossQuotesModule
  trait CrossQuotesModule { this: CrossQuotes.type =>

    /** `scala.reflect.macros.blackbox.Context` on Scala 2, `scala.quoted.Quotes` on Scala 3. */
    def ctx[CastAs]: CastAs

    /** Let us infer inner `A` in `Expr[A]` at compilation phase where we only know outer types. */
    final def castK[F[_], G[_]]: Cast[F, G] = CastImpl.asInstanceOf[Cast[F, G]]

    sealed trait Cast[F[_], G[_]] {
      def apply[A](fa: F[A]): G[A]
    }
    private object CastImpl extends Cast[fp.Id, fp.Id] {
      def apply[A](fa: A): A = fa
    }
  }
}
