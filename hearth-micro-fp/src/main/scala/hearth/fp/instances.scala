package hearth
package fp

import scala.collection.mutable.ListBuffer
import hearth.fp.syntax.*

/** Instances for basic types (which couldn't be placed in type class modules, because they are hierarchical). */
object instances {

  implicit val IdentityApplicative: Applicative[Id] = new Applicative[Id] {

    def pure[A](a: A): A = a

    def map2[A, B, C](fa: A, fb: => B)(f: (A, B) => C): C = f(fa, fb)
  }

  implicit val OptionApplicativeTraverse: ApplicativeTraverse[Option] = new ApplicativeTraverse[Option] {

    def pure[A](a: A): Option[A] = Some(a)

    def map2[A, B, C](fa: Option[A], fb: => Option[B])(f: (A, B) => C): Option[C] = fa.zip(fb).map(f.tupled)

    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa.fold(Option.empty[B].pure[G])(f(_).map(Some(_)))

    def parTraverse[G[_]: Parallel, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa.fold(Option.empty[B].pure[G])(f(_).map(Some(_)))
  }

  implicit val ListApplicativeTraverse: ApplicativeTraverse[List] = new ApplicativeTraverse[List] {

    def pure[A](a: A): List[A] = List(a)

    def map2[A, B, C](fa: List[A], fb: => List[B])(f: (A, B) => C): List[C] = fa.zip(fb).map(f.tupled)

    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a))(_.append(_))
      }.map(_.toList)

    def parTraverse[G[_]: Parallel, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.parMap2(f(a))(_.append(_))
      }.map(_.toList)
  }

  implicit val VectorApplicativeTraverse: ApplicativeTraverse[Vector] = new ApplicativeTraverse[Vector] {

    def pure[A](a: A): Vector[A] = Vector(a)

    def map2[A, B, C](fa: Vector[A], fb: => Vector[B])(f: (A, B) => C): Vector[C] = fa.zip(fb).map(f.tupled)

    def traverse[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a))(_.append(_))
      }.map(_.toVector)

    def parTraverse[G[_]: Parallel, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.parMap2(f(a))(_.append(_))
      }.map(_.toVector)
  }
}
