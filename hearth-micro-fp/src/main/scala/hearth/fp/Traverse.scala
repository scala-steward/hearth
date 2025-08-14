package hearth
package fp

/** We do something for each element of a thing with a result-type, and get the result-type of the whole transformed
  * thing.
  *
  * @since 0.1.0
  */
trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def parTraverse[G[_]: Parallel, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(instances.IdentityApplicative)
}
object Traverse {

  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def traverse[G[_]: Applicative, B](f: A => G[B])(implicit F: Traverse[F]): G[F[B]] =
      F.traverse(fa)(f)

    def parTraverse[G[_]: Parallel, B](f: A => G[B])(implicit F: Traverse[F]): G[F[B]] =
      F.parTraverse(fa)(f)
  }

  final class SequenceOps[F[_], G[_], A](private val fga: F[G[A]]) extends AnyVal {

    def sequence(implicit F: Traverse[F], G: Applicative[G]): G[F[A]] =
      F.traverse(fga)(identity)

    def parSequence(implicit F: Traverse[F], G: Parallel[G]): G[F[A]] =
      F.parTraverse(fga)(identity)
  }
}
