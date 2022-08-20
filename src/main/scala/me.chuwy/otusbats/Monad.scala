package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
}

object Monad {
  implicit val optionMonad: Monad[Option[_]] = new Monad[Option[_]] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(v) => f(v)
      case None => None
    }
    override def point[A](a: A): Option[A] = Option(a)
  }

  implicit val listMonad: Monad[List[_]] = new Monad[List[_]] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)
  }

  implicit val vectorMonad: Monad[Vector[_]] = new Monad[Vector[_]] {
    override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)

    override def point[A](a: A): Vector[A] = Vector(a)
  }
}
