package ru.otus.module2

import cats.Functor

import scala.util.{Failure, Success, Try}

package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]


  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */
  lazy val treeFunctor = new Functor[Tree] {

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f),map(right)(f))
    }
  }

  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }


  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

   lazy val tryME = new MonadError[Try,Throwable] {

     override def raiseError[A](e: Throwable): Try[A] = Failure(e)

     override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa match {
       case Failure(error) => f(error)
       case Success => fa

     }

     override def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa match {
       case Failure(error) => Try(f(error))
       case Success => fa
     }

     override def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = fa match {
       case Success(value) => if(f(value)) pure(value) else raiseError(e)
       case Failure => fa

     }

     override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa match {
       case Success(value) => f(value)
       case Failure(error: Error) => Failure(error)
     }
     override def pure[A](v: A): Try[A] = Success(v)
   }

  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */


   val eitherME = new MonadError[Either,String] {

     override def raiseError[A](e: String): Either[A] = ???

     override def handleErrorWith[A](fa: Either[A])(f: String => Either[A]): Either[A] = ???

     override def handleError[A](fa: Either[A])(f: String => A): Either[A] = ???

     override def ensure[A](fa: Either[A])(e: String)(f: A => Boolean): Either[A] = ???

     override def flatMap[A, B](fa: Either[A])(f: A => Either[B]): Either[B] = ???

     override def pure[A](v: A): Either[A] = ???
   }


}
