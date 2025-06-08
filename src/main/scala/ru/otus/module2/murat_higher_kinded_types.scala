package ru.otus.module2

object murat_higher_kinded_types {

  trait Tupler[F[_]] {
    def tupleF[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object TuplerInstances {

    implicit val optTupler: Tupler[Option] = new Tupler[Option] {
      override def tupleF[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }

    // в целом у всех контейнеров получается схожий подход мб тут подошел бы implicit class
    // для тех классов которые имеют при себе flatmap и map ( Future в том числе)
    implicit val listTupler: Tupler[List] = new Tupler[List]{
      override def tupleF[A, B](fa: List[A], fb: List[B]): List[(A, B)] = {
        fa.flatMap(a => fb.map(b => (a,b)))
      }
    }
    }
    }


    object Tupler {
 // суммонер
    def apply[F[_]](implicit T: Tupler[F[_]]): Tupler[F[_]] = T
    //метод для реализации получается не нужен - так идея подсказывает
    // def tupleF[F[_]: Tupler, A, B](fa: F[A], fb: F[B]): F[(A, B)] = Tupler[F].tupleF(fa,fb)
  }
}
