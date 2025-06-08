package ru.otus

import ru.otus.module1.{concurrency, executors, future}
import ru.otus.module1.concurrency.{getRatesLocation1, getRatesLocation2, printRunningTime}
import ru.otus.module2.murat_higher_kinded_types.TuplerSyntax.TuplerMurSyntax
import ru.otus.module2.murat_higher_kinded_types.{Tupler, TuplerSyntax}
import ru.otus.module2.{implicits, type_classes}

import scala.util.{Failure, Success}


object App {
  def main(args: Array[String]): Unit = {

    val optionsTupled: Option[(Int, String)] = Tupler[Option].tupleF(Some(35), Some("Murat"))
    println(optionsTupled)  // Some((35,Murat))

    val listsTupled = Tupler[List].tupleF(List(1, 2), List("a", "b"))
    println(listsTupled)  // List((1,a), (1,b), (2,a), (2,b))

    // вот тут сработало если первым ставть Option
    // если пробовать делать F[_] ковариантным, то надо весь трейт переписывать
    val muratSyntaxOptTupler = Option("Tree") *%%*  Some("27")
    println(muratSyntaxOptTupler)
  }
}