package ru.otus.module3

import zio.{Clock, Console, Duration, IO, Random, Task, UIO, ZIO}

import scala.language.postfixOps
import ru.otus.module3.zio_homework.config._
import zio.Console.printLine

import scala.concurrent.duration.DurationInt
import zioConcurrency._

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  // эффект-рандом
  val generateRandomNumber: UIO[Int] =
    Random.nextIntBetween(1, 4)

  // эффект с консоли
  val readUserGuess: Task[Int] =
    Console.readLine("Угадайте число от 1 до 3: ").flatMap { input =>
      ZIO.attempt(input.toInt).catchSome {
        // если нет числа, то переспросить про число
        case _: NumberFormatException =>
          Console.printLine("Ошибка: Введите число!").zipRight(readUserGuess)
      }
    }


  // Эффект проверки совпадения
  def checkGuess(target: Int, guess: Int): Task[Unit] =
    if (guess equals target)
      Console.printLine(s"Угаданно! Загаданное число: $target")
  else
  Console.printLine(s"Неудача. Загаданное число: $target")

  lazy val guessProgram: Task[Unit] =  for {
    target <- generateRandomNumber
    guess  <- readUserGuess
    _      <- checkGuess(target, guess)
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[E, A](effect: IO[E, A])(stopCondition: A => Boolean): IO[E, A] =
    effect.repeatUntilZIO(a => ZIO.succeed(!stopCondition(a)))

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */

  // Дефолтная конфигурация
  private val murdefaultConfig: AppConfig =
    AppConfig(host = "localhost", port = "8080")

  def loadConfigOrDefault = Configuration
    .config.tapBoth(
      error => Console.printLine(s"Ошибка загрузки: $error"),
      config => Console.printLine(s"Конфиг загружен: $config")
    )
    .catchAll { error =>
      Console.printLine(s"Конфиг по умолчанию: $murdefaultConfig из-за ошибки: $error")
        .orDie  // Исключение или ошибка
        .as(murdefaultConfig)
    }


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
    // выбрал zipRight потому что результат не нужен
  lazy val eff: ZIO[Any, Nothing, Int] = ZIO.sleep(Duration.fromSeconds(1)) zipRight Random.nextIntBetween(0, 11)


  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app =     printEffectRunningTime {
    for {
      // вычисление эффекта последоватнльно
      results <- ZIO.foreach(effects)(identity)
      sum = results.sum
      _ <- printLine(s"Sum: $sum")
    } yield sum
  }


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */


  // нашел такой вариант - параллельно
  lazy val appSpeedUp = printEffectRunningTime {ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _) }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */
  //  в package object  config

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = ZIO.serviceWithZIO[DurationLogger] { logger =>
    DurationLogger.printEffectRunningTime {
      ZIO.collectAllPar(effects).map(_.sum)
        .flatMap(sum => printLine(s"Sum: $sum"))
    }
  }
  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provide(
    DurationLogger.live
  )


}
