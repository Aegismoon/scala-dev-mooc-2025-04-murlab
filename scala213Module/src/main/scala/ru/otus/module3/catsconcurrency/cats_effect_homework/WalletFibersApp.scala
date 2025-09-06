package catsconcurrency.cats_effect_homework


import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def printBalance(wallet1:Wallet[IO],wallet2:Wallet[IO], wallet3:Wallet[IO]) ={
      wallet1.balance.map(v=> s"wallet 1 balance:${v}").flatMap(IO.println) *>
      wallet2.balance.map(v=> s"wallet 2 balance:${v}").flatMap(IO.println) *>
      wallet3.balance.map(v=> s"wallet 3 balance:${v}").flatMap(IO.println)
  }


  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      fiber1 <- (IO.sleep(100.millis) *> wallet1.topup(100)).iterateWhile(_ => true).start
      fiber2 <- (IO.sleep(500.millis) *> wallet2.topup(100)).iterateWhile(_ => true).start
      fiber3 <- (IO.sleep(2000.millis) *> wallet3.topup(100)).iterateWhile(_ => true).start
      fiber4 <- (IO.sleep(1000.millis) *> printBalance(wallet1,wallet2,wallet3)).iterateWhile(_ => true).start
      _ <- IO.readLine
      _ <- fiber1.cancel
      _ <- fiber2.cancel
      _ <- fiber3.cancel
      _ <- fiber4.cancel
    } yield ()

}
