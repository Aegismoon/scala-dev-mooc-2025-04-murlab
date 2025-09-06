package catsconcurrency.cats_effect_homework

import cats.effect.{IO, Sync}
import cats.implicits._
import Wallet._
import cats.effect.IO.pure

import java.nio.charset.StandardCharsets
import java.nio.file.Files._
import java.nio.file.{Files, Paths, StandardOpenOption}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  private val filename = s"$id.wallet"
  private val walletDir = Paths.get("wallets")
  private val walletFile = walletDir.resolve(filename)

  private def readBalanceFromFile: F[BigDecimal]= Sync[F].delay{
    val balanceString = new String(Files.readAllBytes(walletFile),StandardCharsets.UTF_8)
    BigDecimal(balanceString)
  }
  //запись баланса в файл с переписыванием
  private def writeBalanceToFile(balance:BigDecimal): F[Unit] = {
    Sync[F].delay{
      Files.write(walletFile,balance.toString.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.TRUNCATE_EXISTING)
    }
  }
  def balance: F[BigDecimal] = readBalanceFromFile
  def topup(amount: BigDecimal): F[Unit] = readBalanceFromFile.flatMap(balance => writeBalanceToFile( balance + amount))

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
    readBalanceFromFile.flatMap{balance =>
      if (amount > balance) Sync[F].pure(Left(BalanceTooLow))
      else writeBalanceToFile(balance - amount).map(_ => Right(()))
    }

  }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = for{
    wallet <- Sync[F].delay{print(s"Make a new fileWallet id ${id}")} *> initFileSystem(id).as(new FileWallet[F](id))
  } yield wallet

  // инициация папки с кощельками
  private def initFileSystem[F[_]: Sync](id: WalletId): F[Unit] =
    Sync[F].delay {
      val walletDir = Paths.get("wallets")
      if (!Files.exists(walletDir)) Files.createDirectories(walletDir)
      val walletFile = walletDir.resolve(s"$id.wallet")
      if (!Files.exists(walletFile)) {
        Files.write(walletFile, "0".getBytes(StandardCharsets.UTF_8))
      }
    }

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
