package ru.otus.module3.zio_homework
import zio.{Clock, Config, ConfigProvider, Console, Duration, IO, Random, Task, UIO, ZIO, ZLayer}
import ru.otus.module3.zioConcurrency
import zio.config.magnolia._


package object config {
   case class AppConfig(host: String, port: String)




  private val myConfigAutomatic: Config[AppConfig] = deriveConfig[AppConfig]

  object Configuration{
    val config: IO[Config.Error, AppConfig] = ConfigProvider.defaultProvider.load(myConfigAutomatic)
  }


  // Интерфейс сервиса
  trait DurationLogger {
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A]
  }

  // реализация сервиса через уже реализованную функцию
  case class DurationLoggerLive() extends DurationLogger {
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A] = printEffectRunningTime(zio)
  }
  // ZLayer  сервиса
  object DurationLogger {
    val live: ZLayer[Any, Nothing, DurationLogger] =
      ZLayer.succeed(DurationLoggerLive())

    // суммонер для сервиса
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with DurationLogger, E, A] =
      ZIO.serviceWithZIO[DurationLogger](_.printEffectRunningTime(zio))
  }
}
