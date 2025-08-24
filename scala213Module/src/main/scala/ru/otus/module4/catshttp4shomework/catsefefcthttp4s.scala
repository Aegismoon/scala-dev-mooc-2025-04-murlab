package catshttp4s

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Ref}
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.circe.Json
import org.http4s.circe.jsonEncoder
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.{Http, HttpRoutes}
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

object GlobalState {

  val counterRef: Ref[IO, Int] = Ref[IO].of(0).unsafeRunSync()

}

//1
object Restfull {


  val service: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "hello" / name => Ok(name)
  }


  def streamChonker: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "slow" / chunk / total / time =>
      val chunkSize = chunk.toInt
      val totalSize = total.toLong
      val delay     = time.toInt.millis
      val byteStream =
        Files[IO].readAll(Path("data.txt"))
          .metered(delay)
          .take(totalSize)
          .chunkN(chunkSize)
          .flatMap(Stream.chunk)
      Ok(byteStream)    //.map(_.withContentType(`Content-Type`(MediaType.application.`octet-stream`)))
  }

  def serviceCounter: HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root/ "counter" => for {
      uCntr <- GlobalState.counterRef.updateAndGet(_ + 1)
      response <- Ok(Json.fromString(s"counter:$uCntr"))
    } yield response
  }

  val httpApp: Http[IO, IO] = serviceCounter.orNotFound

  val server1 = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8070).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(httpApp).build
  } yield s
}

object mainServer extends IOApp.Simple {
    def run(): IO[Unit] = {
      Restfull.server1.use(_ => IO.never)
  }
}

//2
object RestFull2EndPoints {
  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root/ "hello1" / name => Ok(s"web service from $name")
      case POST -> Root/ "hello2" / name => Ok(s"web service from $name")
    }

  val serviceTwo: HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root/ "hello2" / name => Ok("web service OK2")
    }
  }

  val service : HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root / "hello" / name => Ok("web service 0")
  }

  val router = Router(
    "/" -> serviceOne,
    "/api" -> serviceTwo,
    "/apiroot" -> service
  )

  val server = EmberServerBuilder
    .default[IO]
    .withPort(Port.fromInt(8080).get)
    .withHost(Host.fromString("localhost").get)
    .withHttpApp(router.orNotFound).build
}

object  mainServer2Services extends IOApp.Simple {
  def run(): IO[Unit] = {
    RestFull2EndPoints.server.use(_ => IO.never)
  }
}