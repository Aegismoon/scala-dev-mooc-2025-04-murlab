package catshttp4s

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Ref, Resource}
import cats.implicits.toSemigroupKOps
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.circe.Json
import org.http4s.circe._
import org.http4s.FormDataDecoder.formEntityDecoder
import org.http4s.circe.jsonEncoder
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.server.Router
import org.http4s.{Http, HttpApp, HttpRoutes, MediaType, Method, Request, Response, Uri}
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
      Ok(byteStream,`Content-Type`(MediaType.application.`octet-stream`))
  }

  def serviceCounter: HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root/ "counter" => for {
      uCntr <- GlobalState.counterRef.updateAndGet(_ + 1)
      response <- Ok(Json.fromString(s"counter:$uCntr"))
    } yield response
  }

  val routes: HttpRoutes[IO] =
    serviceCounter <+> streamChonker

  val httpCombinedApp: HttpApp[IO] =
    routes.orNotFound

  val server1 = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8070).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(httpCombinedApp).build
  } yield s
}

object mainServer extends IOApp.Simple {
    def run(): IO[Unit] = {
      Restfull.server1.use(_ => IO.never)
  }
}

object HttpClientLaunchTest extends IOApp.Simple {
  val builder: Resource[IO, Client[IO]] =
    EmberClientBuilder.default[IO].build

  private val request = Request[IO](
    method = Method.GET,
    uri    = Uri.unsafeFromString("http://localhost:8070/slow/4/12/0")
  )

  val result: Resource[IO, Response[IO]] = for {
    client   <- builder
    response <- client.run(request)
  } yield response

  val result1: Resource[IO, String] = for {
    client   <- builder
    strBody  <- Resource.eval(client.expect[String](request))
  } yield strBody

  val result3: IO[String] =
    builder.use(client =>
      client.run(request).use { resp =>
        if (resp.status.isSuccess)
          resp.body.compile.to(Array).map(a => s"bytes:${a.length}")
        else
          IO.pure("error")
      }
    )

  private val request2 = Request[IO](
    method = Method.GET,
    uri    = Uri.unsafeFromString("http://localhost:8070/counter")
  )


  val result2: Resource[IO, Response[IO]] = for {
    client   <- builder
    response <- client.run(request)
  } yield response

  val result21: Resource[IO, String] = for {
    client <- builder
    json   <- Resource.eval(client.expect[Json](request))
  } yield json.asString.getOrElse("")


  val result23: IO[String] =
    builder.use { client =>
      client.run(request).use { resp =>
        if (resp.status.isSuccess)
          resp.as[Json].map(_.asString.getOrElse(""))
        else
          IO.pure("error")
      }
    }

  // -------- main --------
  def run: IO[Unit] = for {
    _ <- result.use(r => IO.println(s"/slow result status=${r.status}"))
    _ <- result1.use(b => IO.println(s"/slow result1 body=${b.take(40)}..."))
    _ <- result3.flatMap(r => IO.println(s"/slow result3 = $r"))

    _ <- result2.use(r => IO.println(s"/counter result status=${r.status}"))
    _ <- result21.use(b => IO.println(s"/counter result21 body=$b"))
    _ <- result23.flatMap(r => IO.println(s"/counter result23 = $r"))
  } yield ()
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