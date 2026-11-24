package com.example.quickstart

import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s._
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.client.UnexpectedStatus
import org.http4s.client.middleware.{Logger => ClientLogger}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder

class ClientErrorBodyLoggingSpec extends CatsEffectSuite {
  import EmberClientHotfix._

  private val errorBody = """{"message":"Hello, foo"}"""

  private val stubServer: Resource[IO, Uri] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"0")
      .withHttpApp(HttpApp[IO](_ => IO.pure(Response[IO](Status.BadGateway).withEntity(errorBody))))
      .build
      .map(_.baseUri)

  /** Runs `call` against a stub server that always answers 502 with `errorBody`, through the same
    * client logging middleware the app uses, and returns the failure plus the captured log lines.
    */
  private def callAndCaptureLogs(
      call: (org.http4s.client.Client[IO], Uri) => IO[Jokes.Joke]
  ): IO[(Throwable, List[String])] =
    stubServer.flatMap(uri => EmberClientBuilder.default[IO].build.map(uri -> _)).use { case (uri, client) =>
      for {
        logs <- Ref.of[IO, Vector[String]](Vector.empty)
        logged = ClientLogger[IO](logHeaders = true, logBody = true, logAction = Some((line: String) => logs.update(_ :+ line)))(client)
        outcome <- call(logged, uri / "hello" / "foo").attempt
        failure <- outcome.fold(IO.pure, joke => IO.raiseError(new AssertionError(s"expected a failure, got $joke")))
        lines <- logs.get
      } yield (failure, lines.toList)
    }

  private def assertBadGatewayLoggedWithBody(result: (Throwable, List[String])): Unit = {
    val (failure, lines) = result
    failure match {
      case UnexpectedStatus(status, _, _) => assertEquals(status, Status.BadGateway)
      case other                          => fail(s"expected UnexpectedStatus, got $other")
    }
    val responseLine = lines.find(_.contains("502"))
    assert(responseLine.isDefined, s"no 502 response was logged: $lines")
    assert(
      responseLine.exists(_.contains(errorBody)),
      s"response log line should contain the error body, got: $responseLine"
    )
  }

  test("expect logs the body of an error response") {
    callAndCaptureLogs((c, uri) => c.expect[Jokes.Joke](uri)).map(assertBadGatewayLoggedWithBody)
  }

  test("expect_ (EmberClientHotfix) logs the body of an error response") {
    callAndCaptureLogs((c, uri) => c.expect_[Jokes.Joke](uri)).map(assertBadGatewayLoggedWithBody)
  }
}
