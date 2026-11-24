package com.example.quickstart


import cats.Applicative
import cats.effect.Concurrent
import cats.effect.kernel.MonadCancelThrow
import cats.effect.Concurrent
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.implicits._
import org.http4s.client.{Client, UnexpectedStatus}
import cats.syntax.all._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.circe._
import org.http4s.Method._
import org.http4s.ember.client.EmberClient

object EmberClientHotfix {

  /** Remove this once it is fixed upstream.
    */
  implicit class ExpectWithBodyStreamDrain[F[_]](client: Client[F])(implicit F: MonadCancelThrow[F]) {

    def expect_[A](req: Request[F])(implicit d: EntityDecoder[F, A]): F[A] =
      client.expectOr(req)(drainBodyStreamOnError(req))

    def expect_[A](uri: Uri)(implicit d: EntityDecoder[F, A]): F[A] =
      client.expectOr(uri)(drainBodyStreamOnError(Request[F](uri = uri)))

    def expectOption_[A](req: Request[F])(implicit d: EntityDecoder[F, A]): F[Option[A]] =
      client.expectOptionOr(req)(drainBodyStreamOnError(req))

    def expectOption_[A](uri: Uri)(implicit d: EntityDecoder[F, A]): F[Option[A]] = {
      val req = Request[F](uri = uri)
      client.expectOptionOr(req)(drainBodyStreamOnError(req))
    }

    def drainBodyStreamOnError(req: Request[F])(resp: Response[F])(implicit G: Applicative[F]): F[Throwable] = F match {
      case concurrentF: Concurrent[F @unchecked] =>
        implicit val C: Concurrent[F] = concurrentF
        resp.body.compile.drain.as(UnexpectedStatus(resp.status, req.method, req.uri))

      case _ =>
        F.pure(UnexpectedStatus(resp.status, req.method, req.uri))
    }
  }
}

trait Jokes[F[_]]{
  def get: F[Jokes.Joke]
}

object Jokes {
  import EmberClientHotfix._

  def apply[F[_]](implicit ev: Jokes[F]): Jokes[F] = ev

  final case class Joke(joke: String) extends AnyVal
  object Joke {
    implicit val jokeDecoder: Decoder[Joke] = deriveDecoder[Joke]
    implicit def jokeEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, Joke] =
      jsonOf
    implicit val jokeEncoder: Encoder[Joke] = deriveEncoder[Joke]
    implicit def jokeEntityEncoder[F[_]]: EntityEncoder[F, Joke] =
      jsonEncoderOf
  }

  final case class JokeError(e: Throwable) extends RuntimeException

  def impl[F[_]: Concurrent](C: Client[F]): Jokes[F] = new Jokes[F]{
    val dsl = new Http4sClientDsl[F]{}
    import dsl._
    def get: F[Jokes.Joke] = {
      //C.expect_[Joke](GET(uri"http://localhost:8181/hello/foo"))
      C.expect[Joke](GET(uri"http://localhost:8181/hello/foo"))
    }
  }
}
