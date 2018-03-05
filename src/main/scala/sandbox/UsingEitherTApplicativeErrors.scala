package sandbox


import cats.Semigroup
import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object UsingEitherTApplicativeErrors extends App {
  import Common._

  /**
    * Reduce load with helper methods and handling Params in common way
   */
  object Reuseable {

    case class Error(val msg: String) extends AnyVal

    type Errors = NonEmptyList[Error]
    type ErrorsOr[A] = Either[Errors,A]
    type ErrorsOrT[A] = EitherT[Future, Errors, A]



    type ErrorsOrParams = ErrorsOr[Map[String,String]]

    def left(l: Errors): ErrorsOrParams = Either.left(l)

    def right(r: Map[String,String]): ErrorsOrParams = Either.right(r)

    implicit val semigroupEither = new Semigroup[ErrorsOrParams] {
      override def combine(x: ErrorsOrParams, y: ErrorsOrParams): ErrorsOrParams = {
        (x, y) match {
          case (Left(er1), Left(er2))     => left(er1 concat er2)
          case (Left(er1), _)             => left(er1)
          case (_, Left(er2))             => left(er2)
          case (Right(r1), Right(r2))     => right(r1 ++ r2)
        }
      }
    }

    def one(msg: String): Errors = NonEmptyList.one(Error(msg))

    def asEither[A](o: Option[A], orElse: String): ErrorsOr[A] = Either.fromOption(o, one(orElse))

    def asET[A](a: A): ErrorsOrT[A] = EitherT.right(Future.successful(a))

    def asET[A](o: Option[A], otherwise: Errors): ErrorsOrT[A] = EitherT.fromOption(o, otherwise)

    def asET[A](f: Future[A]): ErrorsOrT[A] = EitherT.right(f)

    def asET[A](f: Future[Option[A]], errors: Errors) = EitherT.fromOptionF(f, errors)

    def asET[A](e: Either[Errors, A]): ErrorsOrT[A] = EitherT.fromEither(e)

    implicit class ParamHandler(val params: Map[String,Seq[String]]) extends AnyVal {
      def diagnosticGet(key: String): ErrorsOrParams = {
        for {
          seq     <- asEither(params.get(key), s"No parameter $key found")
          value   <- asEither(seq.headOption, s"$key has empty parameter value")
        } yield Map(key -> value)
      }
    }

    def reduceParams(maybeParams: Params, keys: List[String]): ErrorsOrParams = {
      maybeParams.fold(
        left(one("No parameters provided"))
      )( params =>
        keys
          .map(params.diagnosticGet)
          .fold(right(Map()))(semigroupEither.combine)
      )
    }
  }

  def handle(maybeParams: Params): Future[Result] = {
    import Reuseable._

    def noUserFound(username: String): String = s"No user found for $username"

    def asErrorResult(errors: Errors) : Result = errors.map(_.msg).reduceLeft(_ ++ "," ++ _)

    (for {
      paramsWeNeed  <- asET(reduceParams(maybeParams, List("username", "password")))
      username      <- asET(paramsWeNeed("username"))
      password      <- asET(paramsWeNeed("password"))
      user          <- asET(serviceB.callSvc(username, password), one(noUserFound(username)))
      session       <- asET(serviceA.callSvc)
    } yield session)
      .fold[Result](asErrorResult, sessionResult)
  }
}