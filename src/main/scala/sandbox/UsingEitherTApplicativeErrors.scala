package sandbox


import cats.Monoid
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
    type ErrorOr[A] = Either[Error, A]
    
    type Errors = NonEmptyList[Error]
    type ErrorsOr[A] = Either[Errors,A]

    def one(msg: String): Errors = NonEmptyList.one(Error(msg))

    def left[A](msg: String) : ErrorsOr[A] = Either.left[Errors, A](one(msg))

    implicit def monoidForErrorsOr[B](implicit B: Monoid[B]): Monoid[ErrorsOr[B]] =
      new Monoid[ErrorsOr[B]] {
        def empty: ErrorsOr[B] =
          Right(B.empty)
        def combine(x: ErrorsOr[B], y: ErrorsOr[B]): ErrorsOr[B] =
          (x,y) match {
            case (Right(r1), Right(r2))     => Right(r1.combine(r2))
            case (Left(l1), Left(l2))     => Left(l1.combine(l2))
            case (Left(l1), _)             => Left(l1)
            case (_, Left(l2))             => Left(l2)
          }
      }

    type ErrorsOrT[A] = EitherT[Future, Errors, A]

    def asEither[A](o: Option[A], orElse: String): ErrorsOr[A] = Either.fromOption(o, one(orElse))

    def asET[A](a: A): ErrorsOrT[A] = EitherT.right(Future.successful(a))

    def asET[A](o: Option[A], otherwise: Errors): ErrorsOrT[A] = EitherT.fromOption(o, otherwise)

    def asET[A](f: Future[A]): ErrorsOrT[A] = EitherT.right(f)

    def asET[A](f: Future[Option[A]], errors: Errors) = EitherT.fromOptionF(f, errors)

    def asET[A](e: Either[Errors, A]): ErrorsOrT[A] = EitherT.fromEither(e)


    type ErrorsOrParams = ErrorsOr[Map[String,String]]

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
        left[Map[String,String]]("No parameters provided")
      )( params =>
         Monoid.combineAll(keys.map(params.diagnosticGet))
      )
    }
  }

  def handle(maybeParams: Params): Future[Result] = {
    import Reuseable._

    def noUserFound(username: String): String = s"No user found for $username"

    def asErrorResult(errors: Errors) : Result = errors.map(_.msg).reduceLeft(_ ++ "," ++ _)

    (for {
      paramsWeNeed  <- asET( reduceParams(maybeParams, List("username", "password")))
      username      <- asET( paramsWeNeed("username"))
      password      <- asET( paramsWeNeed("password"))
      user          <- asET( serviceB.callSvc(username, password), one(noUserFound(username)))
      session       <- asET( serviceA.callSvc)
    } yield session)
      .fold[Result](asErrorResult, sessionResult)
  }
}