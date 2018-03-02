package sandbox

import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object UsingEitherT extends App {
  import Common._

  def handle(maybeParams: Option[Map[String, Seq[String]]]): Future[Result] = {

    def getParam(params: Map[String, Seq[String]], key: String): Option[String] = {
      params
        .getOrElse(key,Seq.empty[String])
        .headOption
    }

    def noParams: Result = "No params provided for username or password"
    def noUsername: Result = "No username"
    def noPassword: Result = "No password"
    def noUserFound(username: String): Result = s"No user found for $username"

    def asET[A](o: Option[A], otherwise: Result) = EitherT.fromOption[Future](o, otherwise)

    (for {
      params        <- asET(maybeParams, noParams)
      username      <- asET(getParam(params, "username"), noUsername)
      password      <- asET(getParam(params, "password"), noPassword)
      user          <- EitherT.fromOptionF(serviceB.callSvc(username, password), noUserFound(username))
      session       <- EitherT.right[Result](serviceA.callSvc)
    } yield session)
    .fold[Result](x => x, s => sessionResult(s))
  }

  /**
    * Reduce load with helper methods and handling Params in common way
   */
  object Reuseable {

    def asET[A](o: Option[A], otherwise: Result) = EitherT.fromOption[Future](o, otherwise)

    def asET[A](e: Either[Error, A]) = EitherT.fromEither[Future](e)

    def asET[A](f: Future[Option[A]], error: Error) = EitherT.fromOptionF(f, error)

    def asET[A](f: Future[A]) = EitherT.right[A](f)

    implicit class ParamHandler(val maybeParams: Params) extends AnyVal {
      def diagnosticGet(key: String): Either[Error, String] = {
        for {
          params <- Either.fromOption(maybeParams, "No parameters provided")
          seq <- Either.fromOption(params.get(key), s"No parameter $key found")
          value <- Either.fromOption(seq.headOption, s"$key has empty parameter value")
        } yield value
      }
    }

  }

  def handle2(maybeParams: Params): Future[Result] = {
    import Reuseable._

     def noUserFound(username: String): Result = s"No user found for $username"

    (for {
      username      <- asET(maybeParams.diagnosticGet("username"))
      password      <- asET(maybeParams.diagnosticGet("password"))
      user          <- asET(serviceB.callSvc(username, password), noUserFound(username))
      session       <- asET(serviceA.callSvc)
    } yield session)
      .fold[Result](x => x, s => sessionResult(s))
  }
}