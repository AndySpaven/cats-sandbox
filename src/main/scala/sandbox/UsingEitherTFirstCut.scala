package sandbox

import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object UsingEitherTFirstCut extends App {
  import Common._
  import Reuseable._

  def handle(maybeParams: Option[Map[String, Seq[String]]]): Future[Result] = {

    def getParam(params: Map[String, Seq[String]], key: String): Option[String] = {
      params
        .getOrElse(key,Seq.empty[String])
        .headOption
    }

    val noParams: Result = "No params provided for username or password"
    val noUsername: Result = "No username"
    val noPassword: Result = "No password"
    def noUserFound(username: String): Result = s"No user found for $username"

    (for {
      params        <- asET(maybeParams, noParams)
      username      <- asET(getParam(params, "username"), noUsername)
      password      <- asET(getParam(params, "password"), noPassword)
      user          <- asET(serviceB.callSvc(username, password), noUserFound(username))
      session       <- asET(serviceA.callSvc)
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

    def asET[A](f: Future[A]) = EitherT.right[Error](f)
  }
}