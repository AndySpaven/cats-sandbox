import sandbox._
import sandbox.Common._

import scala.collection.immutable.{Map, Seq}
import scala.concurrent.Await
import scala.concurrent.duration._


val okParams: Params = Some(Map("username" -> Seq("Bob", "Fred"), "password" -> Seq("Secret")))
val noUsernameParams: Params = Some(Map[String,Seq[String]]("password" -> Seq("Secret")))
val findNoUserParams: Params = Some(Map[String,Seq[String]]("username" -> Seq("Bob"),"password" -> Seq("Bob")))
val noneOfTheNeededParams : Params = Some(Map("username" -> Seq()))


Await.result(UsingOptionT.handle(okParams), 5.seconds)

Await.result(UsingOptionT.handle(noUsernameParams), 5.seconds)

Await.result(UsingOptionT.handle(findNoUserParams), 5.seconds)

Await.result(UsingOptionT.handle(None), 5.seconds)

//-----------------

Await.result(UsingEitherTFirstCut.handle(okParams), 5.seconds)

Await.result(UsingEitherTFirstCut.handle(noUsernameParams), 5.seconds)

Await.result(UsingEitherTFirstCut.handle(findNoUserParams), 5.seconds)

Await.result(UsingEitherTFirstCut.handle(None), 5.seconds)

//-----------------

Await.result(UsingEitherT.handle(okParams), 5.seconds)

Await.result(UsingEitherT.handle(noUsernameParams), 5.seconds)

Await.result(UsingEitherT.handle(findNoUserParams), 5.seconds)

Await.result(UsingEitherT.handle(None), 5.seconds)

//-----------------

Await.result(UsingEitherTApplicativeErrors.handle(okParams), 5.seconds)

Await.result(UsingEitherTApplicativeErrors.handle(noUsernameParams), 5.seconds)

Await.result(UsingEitherTApplicativeErrors.handle(findNoUserParams), 5.seconds)

Await.result(UsingEitherTApplicativeErrors.handle(None), 5.seconds)

Await.result(UsingEitherTApplicativeErrors.handle(noneOfTheNeededParams), 5.seconds)


import cats.implicits._
import cats.Show
implicit val showError: Show[UsingEitherTApplicativeErrors.Reuseable.Error] = Show.show(e => e.msg)
implicit val showErrors: Show[UsingEitherTApplicativeErrors.Reuseable.Errors] = Show.show(nel => nel.toList.mkString("Errors: (", ",", ")"))

UsingEitherTApplicativeErrors.Reuseable.reduceParams(okParams, List("username", "password")).show

UsingEitherTApplicativeErrors.Reuseable.reduceParams(Some(Map("fred"-> Seq("Bob"))), List("username", "password")).show

UsingEitherTApplicativeErrors.Reuseable.reduceParams(noUsernameParams, List("username", "password")).show

UsingEitherTApplicativeErrors.Reuseable.reduceParams(None, List("username", "password")).show
