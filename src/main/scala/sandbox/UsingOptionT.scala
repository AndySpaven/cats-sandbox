/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sandbox

import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import sandbox.Common._

object UsingOptionT {

  def handle(maybeParams: Params): Future[Result] = {
    def noSessionResult: Result = {
      "No Session"
    }

    def extractParam(maybeParams: Params, key: String): Option[String] = {
      maybeParams
        .getOrElse(Map.empty)
        .getOrElse(key,Seq.empty[String])
        .headOption
    }

    // a helper to make the first two lines nicer
    def asOptT[A](o: Option[A]): OptionT[Future, A] = OptionT.fromOption[Future](o)

    def fAsOptT[A](f: Future[A]): OptionT[Future, A] = OptionT.liftF(f)

    (for {
      username      <- asOptT(extractParam(maybeParams, "username"))
      password      <- asOptT(extractParam(maybeParams, "password"))
      user          <- OptionT(serviceB.callSvc(username, password))
      session       <- fAsOptT(serviceA.callSvc)
    } yield session)
      .cata(noSessionResult, s => sessionResult(s))
  }


}
