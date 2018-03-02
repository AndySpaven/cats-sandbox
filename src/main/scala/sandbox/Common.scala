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

import sandbox.ServiceA.Session

import scala.collection.immutable.{Map, Seq}

object Common {

  type Params = Option[Map[String, Seq[String]]]

  type Error = String
  type Result = String

  lazy val serviceA = new ServiceA
  lazy val serviceB = new ServiceB

  def sessionResult(session : Session): Result = {
    "Session : " + session
  }

}
