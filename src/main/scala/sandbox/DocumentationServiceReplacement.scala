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

import cats.data.OptionT
import cats.implicits._

import scala.concurrent.Future

class DocumentationServiceReplacement {
  import DocumentationService._
  import scala.concurrent.ExecutionContext.Implicits.global

  def fetchApiDocumentationResource(serviceName: String, version: String, resource: String): Future[Result] = {

    def findMatchingVersion(apiDefinition: ExtendedApiDefinition): Option[ExtendedApiVersion] = apiDefinition.versions.find(_.version == version)

    def fetchApiVersion: Future[ExtendedApiVersion] = {
        OptionT(apiDefinitionService.fetchApiDefinition(serviceName))
          .mapFilter(findMatchingVersion)
          .getOrElseF(Future.failed[ExtendedApiVersion](new IllegalArgumentException("Version not found")))
    }

    def fetchResource(apiVersion: ExtendedApiVersion): Future[StreamedResponse] = {

      lazy val futureLocalResource = apiMicroserviceConnector.fetchApiDocumentationResource(serviceName, version, resource)
      lazy val futureRemoteResource = apiDocumentationConnector.fetchApiDocumentationResource(serviceName, version, resource)

      if (config.isSandbox) futureLocalResource else {
        apiVersion.sandboxAvailability.fold(futureLocalResource)(_ => futureRemoteResource)
      }
    }

    for {
      apiVersion <- fetchApiVersion
      streamedResponse <- fetchResource(apiVersion)
    } yield streamedResponse.headers.status match {
      case 200 => "Blobby Blobby"
      case 404 => throw new NotFoundException(s"$resource not found for $serviceName $version")
      case _ => throw new InternalServerException(s"Error downloading $resource for $serviceName $version")
    }
  }

}
