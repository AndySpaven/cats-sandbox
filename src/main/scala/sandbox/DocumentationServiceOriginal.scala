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

import scala.concurrent.Future

object DocumentationService {
  trait ApiDefinitionService {
    def fetchApiDefinition(serviceName: String): Future[Option[ExtendedApiDefinition]]
  }

  trait ApiMicroserviceConnector {
    def fetchApiDocumentationResource(serviceName: String, version: String, resource: String): Future[StreamedResponse]
  }

  trait ApiDocumentationConnector {
    def fetchApiDocumentationResource(serviceName: String, version: String, resource: String): Future[StreamedResponse]
  }

  trait ServiceConfiguration {
    def isSandbox: Boolean
  }

  type Result = Object

  trait ExtendedApiDefinition {
    def versions: Seq[ExtendedApiVersion]
  }

  trait ExtendedApiVersion {
    def sandboxAvailability: Option[ApiAvailability]

    def version: String
  }

  trait WSReponseHeaders {
    def headers: Map[String,String]
    def status: Int
  }

  trait StreamedResponse {
    def headers: WSReponseHeaders
  }

  trait ApiAvailability {

  }

  case class NotFoundException(str: String) extends RuntimeException(str)
  case class InternalServerException(str: String) extends RuntimeException(str)

  val apiDefinitionService: ApiDefinitionService = ???
  val apiMicroserviceConnector: ApiMicroserviceConnector = ???
  val apiDocumentationConnector: ApiDocumentationConnector = ???
  val config: ServiceConfiguration = ???

}

class DocumentationService {
  import DocumentationService._
  import scala.concurrent.ExecutionContext.Implicits.global

  def fetchApiDocumentationResource(serviceName: String, version: String, resource: String): Future[Result] = {

    def fetchApiVersion: Future[ExtendedApiVersion] = {
      apiDefinitionService.fetchApiDefinition(serviceName).map {
        case Some(definition) => definition.versions.find(_.version == version)
        case _ => None
      }.flatMap {
        case Some(v) => Future.successful(v)
        case _ => Future.failed[ExtendedApiVersion](new IllegalArgumentException("Version not found"))
      }
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