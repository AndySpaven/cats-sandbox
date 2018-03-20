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
import sandbox.DocumentationService.ExtendedApiVersion
import cats.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object DocumentationServiceReplacement2 {
  sealed trait DocumentationServiceResponse
  case class DocumentationFound(apiVersion: ExtendedApiVersion) extends DocumentationServiceResponse

  trait BadDocumentationServiceResponse extends DocumentationServiceResponse
  case class ServiceNotFound(serviceName: String, version: String, resource: String) extends BadDocumentationServiceResponse
  case class VersionNotFound(serviceName: String, version: String, resource: String) extends BadDocumentationServiceResponse
  case class ResourceNotFound(serviceName: String, version: String, resource: String) extends BadDocumentationServiceResponse
  case class ErrorDownloadingResource(serviceName: String, version: String, resource: String) extends BadDocumentationServiceResponse

  type EType = EitherT[Future, BadDocumentationServiceResponse, DocumentationServiceResponse]
  def asType[A](o: Option[A], otherwise: BadDocumentationServiceResponse)     = EitherT.fromOption[Future](o, otherwise)
  def asType[A](e: Either[BadDocumentationServiceResponse, A])                = EitherT.fromEither[Future](e)
  def asType[A](f: Future[Option[A]], error: BadDocumentationServiceResponse) = EitherT.fromOptionF(f, error)
  def asType[A](f: Future[A])                                                 = EitherT.right[BadDocumentationServiceResponse](f)
}



class DocumentationServiceReplacement2 {
  import DocumentationService._
  import DocumentationServiceReplacement2._


  def fetchApiDocumentationResource(serviceName: String, version: String, resource: String): Future[DocumentationServiceResponse] = {

    lazy val futureLocalResource = apiMicroserviceConnector.fetchApiDocumentationResource(serviceName, version, resource)
    lazy val futureRemoteResource = apiDocumentationConnector.fetchApiDocumentationResource(serviceName, version, resource)

    def getResource(apiVersion: ExtendedApiVersion): Future[StreamedResponse] =
      if (config.isSandbox)
        futureLocalResource
      else
        apiVersion.sandboxAvailability.fold(futureLocalResource)(_ => futureRemoteResource)


    def findMatchingVersion(apiDefinition: ExtendedApiDefinition): Option[ExtendedApiVersion] = apiDefinition.versions.find(_.version == version)

    def mapStatus(apiVersion: ExtendedApiVersion,r: StreamedResponse) : Either[BadDocumentationServiceResponse,DocumentationServiceResponse] = {
      r.headers.status match {
        case 200 => Right(DocumentationFound(apiVersion))
        case 404 => Left(ResourceNotFound(serviceName, version, resource))
        case _ => Left(ErrorDownloadingResource(serviceName, version, resource))
      }
    }

    (for {
      apiDefinition     <- asType( apiDefinitionService.fetchApiDefinition(serviceName), ServiceNotFound(serviceName,version,resource) )
      apiVersion        <- asType( findMatchingVersion(apiDefinition), VersionNotFound(serviceName,version,resource) )
      streamedResponse  <- asType( getResource(apiVersion) )
      response          <- asType( mapStatus(apiVersion,streamedResponse)  )
    } yield response)
    .value.map(e => e.fold(a => a, b => b))
  }
}
