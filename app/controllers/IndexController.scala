/*
 * Copyright 2025 HM Revenue & Customs
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

package controllers

import config.FrontendAppConfig
import connectors.UpscanConnector
import controllers.Execution.trampoline
import controllers.actions.{DataCreationAction, DataRequiredAction, DataRetrievalAction, IdentifierAction}
import forms.UploadXMLFormProvider
import models.ErrorCode.*
import models.InvalidArgumentErrorMessage.{DisallowedCharacters, FileIsEmpty, InvalidFileNameLength, TypeMismatch}
import models.requests.DataRequest
import models.upscan.*
import models.{ErrorCode, InvalidArgumentErrorMessage}
import org.apache.pekko
import org.apache.pekko.actor.ActorSystem
import pages.{FileReferencePage, PollingCountPage, UploadIDPage}
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.i18n.Lang.logger
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.{FileUploadPendingView, UploadXMLView}

import javax.inject.Inject
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class IndexController @Inject() (
  val controllerComponents: MessagesControllerComponents,
  sessionRepository: SessionRepository,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  setData: DataCreationAction,
  requireData: DataRequiredAction,
  actorSystem: ActorSystem,
  config: FrontendAppConfig,
  upscanConnector: UpscanConnector,
  formProvider: UploadXMLFormProvider,
  view: UploadXMLView,
  uploadPendingView: FileUploadPendingView
) extends FrontendBaseController
    with I18nSupport {

  def onPageLoad(): Action[AnyContent] = (identify andThen getData andThen setData).async {
    implicit request =>
      val form = formProvider()
      toResponse(form)
  }

  def showError(errorCode: String, errorMessage: String, errorRequestId: String): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      val form = formProvider()

      val formWithErrors: Form[String] = ErrorCode.fromCode(errorCode) match {
        case Some(ErrorCode.EntityTooLarge) => form.withError("file-upload", "uploadFile.error.file.size.large")
        case Some(VirusFile)                => form.withError("file-upload", "uploadFile.error.file.content.virus")
        case Some(InvalidArgument | OctetStream) =>
          InvalidArgumentErrorMessage.fromMessage(errorMessage) match {
            case Some(InvalidFileNameLength) => form.withError("file-upload", "uploadFile.error.file.name.length")
            case Some(DisallowedCharacters)  => form.withError("file-upload", "uploadFile.error.file.name.disallowed.characters")
            case Some(TypeMismatch)          => form.withError("file-upload", "uploadFile.error.file.type.invalid")
            case Some(FileIsEmpty)           => form.withError("file-upload", "uploadFile.error.file.content.empty")
            case None                        => form.withError("file-upload", "uploadFile.error.file.select")
          }
        case _ =>
          logger.warn(s"Upscan error $errorCode: $errorMessage, requestId is $errorRequestId")
          form.withError("file-upload", "uploadFile.error.file.content.unknown")
      }
      toResponse(formWithErrors)
  }

  private def toResponse(preparedForm: Form[String])(implicit request: DataRequest[AnyContent], hc: HeaderCarrier): Future[Result] = {
    val uploadId: UploadId = UploadId.generate
    (for {
      upscanInitiateResponse <- upscanConnector.getUpscanFormData(uploadId)
      uploadId               <- upscanConnector.requestUpload(uploadId, upscanInitiateResponse.fileReference)
      updatedAnswers <- Future.fromTry(
        request.userAnswers
          .set(UploadIDPage, uploadId)
          .flatMap(_.set(FileReferencePage, upscanInitiateResponse.fileReference))
      )
      _ <- sessionRepository.set(updatedAnswers)
    } yield Ok(view(preparedForm, upscanInitiateResponse)))
      .recover {
        case e: Exception =>
          logger.error("UploadFileController: An exception occurred when contacting Upscan ", e)
          Redirect(routes.JourneyRecoveryController.onPageLoad())
      }
  }

  def getStatus(uploadId: UploadId): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      val refreshThreshHold = 5;
      val pollingCount      = request.userAnswers.get(PollingCountPage).getOrElse(0)
      //What is missing deleting the PollingCountPage when we redirect to a new page.

      // Delay the call to make sure the backend db has been populated by the upscan callback first
      pekko.pattern.after(config.upscanCallbackDelayInSeconds.seconds, actorSystem.scheduler) {
        upscanConnector.getUploadStatus(uploadId) map {
          case Some(uploadedSuccessfully: UploadedSuccessfully) =>
            if (isFileNameInValid(uploadedSuccessfully.name)) {
              Redirect(routes.IndexController.showError(InvalidArgument.code, InvalidFileNameLength.message, "").url)
            } else if (isFileEmpty(uploadedSuccessfully.size)) {
              Redirect(routes.IndexController.showError(InvalidArgument.code, FileIsEmpty.message, "").url)
            } else {
              Redirect(routes.FileValidationController.onPageLoad().url)
            }
          case Some(r: UploadRejected) =>
            if (r.details.message.contains("octet-stream")) {
              logger.warn(s"Show errorForm on rejection $r")
              val errorReason = r.details.failureReason
              Redirect(routes.IndexController.showError(OctetStream.code, errorReason.toLowerCase, "").url)
            } else {
              logger.warn(s"Upload rejected. Error details: ${r.details}")
              Redirect(routes.IndexController.showError(InvalidArgument.code, TypeMismatch.message, "").url)
            }
          case Some(Quarantined) =>
            Redirect(routes.IndexController.showError(VirusFile.code, "", "").url)
          case Some(Failed) =>
            logger.warn("File upload returned failed status")
            Redirect(routes.IndexController.showError("UploadFailed", "", "").url)
          case Some(_) =>
            if (pollingCount <= refreshThreshHold) {
              val count = pollingCount + 1
              Future
                .fromTry(request.userAnswers.set(PollingCountPage, count))
                .flatMap(
                  u => sessionRepository.set(u)
                )
              Redirect(routes.IndexController.getStatus(uploadId).url)
            } else {
              Ok(uploadPendingView(uploadId))
            }
          // Ok(uploadPendingView(uploadId))
          // Redirect(routes.IndexController.getStatus(uploadId).url)
          case None =>
            logger.error("Unable to retrieve file upload status from Upscan")
            Redirect(routes.IndexController.showError("UploadFailed", "", "").url)
        }
      }
  }

  private def isFileNameInValid(name: String): Boolean = name.stripSuffix(".xml").length > config.upscanMaxFileNameLength

  private def isFileEmpty(size: Long): Boolean = size == 0L
}

//if (pollingCount <= refreshThreshHold) {
//  val count = pollingCount + 1
//  Future.fromTry(request.userAnswers.set(PollingCountPage, count)).flatMap(u => sessionRepository.set(u))
//  Redirect(routes.IndexController.getStatus(uploadId).url)
//} else {
//  Ok(uploadPendingView(uploadId))
//}
