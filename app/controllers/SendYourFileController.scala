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

import models.fileDetails.BusinessRuleErrorCode.{FATCARegimeIncorrect2, FailedSchemaValidationCrs, FailedSchemaValidationFatca}
import models.fileDetails.FileValidationErrors
import connectors.FileDetailsConnector
import controllers.actions.*
import models.GiinUpdateState.GinUpDateRequired
import models.ReportElectionState.*
import models.requests.DataRequest
import models.submission.*
import models.submission.fileDetails.{Accepted as FileStatusAccepted, NotAccepted, Pending, Rejected, RejectedSDES, RejectedSDESVirus}
import models.upscan.URL
import models.{FATCA, GiinUpdateState, ReportElectionState, SendYourFileAdditionalText, UserAnswers, ValidatedFileData}
import pages.*
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc.*
import play.api.mvc.Results.{InternalServerError, Redirect}
import repositories.SessionRepository
import services.{FileDetailsService, SubmissionService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.SendYourFileView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SendYourFileController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: SendYourFileView,
  submissionService: SubmissionService,
  sessionRepository: SessionRepository,
  fileDetailsConnector: FileDetailsConnector,
  fileDetailsService: FileDetailsService
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData) {
    implicit request =>
      request.userAnswers
        .get(ValidXMLPage)
        .fold(
          Redirect(controllers.routes.PageUnavailableController.onPageLoad().url)
        ) {
          validatedFileData =>
            val reportElections = ReportElectionState.from(request.userAnswers.get(ReportElectionsPage))
            val giinUpdate      = GiinUpdateState.from(validatedFileData.messageSpecData.giin)

            val viewContent = (validatedFileData.messageSpecData.messageType, giinUpdate, reportElections) match {
              case (FATCA, GinUpDateRequired, ElectionsReported)    => SendYourFileAdditionalText.BOTH
              case (FATCA, GinUpDateRequired, ElectionsNotReported) => SendYourFileAdditionalText.GIIN
              case (_, _, ElectionsReported)                        => SendYourFileAdditionalText.ELECTIONS
              case _                                                => SendYourFileAdditionalText.NONE
            }

            Ok(view(viewContent, validatedFileData.messageSpecData.reportType))
        }
  }

  def onSubmit(): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      request.userAnswers.get(ValidXMLPage) match {
        case Some(_) =>
          submissionService.submitElectionsAndGiin(request.userAnswers).flatMap {
            status =>
              navigateBasedOnStatus(status, request.userAnswers)
          }
        case _ =>
          Future.successful(Redirect(routes.PageUnavailableController.onPageLoad()))
      }
  }

  private def navigateBasedOnStatus(status: GiinAndElectionStatus, ua: UserAnswers)(implicit request: DataRequest[_]): Future[Result] =
    status match {
      case giinUpdateFailed: GiinUpdateFailed =>
        val dbStatus = GiinAndElectionDBStatus(giinUpdateFailed.giinStatus, giinUpdateFailed.electionStatus)
        for {
          updatedAnswers <- Future.fromTry(ua.set(GiinAndElectionStatusPage, dbStatus))
          _              <- sessionRepository.set(updatedAnswers)
        } yield Redirect(routes.GiinNotSentController.onPageLoad())

      case electionFailed: ElectionsSubmitFailed =>
        val dbStatus = GiinAndElectionDBStatus(electionFailed.giinStatus, electionFailed.electionStatus)
        for {
          updatedAnswers <- Future.fromTry(ua.set(GiinAndElectionStatusPage, dbStatus))
          _              <- sessionRepository.set(updatedAnswers)
        } yield Redirect(routes.ElectionsNotSentController.onPageLoad())

      case GiinAndElectionSubmittedSuccessful =>
        handleBothSubmitted(ua)
    }

  private def handleBothSubmitted(answers: UserAnswers)(implicit request: DataRequest[_]): Future[Result] =
    (answers.get(ValidXMLPage), answers.get(URLPage), answers.get(UploadIDPage), answers.get(FileReferencePage)) match {
      case (Some(ValidatedFileData(fileName, messageSpecData, fileSize, checksum)), Some(fileUrl), Some(uploadId), Some(fileReference)) =>
        val submissionDetails = SubmissionDetails(fileName, uploadId, request.fatcaId, fileSize, fileUrl, checksum, messageSpecData, fileReference)

        submissionService.submitDocument(submissionDetails) flatMap {
          case Some(conversationId: ConversationId) =>
            for {
              userAnswers <- Future.fromTry(request.userAnswers.set(ConversationIdPage, conversationId))
              _           <- sessionRepository.set(userAnswers)
            } yield Redirect(controllers.routes.StillCheckingYourFileController.onPageLoad())
          case _ =>
            Future.successful(InternalServerError)
        }
      case _ =>
        Future.successful(InternalServerError)
    }

  def getStatus: Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      (request.userAnswers.get(ValidXMLPage), request.userAnswers.get(ConversationIdPage)) match {
        case (Some(xmlDetails), Some(conversationId)) =>
          fileDetailsConnector.getStatus(conversationId) flatMap {
            case Some(FileStatusAccepted) =>
              Future.successful(Ok(Json.toJson(URL(routes.FileConfirmationController.onPageLoad(conversationId.value).url))))
            case Some(Pending) =>
              Future.successful(NoContent)
            case Some(Rejected) =>
              fileDetailsService.getFileDetails(conversationId) flatMap {
                case Some(fileDetails) => handleRejectedWithErrors(fileDetails.errors, conversationId, xmlDetails.messageSpecData.messageType.toString)
                case None              => Future.successful(Ok(Json.toJson(URL(routes.JourneyRecoveryController.onPageLoad().url))))
              }
            case Some(RejectedSDESVirus) =>
              Future.successful(Ok(Json.toJson(URL(routes.VirusFoundController.onPageLoad().url))))
            case Some(RejectedSDES) =>
              Future.successful(Ok(Json.toJson(URL(routes.JourneyRecoveryController.onPageLoad().url))))
            case Some(NotAccepted) =>
              Future.successful(Ok(Json.toJson(URL(routes.FileNotAcceptedController.onPageLoad(xmlDetails.messageSpecData.messageType.toString).url))))
            case None =>
              logger.warn("getStatus: no status returned")
              Future.successful(InternalServerError)
          }
        case _ =>
          request.userAnswers.get(GiinAndElectionStatusPage) match {
            case Some(giinAndElectionStatus) =>
              if (!giinAndElectionStatus.giinStatus) {
                Future.successful(Ok(Json.toJson(URL(routes.GiinNotSentController.onPageLoad().url))))
              } else if (!giinAndElectionStatus.electionStatus) {
                Future.successful(Ok(Json.toJson(URL(routes.ElectionsNotSentController.onPageLoad().url))))
              } else {
                logger.warn("UserAnswers.ConversationId is empty")
                Future.successful(InternalServerError)
              }
            case None =>
              logger.warn("UserAnswers.ConversationId is empty")
              Future.successful(InternalServerError)
          }
      }
  }

  private def handleRejectedWithErrors(errors: Option[FileValidationErrors], conversationId: ConversationId, regime: String): Future[Result] = {
    val notAcceptedErrorCodes = Set(FailedSchemaValidationCrs, FailedSchemaValidationFatca, FATCARegimeIncorrect2)
    val isNotAcceptedRecord = errors
      .flatMap(_.recordError)
      .getOrElse(Nil)
      .exists(
        e => notAcceptedErrorCodes(e.code)
      )

    val isNotAccepted = errors
      .flatMap(_.fileError)
      .getOrElse(Nil)
      .exists(
        e => notAcceptedErrorCodes(e.code)
      )

    if (isNotAccepted || isNotAcceptedRecord)
      Future.successful(Ok(Json.toJson(URL(routes.FileNotAcceptedController.onPageLoad(regime).url))))
    else
      Future.successful(Ok(Json.toJson(URL(routes.RulesErrorController.onPageLoad(conversationId.value).url))))
  }
}
