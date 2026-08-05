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
import connectors.FileDetailsConnector
import controllers.actions.*
import models.fileDetails.BusinessRuleErrorCode.{FATCARegimeIncorrect2, FailedSchemaValidationCrs, FailedSchemaValidationFatca}
import models.fileDetails.FileValidationErrors
import models.submission.fileDetails.{Accepted as FileStatusAccepted, NotAccepted, Pending, Rejected, RejectedSDES, RejectedSDESVirus}
import pages.{ConversationIdPage, ValidXMLPage}
import play.api.i18n.Lang.logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import services.FileDetailsService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.FileCheckViewModel.createFileSummary
import views.html.{StillCheckingYourFileView, ThereIsAProblemView}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class StillCheckingYourFileController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: StillCheckingYourFileView,
  errorView: ThereIsAProblemView,
  frontendAppConfig: FrontendAppConfig,
  fileDetailsConnector: FileDetailsConnector,
  fileDetailsService: FileDetailsService
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      (request.userAnswers.get(ValidXMLPage), request.userAnswers.get(ConversationIdPage)) match {
        case (Some(xmlDetails), Some(conversationId)) =>
          fileDetailsConnector.getStatus(conversationId) flatMap {
            case Some(FileStatusAccepted) =>
              Future.successful(Redirect(routes.FilePassedChecksController.onPageLoad()))
            case Some(Pending) =>
              val messageSpecData = xmlDetails.messageSpecData
              Future.successful(
                Ok(
                  view(createFileSummary(messageSpecData.messageRefId, "Pending"),
                       frontendAppConfig.signOutUrl,
                       messageSpecData.isFiUser,
                       messageSpecData.fiNameFromFim
                  )
                )
              )
            case Some(RejectedSDES) =>
              Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
            case Some(RejectedSDESVirus) =>
              Future.successful(Redirect(routes.VirusFoundController.onPageLoad()))
            case Some(Rejected) =>
              fileDetailsService.getFileDetails(conversationId) flatMap {
                case Some(fileDetails) => handleRejectedWithErrors(fileDetails.errors, xmlDetails.messageSpecData.messageType.toString)
                case None              => Future.successful(InternalServerError(errorView()))
              }
            case Some(NotAccepted) =>
              Future.successful(Redirect(routes.FileNotAcceptedController.onPageLoad(xmlDetails.messageSpecData.messageType.toString)))
            case None =>
              Future.successful(InternalServerError(errorView()))
          }
        case _ =>
          logger.warn("Unable to retrieve fileName & conversationId")
          Future.successful(InternalServerError(errorView()))
      }

  }

  private def handleRejectedWithErrors(errors: Option[FileValidationErrors], regime: String): Future[Result] = {
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
      Future.successful(Redirect(routes.FileNotAcceptedController.onPageLoad(regime)))
    else
      Future.successful(Redirect(routes.FileFailedChecksController.onPageLoad()))
  }

}
