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

import base.SpecBase
import config.FrontendAppConfig
import connectors.FileDetailsConnector
import models.CRSReportType.NewInformation
import models.fileDetails.BusinessRuleErrorCode.{FailedSchemaValidationCrs, FailedSchemaValidationFatca}
import models.fileDetails.{FileErrors, FileValidationErrors}
import models.submission.ConversationId
import models.submission.fileDetails.*
import models.{CRS, FATCA, MessageSpecData, UserAnswers}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalactic.Prettifier.default
import pages.{ConversationIdPage, ValidXMLPage}
import play.api.inject
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import services.FileDetailsService
import uk.gov.hmrc.http.HeaderCarrier
import viewmodels.FileCheckViewModel
import views.html.StillCheckingYourFileView

import scala.concurrent.{ExecutionContext, Future}

class StillCheckingYourFileControllerSpec extends SpecBase {

  val mockFileDetailsConnector: FileDetailsConnector = mock[FileDetailsConnector]
  val mockFileDetailsService: FileDetailsService     = mock[FileDetailsService]
  lazy val pageUnavailableUrl: String                = controllers.routes.PageUnavailableController.onPageLoad().url
  val hardcodedFiName                                = "testFiName"
  val exampleGiin                                    = "8Q298C.00000.LE.340"
  val conversationId: ConversationId                 = ConversationId("conversationId")
  val messageSpecData: MessageSpecData               = getMessageSpecData(CRS, fiNameFromFim = hardcodedFiName, reportType = NewInformation)

  val ua: UserAnswers =
    emptyUserAnswers.withPage(ValidXMLPage, getValidatedFileData(messageSpecData))

  "StillCheckingYourFIle Controller" - {

    "must redirect to file passed check controller when file status is accepted" in {
      val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)

      val application = applicationBuilder(userAnswers = Some(validUserAnswers))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
        )
        .build()

      when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(Some(Accepted)))

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.FilePassedChecksController.onPageLoad().url

      }
    }

    "must return OK and the still checking file view when file status is pending" in {
      val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)

      val application = applicationBuilder(userAnswers = Some(validUserAnswers))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
        )
        .build()

      val appConfig = application.injector.instanceOf[FrontendAppConfig]

      when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(Some(Pending)))

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[StillCheckingYourFileView]

        val messagesApi         = messages(application)
        val expectedSummaryList = FileCheckViewModel.createFileSummary(messageSpecData.messageRefId, "Pending")(messagesApi)

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(expectedSummaryList, appConfig.signOutUrl, messageSpecData.isFiUser, messageSpecData.fiNameFromFim)(
          request,
          messages(application)
        ).toString
      }
    }

    "must redirect to Journey Recovery  when file status is RejectedSDES" in {
      val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)

      val application = applicationBuilder(userAnswers = Some(validUserAnswers))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
        )
        .build()

      when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(Some(RejectedSDES)))

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to virus found when file status is RejectedSDESVirus" in {
      val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)

      val application = applicationBuilder(userAnswers = Some(validUserAnswers))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
        )
        .build()

      when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(Some(RejectedSDESVirus)))

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.VirusFoundController.onPageLoad().url
      }
    }

    "must return internal server error when file status returned is None" in {
      val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)

      val application = applicationBuilder(userAnswers = Some(validUserAnswers))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
        )
        .build()

      when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(None))

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }

    "must return Internal Server Error when conversationId is missing from user answers" in {
      val application = applicationBuilder(userAnswers = Some(ua))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }

    "must redirect to file checks failed when file status is Rejected with no schema errors" in {
      val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)
      val fileDetails      = getTestFileDetails(status = Rejected, errors = Some(FileValidationErrors(fileError = None, recordError = None)))

      val application = applicationBuilder(userAnswers = Some(validUserAnswers))
        .overrides(
          bind[FileDetailsConnector].toInstance(mockFileDetailsConnector),
          bind[FileDetailsService].toInstance(mockFileDetailsService)
        )
        .build()

      when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(Some(Rejected)))
      when(mockFileDetailsService.getFileDetails(any[ConversationId])(any[HeaderCarrier](), any[ExecutionContext]()))
        .thenReturn(Future.successful(Some(fileDetails)))

      running(application) {
        val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.FileFailedChecksController.onPageLoad().url
      }
    }

    "must redirect to file-not-accepted" - {
      "when file status is NotAccepted" in {
        val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)

        val application = applicationBuilder(userAnswers = Some(validUserAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(NotAccepted)))

        running(application) {
          val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
          val result  = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.FileNotAcceptedController.onPageLoad(CRS.toString).url
        }
      }
      "when file status is Rejected with CRS schema validation error" in {
        val validUserAnswers = ua.withPage(ConversationIdPage, conversationId)
        val fileDetails = getTestFileDetails(
          status = Rejected,
          errors = Some(FileValidationErrors(fileError = Some(Seq(FileErrors(FailedSchemaValidationCrs, None))), recordError = None))
        )

        val application = applicationBuilder(userAnswers = Some(validUserAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector),
            bind[FileDetailsService].toInstance(mockFileDetailsService)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(Rejected)))
        when(mockFileDetailsService.getFileDetails(any[ConversationId])(any[HeaderCarrier](), any[ExecutionContext]()))
          .thenReturn(Future.successful(Some(fileDetails)))

        running(application) {
          val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
          val result  = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.FileNotAcceptedController.onPageLoad(CRS.toString).url
        }
      }

      "when file status is Rejected with FATCA schema validation error" in {
        val messageSpecData  = getMessageSpecData(FATCA, fiNameFromFim = hardcodedFiName, reportType = NewInformation)
        val validUserAnswers = emptyUserAnswers.withPage(ValidXMLPage, getValidatedFileData(messageSpecData)).withPage(ConversationIdPage, conversationId)
        val fileDetails = getTestFileDetails(
          status = Rejected,
          errors = Some(FileValidationErrors(fileError = Some(Seq(FileErrors(FailedSchemaValidationFatca, None))), recordError = None))
        )

        val application = applicationBuilder(userAnswers = Some(validUserAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector),
            bind[FileDetailsService].toInstance(mockFileDetailsService)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(Rejected)))
        when(mockFileDetailsService.getFileDetails(any[ConversationId])(any[HeaderCarrier](), any[ExecutionContext]()))
          .thenReturn(Future.successful(Some(fileDetails)))

        running(application) {
          val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
          val result  = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.FileNotAcceptedController.onPageLoad(FATCA.toString).url
        }
      }

      "when file status is Rejected with FATCA 51 error" in {
        val messageSpecData  = getMessageSpecData(FATCA, fiNameFromFim = hardcodedFiName, reportType = NewInformation)
        val validUserAnswers = emptyUserAnswers.withPage(ValidXMLPage, getValidatedFileData(messageSpecData)).withPage(ConversationIdPage, conversationId)
        val fileDetails = getTestFileDetails(
          status = Rejected,
          errors = Some(FileValidationErrors(fileError = Some(Seq(FileErrors(FailedSchemaValidationFatca, None))), recordError = None))
        )

        val application = applicationBuilder(userAnswers = Some(validUserAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector),
            bind[FileDetailsService].toInstance(mockFileDetailsService)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(Rejected)))
        when(mockFileDetailsService.getFileDetails(any[ConversationId])(any[HeaderCarrier](), any[ExecutionContext]()))
          .thenReturn(Future.successful(Some(fileDetails)))

        running(application) {
          val request = FakeRequest(GET, routes.StillCheckingYourFileController.onPageLoad().url)
          val result  = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.FileNotAcceptedController.onPageLoad(FATCA.toString).url
        }
      } //

    }
  }
}
