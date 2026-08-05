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
import connectors.FileDetailsConnector
import models.CRSReportType.NewInformation
import models.fileDetails.BusinessRuleErrorCode.{FATCARegimeIncorrect2, FailedSchemaValidationCrs, FailedSchemaValidationFatca}
import models.fileDetails.{FileErrors, FileValidationErrors, RecordError}
import models.requests.DataRequest
import models.submission.*
import models.submission.fileDetails.*
import models.upscan.{Reference, UploadId}
import models.{CRS, CRSReportType, FATCA, FATCAReportType, MessageSpecData, SendYourFileAdditionalText, UserAnswers}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.*
import org.mockito.Mockito.when
import org.scalatest.BeforeAndAfterEach
import pages.*
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import services.{FileDetailsService, SubmissionService}
import uk.gov.hmrc.http.HeaderCarrier
import views.html.SendYourFileView

import scala.concurrent.{ExecutionContext, Future}

class SendYourFileControllerSpec extends SpecBase with BeforeAndAfterEach {

  val mockSubmissionService: SubmissionService       = mock[SubmissionService]
  val mockFileDetailsConnector: FileDetailsConnector = mock[FileDetailsConnector]
  val mockFileDetailsService: FileDetailsService     = mock[FileDetailsService]
  lazy val pageUnavailableUrl: String                = controllers.routes.PageUnavailableController.onPageLoad().url
  lazy val sendYourFileUrl: String                   = routes.SendYourFileController.onPageLoad().url
  val hardcodedFiName                                = "testFiName"
  val exampleGiin                                    = "8Q298C.00000.LE.340"
  val conversationId: ConversationId                 = ConversationId("conversationId")
  val messageSpecData: MessageSpecData               = getMessageSpecData(CRS, fiNameFromFim = hardcodedFiName, reportType = NewInformation)

  val messageSpecDataFatca: MessageSpecData =
    getMessageSpecData(giin = None, messageType = FATCA, fiNameFromFim = hardcodedFiName, reportType = FATCAReportType.TestData)

  val ua: UserAnswers =
    emptyUserAnswers.withPage(ValidXMLPage, getValidatedFileData(messageSpecData))

  "SendYourFile Controller" - {

    "onPageLoad" - {
      "must return OK and the correct view for a GET for CRS and election not required" in {
        val ua: UserAnswers = emptyUserAnswers
          .withPage(
            ValidXMLPage,
            getValidatedFileData(
              messageSpecData.copy(electionsRequired = false)
            )
          )
          .withPage(ReportElectionsPage, false)

        val application = applicationBuilder(userAnswers = Some(ua)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[SendYourFileView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view(SendYourFileAdditionalText.NONE, messageSpecData.reportType)(request, messages(application)).toString
        }
      }

      "must return OK and the correct view for a GET for FATCA and election not required and giin present" in {
        val messageSpecDataLoc = getMessageSpecData(giin = Some("some-giin"),
                                                    electionsRequired = false,
                                                    messageType = FATCA,
                                                    fiNameFromFim = hardcodedFiName,
                                                    reportType = FATCAReportType.TestData
        )
        val ua: UserAnswers = emptyUserAnswers
          .withPage(
            ValidXMLPage,
            getValidatedFileData(
              messageSpecDataLoc
            )
          )
          .withPage(ReportElectionsPage, false)

        val application = applicationBuilder(userAnswers = Some(ua)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[SendYourFileView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view(SendYourFileAdditionalText.NONE, messageSpecDataLoc.reportType)(request, messages(application)).toString
        }
      }

      "must return OK and the correct view for a GET for CRS and election required" in {
        val ua: UserAnswers = emptyUserAnswers
          .withPage(ValidXMLPage, getValidatedFileData(messageSpecData.copy(electionsRequired = true)))
          .withPage(ReportElectionsPage, true)

        val application = applicationBuilder(userAnswers = Some(ua)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[SendYourFileView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view(SendYourFileAdditionalText.ELECTIONS, messageSpecData.reportType)(request, messages(application)).toString
        }
      }

      "must return OK and the correct view for a GET for FATCA and election required and giin present" in {
        val messageSpecDataLoc =
          getMessageSpecData(giin = Some("some-giin"), messageType = FATCA, fiNameFromFim = hardcodedFiName, reportType = FATCAReportType.TestData)
        val ua: UserAnswers = emptyUserAnswers
          .withPage(
            ValidXMLPage,
            getValidatedFileData(
              messageSpecDataLoc
            )
          )
          .withPage(ReportElectionsPage, true)

        val application = applicationBuilder(userAnswers = Some(ua)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[SendYourFileView]

          status(result) mustEqual OK

          contentAsString(result) mustEqual view(SendYourFileAdditionalText.ELECTIONS, messageSpecDataLoc.reportType)(request, messages(application)).toString
        }
      }

      "must return OK and the correct view for a GET for FATCA and election required and giin needs to be updated" in {
        val ua: UserAnswers = emptyUserAnswers
          .withPage(
            ValidXMLPage,
            getValidatedFileData(messageSpecDataFatca)
          )
          .withPage(ReportElectionsPage, true)

        val application = applicationBuilder(userAnswers = Some(ua)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[SendYourFileView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view(SendYourFileAdditionalText.BOTH, messageSpecDataFatca.reportType)(request, messages(application)).toString
        }
      }

      "must return OK and the correct view for a GET for FATCA and election not required and giin needs to be updated" in {
        val ua: UserAnswers = emptyUserAnswers
          .withPage(
            ValidXMLPage,
            getValidatedFileData(messageSpecDataFatca)
          )
          .withPage(ReportElectionsPage, false)

        val application = applicationBuilder(userAnswers = Some(ua)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[SendYourFileView]

          status(result) mustEqual OK
          contentAsString(result) mustEqual view(SendYourFileAdditionalText.GIIN, messageSpecDataFatca.reportType)(request, messages(application)).toString
        }
      }

      "must redirect to page unavailable when xml valid page is not present in user answers for a GET" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        running(application) {
          val request = FakeRequest(GET, sendYourFileUrl)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual pageUnavailableUrl
        }
      }
    }

    "onSubmit" - {
      "must redirect to PageUnavailableController when validXmlPage is missing" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        running(application) {
          val request = FakeRequest(POST, routes.SendYourFileController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.PageUnavailableController.onPageLoad().url
        }
      }

      "must redirect to giin not sent when the giin update fails" in {
        val application = applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[SubmissionService].toInstance(mockSubmissionService)
          )
          .build()

        when(mockSubmissionService.submitElectionsAndGiin(any[UserAnswers])(using any[DataRequest[_]], any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(GiinUpdateFailed(false, true)))

        running(application) {
          val request = FakeRequest(POST, routes.SendYourFileController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.GiinNotSentController.onPageLoad().url
        }
      }

      "must redirect to election not sent when the election submission fails" in {
        val application = applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[SubmissionService].toInstance(mockSubmissionService)
          )
          .build()

        when(mockSubmissionService.submitElectionsAndGiin(any[UserAnswers])(using any[DataRequest[_]], any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(ElectionsSubmitFailed(true, false)))

        running(application) {
          val request = FakeRequest(POST, routes.SendYourFileController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.ElectionsNotSentController.onPageLoad().url
        }
      }

      "must return an internal server error when required data is missing for a successful submission" in {
        val incompleteUA = emptyUserAnswers.withPage(
          ValidXMLPage,
          getValidatedFileData(getMessageSpecData(CRS, fiNameFromFim = hardcodedFiName, reportType = NewInformation)).copy(fileName = "")
        )

        val application = applicationBuilder(userAnswers = Some(incompleteUA))
          .overrides(
            bind[SubmissionService].toInstance(mockSubmissionService)
          )
          .build()

        when(mockSubmissionService.submitElectionsAndGiin(any[UserAnswers])(using any[DataRequest[_]], any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(GiinAndElectionSubmittedSuccessful))

        running(application) {
          val request = FakeRequest(POST, routes.SendYourFileController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must return internal server error when submission service returns None conversation Id" in {
        val validUserAnswers = ua
          .withPage(URLPage, "http://test-url.com")
          .withPage(FileReferencePage, Reference("fileRef123"))
          .withPage(UploadIDPage, UploadId("uploadId123"))

        val application = applicationBuilder(userAnswers = Some(validUserAnswers))
          .overrides(
            bind[SubmissionService].toInstance(mockSubmissionService)
          )
          .build()

        when(mockSubmissionService.submitDocument(any[SubmissionDetails]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(None))

        when(mockSubmissionService.submitElectionsAndGiin(any[UserAnswers])(using any[DataRequest[_]], any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(GiinAndElectionSubmittedSuccessful))

        running(application) {
          val request = FakeRequest(POST, routes.SendYourFileController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must redirect to StillCheckingYourFileController for a successful submission" in {
        val validUserAnswers = ua
          .withPage(URLPage, "http://test-url.com")
          .withPage(FileReferencePage, Reference("fileRef123"))
          .withPage(UploadIDPage, UploadId("uploadId123"))

        val application = applicationBuilder(userAnswers = Some(validUserAnswers))
          .overrides(
            bind[SubmissionService].toInstance(mockSubmissionService)
          )
          .build()

        when(mockSubmissionService.submitDocument(any[SubmissionDetails]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(conversationId)))

        when(mockSubmissionService.submitElectionsAndGiin(any[UserAnswers])(using any[DataRequest[_]], any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(GiinAndElectionSubmittedSuccessful))

        running(application) {
          val request = FakeRequest(POST, routes.SendYourFileController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.StillCheckingYourFileController.onPageLoad().url
        }
      }
    }

    "getStatus" - {
      "must return OK and return file passed checks url when file status is accepted" in {

        val userAnswers = ua
          .withPage(ConversationIdPage, conversationId)

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(Accepted)))

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/file-confirmation/conversationId\"}"
        }
      }

      "must return OK and return no content when status is Pending" in {

        val userAnswers = ua
          .withPage(ConversationIdPage, conversationId)

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(Pending)))

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual NO_CONTENT
        }
      }

      "must return OK and return virus found when status is RejectedSDESVirus" in {

        val userAnswers = ua
          .withPage(ConversationIdPage, conversationId)

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(RejectedSDESVirus)))

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/virus-found\"}"
        }
      }

      "must return OK and return journey recovery url when status is RejectedSDES" in {

        val userAnswers = ua
          .withPage(ConversationIdPage, conversationId)

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(Some(RejectedSDES)))

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/there-is-a-problem\"}"
        }
      }

      "must return OK and FileNotAccepted url" - {
        "when status is NotAccepted" in {

          val userAnswers = ua
            .withPage(ConversationIdPage, conversationId)

          val application = applicationBuilder(userAnswers = Some(userAnswers))
            .overrides(
              bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
            )
            .build()

          when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
            .thenReturn(Future.successful(Some(NotAccepted)))

          running(application) {
            val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

            val result = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/file-not-accepted?regime=CRS\"}"
          }
        }

        "when file status is Rejected with FailedSchemaValidationCrs" in {
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
            val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)
            val result  = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/file-not-accepted?regime=CRS\"}"
          }
        }

        "when file status is Rejected with FailedSchemaValidationFatca" in {
          val validUserAnswers =
            emptyUserAnswers.withPage(ValidXMLPage, getValidatedFileData(messageSpecDataFatca)).withPage(ConversationIdPage, conversationId)
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
            val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)
            val result  = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/file-not-accepted?regime=FATCA\"}"
          }
        }

      }

      "when file status is Rejected with fatca business rule 51" in {
        val validUserAnswers =
          emptyUserAnswers.withPage(ValidXMLPage, getValidatedFileData(messageSpecDataFatca)).withPage(ConversationIdPage, conversationId)
        val fileDetails = getTestFileDetails(
          status = Rejected,
          errors = Some(FileValidationErrors(fileError = None, recordError = Some(Seq(RecordError(FATCARegimeIncorrect2, None, None)))))
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
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/file-not-accepted?regime=FATCA\"}"
        }
      }

      "must return internal server error when status is None" in {

        val userAnswers = ua
          .withPage(ConversationIdPage, conversationId)

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[FileDetailsConnector].toInstance(mockFileDetailsConnector)
          )
          .build()

        when(mockFileDetailsConnector.getStatus(any[ConversationId]())(using any[HeaderCarrier], any[ExecutionContext]))
          .thenReturn(Future.successful(None))

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must return json pointing to giin not sent to url when missing a conversation Id and contains GiinAndElectionStatusPage with value has giinstatus false" in {
        val giinAndElectionStatus = GiinAndElectionDBStatus(false, true)

        val userAnswers = ua
          .withPage(GiinAndElectionStatusPage, giinAndElectionStatus)

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/giin-not-sent\"}"
        }
      }

      "must return json pointing to election not sent to url when missing a conversation Id and contains GiinAndElectionStatusPage with value has giinstatus true and election status false" in {
        val giinAndElectionStatus = GiinAndElectionDBStatus(true, false)

        val userAnswers = ua
          .withPage(GiinAndElectionStatusPage, giinAndElectionStatus)

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result).toString mustEqual "{\"url\":\"/report-for-crs-and-fatca/report/problem/elections-not-sent\"}"
        }
      }

      "must return internal server error when conversation Id is missing" in {
        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

        running(application) {
          val request = FakeRequest(GET, routes.SendYourFileController.getStatus().url)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }
  }
}
