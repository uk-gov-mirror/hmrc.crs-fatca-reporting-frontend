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
import connectors.UpscanConnector
import forms.UploadXMLFormProvider
import helpers.FakeUpscanConnector
import models.UserAnswers
import models.upscan.*
import org.mockito.ArgumentMatchers.{any, argThat}
import org.mockito.Mockito
import org.mockito.Mockito.{verify, when}
import pages.{FileReferencePage, UploadIDPage}
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import views.html.UploadXMLView

import scala.concurrent.Future

class IndexControllerSpec extends SpecBase {

  "Index Controller" - {

    val FileSize           = 20L
    val uploadId: UploadId = UploadId("12345")

    val fakeUpscanConnector: FakeUpscanConnector = app.injector.instanceOf[FakeUpscanConnector]

    "onPageLoad" - {

      "must return OK and the correct view for a GET" in {

        val userAnswers = emptyUserAnswers
          .withPage(UploadIDPage, UploadId("uploadId"))

        when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

        running(application) {
          val formProvider = new UploadXMLFormProvider()
          val form         = formProvider()

          val request = FakeRequest(GET, routes.IndexController.onPageLoad().url)

          val result = route(application, request).value

          val view = application.injector.instanceOf[UploadXMLView]

          status(result) mustEqual OK

          contentAsString(result) mustEqual view(form, UpscanInitiateResponse(Reference(""), "target", Map.empty))(request, messages(application)).toString

          verify(mockSessionRepository).set(argThat {
            ua =>
              ua.get(FileReferencePage).isDefined &&
              ua.get(UploadIDPage).isDefined
          })
        }
      }

      "must return REDIRECT and redirect to Index Controller" in {

        val userAnswers = emptyUserAnswers
          .withPage(UploadIDPage, UploadId("uploadId"))

        when(mockSessionRepository.set(any())).thenThrow(new RuntimeException("Failed"))

        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.IndexController.onPageLoad().url)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          redirectLocation(result) mustEqual Some("/report-for-crs-and-fatca/report/problem/there-is-a-problem")
        }
      }
    }

    "getStatus" - {
      "must read the progress of the upload from the backend" ignore {

        val request = FakeRequest(GET, routes.IndexController.getStatus(uploadId).url)

        def verifyResult(uploadStatus: UploadStatus, expectedResult: Option[String] = None): Unit = {

          val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
            .overrides(
              bind[UpscanConnector].toInstance(fakeUpscanConnector),
              bind[SessionRepository].toInstance(mockSessionRepository)
            )
            .build()

          fakeUpscanConnector.setStatus(uploadStatus)
          val result = route(application, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe expectedResult
          application.stop()
        }

        val invalidFileName = stringWithNCharacter(171).sample.get.concat(".xml")
        val validFileName   = stringWithNCharacter(170).sample.get.concat(".xml")

        verifyResult(InProgress, Some(routes.IndexController.getStatus(uploadId).url))
        verifyResult(Quarantined, Some(routes.IndexController.showError("virusfile", "", "").url))
        verifyResult(UploadRejected(ErrorDetails("REJECTED", "message")), Some(routes.IndexController.showError("invalidargument", "typemismatch", "").url))
        verifyResult(UploadRejected(ErrorDetails("REJECTED", "octet-stream")), Some(routes.IndexController.showError("octetstream", "rejected", "").url))
        verifyResult(Failed, Some(routes.IndexController.showError("UploadFailed", "", "").url))
        verifyResult(UploadedSuccessfully(validFileName, "downloadUrl", FileSize, "MD5:123"), Some(routes.FileValidationController.onPageLoad().url))
        verifyResult(
          UploadedSuccessfully(invalidFileName, "downloadUrl", FileSize, "MD5:123"),
          Some(routes.IndexController.showError("invalidargument", "invalidfilenamelength", "").url)
        )
        verifyResult(
          UploadedSuccessfully(validFileName, "downloadUrl", 0L, "MD5:123"),
          Some(routes.IndexController.showError("invalidargument", "fileisempty", "").url)
        )

      }

      "must show error when Upload Status none" in {

        val request = FakeRequest(GET, routes.IndexController.getStatus(uploadId).url)

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()
        fakeUpscanConnector.resetStatus()

        val result = route(application, request).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some(routes.IndexController.showError("UploadFailed", "", "").url)
        application.stop()
      }
    }

    "showError" - {
      "must show returned error when file size is more than 250mb - Upscan Error" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("EntityTooLarge", "", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("The selected file must be smaller than 250MB")
      }

      "must show returned error when file not selected - Upscan Error" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("octetstream", "rejected", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("Select a file")
      }

      "must show returned error when file is virus infected" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("VirusFile", "", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("The selected file contains a virus")
      }

      "must show returned error when file name length is more than 100 char" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("InvalidArgument", "InvalidFileNameLength", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("File name must be 100 characters or less and match the MessageRefId in the file")
      }

      "must show returned error when file name includes a disallowed character" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("InvalidArgument", "disallowedcharacters", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include(
          """File name must not include less than signs (&lt;), greater than signs (&gt;), colons (:), straight double quotes (&quot;), apostrophes (’), ampersands (&amp;), forward slashes (/), backslashes (\), vertical bars (|), question marks (?) or asterisks (*)"""
        )
      }

      "must show returned error when file size is zero kb - JS enabled flow" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("InvalidArgument", "FileIsEmpty", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("The selected file is empty")
      }

      "must show returned error when file type mismatch" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("InvalidArgument", "typeMismatch", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("The selected file must be an XML")
      }

      "must show returned error when file had invalid argument" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("InvalidArgument", "", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("Select a file")
      }

      "must show returned error when Unknown error" in {

        val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[UpscanConnector].toInstance(fakeUpscanConnector)
          )
          .build()

        val request = FakeRequest(GET, routes.IndexController.showError("UnknownError", "", "").url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("The selected file could not be uploaded")
      }
    }

  }
}
