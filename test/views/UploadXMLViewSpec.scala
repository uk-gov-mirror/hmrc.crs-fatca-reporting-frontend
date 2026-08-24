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

package views

import base.SpecBase
import forms.UploadXMLFormProvider
import models.upscan.{Reference, UpscanInitiateResponse}
import org.jsoup.Jsoup
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Form
import play.api.i18n.{Lang, Messages}
import play.api.mvc.{AnyContent, MessagesControllerComponents}
import play.api.test.{FakeRequest, Injecting}
import play.twirl.api.HtmlFormat
import utils.ViewHelper
import views.html.UploadXMLView

class UploadXMLViewSpec extends SpecBase with GuiceOneAppPerSuite with Injecting with ViewHelper {

  val view1: UploadXMLView                                              = app.injector.instanceOf[UploadXMLView]
  val formProvider                                                      = new UploadXMLFormProvider()
  val form                                                              = formProvider()
  val messagesControllerComponentsForView: MessagesControllerComponents = app.injector.instanceOf[MessagesControllerComponents]

  implicit private val request: FakeRequest[AnyContent] = FakeRequest()
  implicit private val messages: Messages               = messagesControllerComponentsForView.messagesApi.preferred(Seq(Lang("en")))

  "UploadXMLView" - {
    "should render page components" in {
      val renderedHtml: HtmlFormat.Appendable =
        view1(form, UpscanInitiateResponse(Reference(""), "target", Map.empty))
      lazy val doc = Jsoup.parse(renderedHtml.body)

      getWindowTitle(doc) must include("Upload an XML file for CRS or FATCA")
      getPageHeading(doc) mustEqual "Upload an XML file for CRS or FATCA"
      getAllParagraph(doc).text() must include("We will automatically check the formatting of the file and send you to another page once completed.")
      elementText(doc, "#submit") mustEqual "Continue"
      val elems = getAllElements(doc, "a[href*='/manage-your-crs-and-fatca-financial-institutions']")
      elems.size() mustBe 1
      elems.text().trim mustBe "Send a CRS or FATCA report"
    }

    "should render page components with error" in {
      val formWithErrors: Form[String] = form.withError("file-upload", "uploadFile.error.file.size.large")
      val renderedHtml: HtmlFormat.Appendable =
        view1(formWithErrors, UpscanInitiateResponse(Reference(""), "target", Map.empty))
      lazy val doc = Jsoup.parse(renderedHtml.body)

      getAllParagraph(doc).text() must include("The selected file must be smaller than 250MB")
      getWindowTitle(doc) must include("Error: Upload an XML file for CRS or FATCA")
    }
  }

}
