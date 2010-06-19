package be.romberg.liftweb.util

import net.liftweb.http._
import S._
import SHtml._
import scala.xml._

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import BindHelpers._

/**
 * Needs tinymce.js and tinymce_setup.js to be included on the form in order to be effective
 */
class BwMappedRichText[T<:Mapper[T]](fieldOwner: T) extends MappedText[T](fieldOwner) with HintedField[String, T] {

  lazy val whitespaceRx = """\s""".r
  override def cssClasses = List("richtext")

  def asHtmlBox: Box[NodeSeq] = {
    if ((is == null) || (is.length == 0))
      Empty
    else {
        var theHtml = is.toString
        if (!theHtml.startsWith("<html"))
          theHtml = "<html><body>" + theHtml + "</body></html>"

        PCDataXmlParser(theHtml).map(_.first.child.first.child)
    }
  }

  override def asHtml: Elem = asHtmlBox match {
    case Full(nodes) => <div>{nodes}</div>
    case Failure(msg,_,_) => <div>Failure: {msg}</div>
    case _ => <p>(Empty document)</p>
  }

  def asHtmlWithTemplate(content:NodeSeq) = asHtmlBox match {
    case Full(nodes) => bind("bind", content, "content" -> nodes)
    case Failure(msg,_,_) => bind("bind", content, "content" -> <div>{msg}</div>)
    case _ => NodeSeq.Empty
  }

  // only needed for project descriptions at the moment
  def inputInitialTextWhenEmpty = ""

  def inputInitialText : String =
    if ((is == null) || (is.length == 0))
      inputInitialTextWhenEmpty
    else is


  // Set an empty string when the user has not changed the initial text
  def filterUnchangedInitialText(in: String):String = {
    val noWsIn = whitespaceRx.replaceAllIn(in, "")
    val noWsInitial = whitespaceRx.replaceAllIn(inputInitialTextWhenEmpty, "")
    if (noWsIn == noWsInitial)
      ""
    else
      in
  }

  override def setFilter = filterUnchangedInitialText _ :: super.setFilter

  override def _toForm: Box[NodeSeq] = {
    // Specifying width and height explicitly because of TinyMCE bug when initially hiding the textarea
    // cf. http://stackoverflow.com/questions/685175/why-is-my-tinymce-hidden-textarea-acting-up
    S.fmapFunc({s: List[String] => this.setFromAny(s)}){funcName =>
      Full(<textarea name={funcName} lift:gc={funcName} rows={textareaRows.toString} cols={textareaCols.toString} style="width:710px; height:320px" id={fieldId}>{inputInitialText}</textarea>)
    }
  }

  def textareaRows  = 12

  def textareaCols = 60


}
