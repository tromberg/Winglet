package be.romberg.liftweb.util

import net.liftweb._
import http._
import mapper._
import net.liftweb.util._

import scala.xml._
import BindHelpers._

/**
 * Extended Bind functionality to reduce template and binding code for binding a template to a Mapper
 *
 * Usage:
 *
 * <h3>1. Template</h3>
 *
 * <p>Simple usage:</p>
 *
 * <code>
 * <lift:MySnippet.method>
 * <show:field1/>
 * <show:field2/>
 * </lift:MySnippet.method>
 * </code>
 *
 * <p>For forms, use</p>
 * <code><input:field1/></code>
 * <p>instead of</p>
 * <code><show:field1/></code>
 * 
 * The uniqueFieldId for all fields must exist!
 *
 * 
 *
 */
trait MBindHelper {
  private val _currentNode = new ThreadGlobal[Elem]
  private val _currentShowTemplate = new ThreadGlobal[NodeSeq]
  private val _currentShowEmptyTemplate = new ThreadGlobal[NodeSeq]
  private val _currentInputTemplate = new ThreadGlobal[NodeSeq]
  private val _currentCaptionTemplate = new ThreadGlobal[NodeSeq]

  var defaultShowTemplate = <field:value/>
  var defaultShowEmptyTemplate = NodeSeq.Empty
  // IntelliJ gets confused by entity references emsp (u2003) and ensp(2002)
  var defaultInputTemplate = <p><label><field:caption /></label>{"\u2003"}<field:hint><span class="fieldhint"><hint /></span></field:hint><br /><field:input />{"\u2002"}<field:message errorClass="errorBox" warningClass="warningBox" noticeClass="noticeBox" /></p>
  var defaultCaptionTemplate = <th><field:caption /></th>

  def currentNode = _currentNode.value
  def currentShowTemplate = {
    val res = _currentShowTemplate.value
    if (res == null) defaultShowTemplate else res
  }

  def currentShowEmptyTemplate = {
    val res = _currentShowEmptyTemplate.value
    if (res == null) defaultShowEmptyTemplate else res
  }

  def currentInputTemplate = {
    val res = _currentInputTemplate.value
    if (res == null) defaultInputTemplate else res
  }

  def currentCaptionTemplate = {
    val res = _currentCaptionTemplate.value
    if (res == null) defaultCaptionTemplate else res
  }

  def isFieldEmpty[A <: Mapper[A]](field:MappedField[_,A]): Boolean = {
    (field.isInstanceOf[MappedLong[_]] && (field.asInstanceOf[MappedLong[A]].is == 0L)) ||
            (field.isInstanceOf[MappedString[_]] && {val s=field.asInstanceOf[MappedString[A]].is; (s==null) || (s.length == 0)}) ||
            (field.isInstanceOf[MappedBinary[_]] && {val b=field.asInstanceOf[MappedBinary[A]].is; (b==null) || (b.length == 0)}) ||
            (field.isInstanceOf[MappedText[_]] && {val b=field.asInstanceOf[MappedText[A]].is; (b==null) || (b.length == 0)}) ||
            (field.isInstanceOf[MappedDate[_]] && (field.asInstanceOf[MappedDate[A]].is == null)) ||
            (field.isInstanceOf[MappedEnumList[_,_]] && (field.asInstanceOf[MappedEnumList[A,_]].is.length == 0))
  }

  def defaultShowField[A <: Mapper[A]](field:MappedField[_,A]): NodeSeq = {
    if (isFieldEmpty(field)) showEmpty(field.displayName)
    else showValueIfNotEmpty(field.displayName, field.asHtml)
  }

  def showValue(caption:NodeSeq, value:NodeSeq):NodeSeq =  BindHelpers.bind("field", currentShowTemplate,
        "caption" -> caption,
        "value" -> value
        )

  def showValue(caption:String, value:NodeSeq):NodeSeq = showValue(Text(caption), value)
  def showValue(caption:String, value:String):NodeSeq = showValue(Text(caption), Text(value))

  def showValueIfNotEmpty(caption:String, value:NodeSeq):NodeSeq = if (value.text.length > 0) showValue(caption, value) else Nil
  def showValueIfNotEmpty(caption:String, value:String):NodeSeq = if (value.length > 0) showValue(caption, value) else Nil
 
  def showEmpty(caption:String):NodeSeq = BindHelpers.bind("field", currentShowEmptyTemplate,
        "caption" -> caption
        )

  def defaultInputField[A <: Mapper[A]](field:MappedField[_,A]): NodeSeq = {
    val hint:String = if (field.isInstanceOf[HintedField[_, _]])
      field.asInstanceOf[HintedField[Any,A]].fieldHintWithRequired
    else ""

    BindHelpers.bind("field", currentInputTemplate,
      "caption" -> field.displayName,
      "input" -> field.toForm,
      "hint" -> Text(hint),
      "message" -> {child: NodeSeq => (<lift:Msg id={field.uniqueFieldId.open_!} /> % (BindHelpers.currentNode.map(_.attributes) openOr Null))}
      )
  }

  def makeInputField(caption:NodeSeq, input:NodeSeq, hint:NodeSeq, message:NodeSeq):NodeSeq = {
    BindHelpers.bind("field", currentInputTemplate,
      "caption" -> caption,
      "input" -> input,
      "hint" -> hint,
      "message" -> message
      )
  }

  def defaultFieldCaption[A <: Mapper[A]](field:MappedField[_,A]): NodeSeq = {
    makeCaption(Text(field.displayName))
  }

  def makeCaption(caption:NodeSeq) =
    BindHelpers.bind("field", currentCaptionTemplate,
      "caption" -> caption)

  def bindMapper[A <: Mapper[A]](template: NodeSeq, data: Mapper[A], specialBinding: PartialFunction[String, NodeSeq]): NodeSeq = {
    def in_bind(xml:NodeSeq):NodeSeq = xml.flatMap {
      case s:Elem => s.prefix match {
        case "show" => {
          val saveTemplate = currentShowTemplate
          if (s.child.length > 0) _currentShowTemplate(s.child)
          _currentNode(s)
          val res = (specialBinding orElse ((l:String) => l match {case label =>
            data.fieldByName(label.toLowerCase).map((field:MappedField[_,A]) => defaultShowField[A](field)
            ) openOr Text("ERROR:Field not found:" + s.label)}))(s.label)
          if (s.child.length > 0) _currentShowTemplate(saveTemplate)
          res
        }
        case "input" => {
          val saveTemplate = currentInputTemplate
          if (s.child.length > 0) _currentInputTemplate(s.child)
          _currentNode(s)
          val res = (specialBinding orElse ((l:String) => l match {case label =>
            data.fieldByName(label.toLowerCase).map((field:MappedField[_,A]) => defaultInputField[A](field)
            ) openOr Text("ERROR:Field not found:" + s.label)}))(s.label)
          if (s.child.length > 0) _currentInputTemplate(saveTemplate)
          res
        }
        case "template" => {
          s.label match {
            case "show" => {
              if (s.child.length > 0)
                _currentShowTemplate(s.child)
              else
                _currentShowTemplate(null)

              NodeSeq.Empty
            }
            case "showEmpty" =>  {
              if (s.child.length > 0)
                _currentShowEmptyTemplate(s.child)
              else
                _currentShowEmptyTemplate(null)

              NodeSeq.Empty
            }
            case "input" =>  {
              if (s.child.length > 0)
                _currentInputTemplate(s.child)
              else
                _currentInputTemplate(null)

              NodeSeq.Empty
            }
            case _ => Text("ERROR: invalid template tag")
          }
        }
        case _ => Elem(s.prefix, s.label, s.attributes, s.scope, in_bind(s.child) : _*)
      }
      case Group(nodes) => Group(in_bind(nodes))
      case n => n
    }

    in_bind(template)
  }

  object NoSpecialBinding extends PartialFunction[String, NodeSeq] {
    override def isDefinedAt(x:String) = false
    override def apply(v1:String) = NodeSeq.Empty
  }

  object NoSpecialListBinding extends PartialFunction[(Option[_],Int,String), NodeSeq] {
    override def isDefinedAt(x:(Option[_],Int,String)) = false
    override def apply(v1:(Option[_],Int,String)) = NodeSeq.Empty
  }

  protected class MappedPF[A <: Mapper[A]](val mapper:Mapper[A], val index:Int, val globalPF:PartialFunction[(Option[Mapper[A]],Int,String), NodeSeq]) extends PartialFunction[String, NodeSeq] {
    override def isDefinedAt(x:String) = (x == "listIndex") || globalPF.isDefinedAt(Some(mapper), index, x)
    override def apply(v1:String) = if (v1 == "listIndex") Text(index.toString) else globalPF.apply(Some(mapper), index, v1)
  }

  /**
   * Apply a list template to data. At least for Scala 2.7.6, it is not recommended to use an Array to hold the data (flatMap would iterate twice over the items)
   */
  def bindMappers[A <: Mapper[A]](template: NodeSeq, data: Seq[Mapper[A]], specialBinding: PartialFunction[(Option[Mapper[A]],Int,String), NodeSeq]): NodeSeq = {
    var count = data.length

    def applyClass(attr:MetaData)(in: Node): NodeSeq = in match {
         case g: Group => g.nodes.flatMap(applyClass(attr))
         case e: Elem => e % attr
         case other => other
       }

    def in_bind(xml:NodeSeq):NodeSeq = xml.flatMap {
      case s:Elem => s.prefix match {
        case "caption" =>
          val saveTemplate = currentCaptionTemplate
          if (s.child.length > 0) _currentCaptionTemplate(s.child)
          _currentNode(s)
          val res = (specialBinding orElse ((l:(Option[Mapper[A]],Int,String)) => l match {case (_, _, label) =>
            data.first.fieldByName(label.toLowerCase).map((field:MappedField[_,A]) => defaultFieldCaption[A](field)
            ) openOr Text("ERROR:Field not found:" + label)}))(None, 0, s.label)
          if (s.child.length > 0) _currentCaptionTemplate(saveTemplate)
          res
        case "list" => s.label match {
          case"count" => Text(count.toString)
          case "each" => {
            val oddClass = new UnprefixedAttribute("class", s.attribute("oddClass"), Null)
            val evenClass = new UnprefixedAttribute("class", s.attribute("evenClass"), Null)
            var index = 0
            data.flatMap(mapper => {
              index += 1
              bindMapper(s.child, mapper, new MappedPF(mapper, index, specialBinding))
                      .flatMap(applyClass(if ((index % 2) == 0) evenClass else oddClass))})
          }
          case _ => NodeSeq.Empty
        }
        case _ => Elem(s.prefix, s.label, s.attributes, s.scope, in_bind(s.child) : _*)
      }
      case Group(nodes) => Group(in_bind(nodes))
      case n => n
    }

    if (data.isEmpty) chooseTemplate("list", "ifEmpty", template)
    else in_bind(template)

  }
}

object MBindHelper extends MBindHelper