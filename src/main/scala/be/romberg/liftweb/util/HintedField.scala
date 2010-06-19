package be.romberg.liftweb.util

import net.liftweb.http._
import scala.xml._

import net.liftweb.mapper._
import net.liftweb.util._

trait HintedField[FieldType <: Any,OwnerType <: Mapper[OwnerType]] extends MappedField[FieldType, OwnerType] {
  def fieldHint: Option[String] = None
  def fieldHintWithRequired:String = (if (isRequired) "(Required) " else "") + fieldHint.getOrElse("")
  def isRequired: Boolean = false
  override def fieldId = Some(Text(Helpers.nextFuncName))
  // little used now - replaced by MBindHelper
  def toFormFull : NodeSeq = {
    def req = if (isRequired) "(Required)\u2003" else ""
    <label for={fieldId}>{Text(displayName + "\u2003")}<span class="fieldhint">{req + fieldHint.getOrElse("")}</span></label> ++ <br /> ++ toForm.open_!
}
  def cssClasses : List[String] = (if (isRequired) "required" :: Nil else Nil)

  override def toFormAppendedAttributes = (
    if (cssClasses.isEmpty)
      super.toFormAppendedAttributes
    else
      new UnprefixedAttribute("class", cssClasses.mkString(" "), super.toFormAppendedAttributes)
  )
}
