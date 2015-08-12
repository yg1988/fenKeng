package roll
import acyclic.file
import org.scalajs.dom
import scala.scalajs.js

object Calc{

  def apply[T](thunk: => T) = new Calc(thunk)
}
class Calc[T](thunk: => T){
  var lastVal = thunk
  def apply() = lastVal
  def recalc() = {
    lastVal = thunk
    lastVal
  }
}
case class Ref[T](var v: T){
  def apply() = v
  def update(newT: T) = v = newT
}

class PointerEvent extends dom.MouseEvent {
  var width: Double = _
  var rotation: Double = _
  var pressure: Double = _
  var pointerType: js.Any = _
  var isPrimary: Boolean = _
  var tiltY: Double = _
  var height: Double = _
  var intermediatePoints: js.Any = _
  var currentPoint: js.Any = _
  var tiltX: Double = _
  var hwTimestamp: Double = _
  var pointerId: Double = _
  def initPointerEvent(typeArg: String, canBubbleArg: Boolean, cancelableArg: Boolean, viewArg: dom.Window, detailArg: Double, screenXArg: Double, screenYArg: Double, clientXArg: Double, clientYArg: Double, ctrlKeyArg: Boolean, altKeyArg: Boolean, shiftKeyArg: Boolean, metaKeyArg: Boolean, buttonArg: Double, relatedTargetArg: dom.EventTarget, offsetXArg: Double, offsetYArg: Double, widthArg: Double, heightArg: Double, pressure: Double, rotation: Double, tiltX: Double, tiltY: Double, pointerIdArg: Double, pointerType: js.Any, hwTimestampArg: Double, isPrimary: Boolean): Unit = ???
  def getCurrentPoint(element: dom.Element): Unit = ???
  def getIntermediatePoints(element: dom.Element): Unit = ???
  var MSPOINTER_TYPE_PEN: Double = _
  var MSPOINTER_TYPE_MOUSE: Double = _
  var MSPOINTER_TYPE_TOUCH: Double = _
}

object PointerEvent extends js.Object {
  /* ??? ConstructorMember(FunSignature(List(),List(),Some(TypeRef(TypeName(MSPointerEvent),List())))) */
  var MSPOINTER_TYPE_PEN: Double = _
  var MSPOINTER_TYPE_MOUSE: Double = _
  var MSPOINTER_TYPE_TOUCH: Double = _
}