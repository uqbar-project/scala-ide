package scala.tools.eclipse

import org.eclipse.jdt.internal.ui.viewsupport.AppearanceAwareLabelProvider
import org.eclipse.jface.viewers.StyledString
import scala.tools.eclipse.javaelements.ScalaDefElement

class ScalaElementsLabelProvider(textFlags: Long, imageFlags: Int) extends AppearanceAwareLabelProvider {

  override def getStyledText(element: Object) = {
    val styledText = new ScalaElementsFormatter().getStyledText(element)
    if (styledText != null) styledText else super.getStyledText(element)
  }

}

class ScalaElementsFormatter {
  def getStyledText(element: Object) = {
    element match {
      case e: ScalaDefElement =>
        val display = new StyledString(e.getElementName)
        if (!e.getParameterTypes().isEmpty) {
          display.append("(" + e.getParameterTypes().mkString(", ") + ")")
        }
        display
      case _ => null
    }
  }
}