package scala.tools.eclipse.ui

import scala.tools.eclipse.ScalaElementsFormatter
import scala.tools.eclipse.javaelements.ScalaDefElement

import org.junit.Assert.assertEquals
import org.junit.Test

class ScalaElementLabelProviderTests {

  @Test
  def scalaDefElement_methodNoParameters = {
    val scalaDefElement = new ScalaDefElement(null, "methodName", Array(), true, "", 0)
    assertText(scalaDefElement, "methodName")
  }

  @Test
  def scalaDefElement_methodParameterWithNativeType = {
    val scalaDefElement = new ScalaDefElement(null, "methodName", Array("scala.Int"), true, "", 0)
    assertText(scalaDefElement, "methodName(p1:Int)")
  }

  def assertText(scalaDefElement: ScalaDefElement, expectedText: String): Unit = {
    val labelProvider = new ScalaElementsFormatter
    val formattedText = labelProvider.getStyledText(scalaDefElement).getString
    assertEquals(expectedText, formattedText)
  }
}