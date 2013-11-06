package scala.tools.eclipse.ui

import scala.tools.eclipse.ScalaElementsFormatter
import scala.tools.eclipse.javaelements.ScalaDefElement
import org.junit.Assert.assertEquals
import org.junit.Test
import scala.tools.eclipse.EclipseUserSimulator
import scala.tools.eclipse.ScalaPlugin
import org.eclipse.core.resources.ResourcesPlugin
import scala.tools.eclipse.testsetup.TestProjectSetup
import scala.tools.eclipse.javaelements.ScalaClassElement
import scala.tools.eclipse.testsetup.SDTTestUtils
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.IPath
import scala.tools.eclipse.testsetup.FileUtils
import scala.tools.eclipse.ScalaProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.NullProgressMonitor
import scala.tools.eclipse.javaelements.ScalaSourceFile

class ScalaElementLabelProviderTests {

  private val simulator = new EclipseUserSimulator

  @Test
  def scalaDefElement_methodNoParameters = {
    //    val scalaProject =
    //      ScalaPlugin.plugin.getScalaProject(ResourcesPlugin.getWorkspace().getRoot().getProject("org.scala-ide.sdt.core.tests"))
    //          simulator.createProjectInWorkspace("testProject", true)
    //scalaProject.
    val TestProjectName = "scala-element-label-provider"
    val scalaProject = simulator.createProjectInWorkspace(TestProjectName, withSourceRoot = true)
    val projectSetup = new TestProjectSetup(TestProjectName) {
      override lazy val project = scalaProject
    }

    val sourceCode = """
        |package test
        |
        |object OuterWithWronMain {
        |  def main(args: Seq[String]) {} // not Array[String]
        |}
        |
        |object OuterWithGoodMain {
        |  def main(args: Array[String]) {}
        |}
        |
        |class ClassWithGoodMain {
        |  def main(args: Array[String]) {}  // it's a class, should not be reported
        |}
        |
        |object ObjectExtendsApp extends App {} // should be reported
        |
        |object Outer {
        |  object Inner extends App {} // not top level, should not be reported
        |}
      """.stripMargin

    val testWorkspaceLocation = SDTTestUtils.sourceWorkspaceLoc(projectSetup.bundleName)
    val findReferencesTestWorkspace = testWorkspaceLocation.append(new Path(TestProjectName))
    val testProject = findReferencesTestWorkspace.append("method-test-project")

    mirrorContentOf(scalaProject, testProject)

    //      runTest(sourceName, testDefinition.testMarker, testDefinition.toExpectedTestResult)
    val cu = projectSetup.scalaCompilationUnit("TestClass.scala")

    projectSetup.reload(cu)
    val elements = cu.codeSelect(0, 53)
    //    createSourceFile("test", "MyMain.scala") {
    //      sourceCode
    //    }
    //    val elements = cu.findElements(new ScalaClassElement(null, "ClassWithGoodMain", true))
    val unit = ScalaSourceFile.createFromPath("TestClass.scala")
    cu.withSourceFile { (source, pcompiler) =>
      pcompiler.withParseTree(source) { tree =>
        pcompiler.askOption { () =>
          tree.id
          // Compute some value from the tree.
        }
      }
    }
    val scalaDefElement = new ScalaDefElement(null, "methodName", Array(), true, "", 0)
    assertText(scalaDefElement, "methodName")
  }

  private def mirrorContentOf(project: ScalaProject, sourceProjectLocation: IPath): Unit = {
    val target = project.underlying.getLocation.toFile
    val from = sourceProjectLocation.toFile

    FileUtils.copyDirectory(from, target)

    project.underlying.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor)
  }

  //  @Test
  def scalaDefElement_methodParameterWithNativeType = {
    val scalaClassElement = new ScalaClassElement(null, "FormaterElementTest", true)
    val scalaDefElement = new ScalaDefElement(scalaClassElement, "methodName", Array("scala.Int"), true, "", 0)
    assertText(scalaDefElement, "methodName(p1:Int)")
  }

  def assertText(scalaDefElement: ScalaDefElement, expectedText: String): Unit = {
    val labelProvider = new ScalaElementsFormatter
    val formattedText = labelProvider.getStyledText(scalaDefElement).getString
    assertEquals(expectedText, formattedText)
  }

}

class TestClass {

  def methodName(p1: Int) = Unit
}