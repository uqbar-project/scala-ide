package scala.tools.eclipse.debug.model

import scala.collection.JavaConverters.setAsJavaSetConverter
import scala.tools.eclipse.ScalaPlugin
import scala.tools.eclipse.ScalaProject
import scala.tools.eclipse.debug.evaluation.ScalaEvaluationEngine
import scala.tools.eclipse.launching.ScalaLaunchDelegate
import org.eclipse.debug.core.model.IDebugElement
import org.eclipse.debug.core.model.IWatchExpressionDelegate
import org.eclipse.debug.core.model.IWatchExpressionListener
import org.eclipse.debug.core.model.IWatchExpressionResult
import scala.tools.eclipse.debug.ScalaDebugger
import org.eclipse.debug.core.model.IThread
import org.eclipse.debug.core.model.IStackFrame
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

class ScalaWatchExpressionDelegate extends IWatchExpressionDelegate {

  def evaluateExpression(expression: String, context: IDebugElement, listener: IWatchExpressionListener) = {
  //      try {
      val debugTarget = context.getDebugTarget.asInstanceOf[ScalaDebugTarget]
      val result = doEvaluate(expression, debugTarget)

      listener.watchEvaluationFinished(
        new IWatchExpressionResult {
          val getValue = wrapValue(result, debugTarget)
          val hasErrors = false
          val getErrorMessages = Array[String]()
          val getExpressionText = expression
          val getException = null
        }
      )
  //  } catch {
  //    case e: Throwable =>
  //      val x = e
  //      val y = x
  //
  //      new IWatchExpressionResult {
  //        val getValue = null
  //        val hasErrors = true
  //        val getErrorMessages = Array[String]("Unsupported expression")
  //        val getExpressionText = expression
  //        val getException = new DebugException(new Status(IStatus.ERROR, ScalaDebugPlugin.id, e.getMessage, e))
  //      }
  //  }
  }

  protected def wrapValue(value: Any, debugTarget: ScalaDebugTarget) = ScalaValue(value match {
    case b: Boolean => b
    case b: Byte    => b
    case c: Char    => c
    case s: Short   => s
    case i: Int     => i
    case l: Long    => l
    case f: Float   => f
    case d: Double  => d
    case other      => other.toString
  }, debugTarget)

  protected def doEvaluate(expression: String, debugTarget: ScalaDebugTarget): Any = {
    def bindStackFrame(evalEngine: ScalaEvaluationEngine, stackFrame: ScalaStackFrame, scalaProject: ScalaProject): Unit = {
      val bindings = ScalaEvaluationEngine.yieldStackFrameBindings(Option(stackFrame), scalaProject)

      for (b <- bindings) evalEngine.bind(b.name, b.value, true)(b.tpe)
    }

    def getScalaLaunchDelegate(thread: ScalaThread): ScalaLaunchDelegate = {
      val launch = thread.getDebugTarget.getLaunch
      val launchDelegate = launch.getLaunchConfiguration().getPreferredDelegate(Set(launch.getLaunchMode()).asJava)

      launchDelegate.getDelegate.asInstanceOf[ScalaLaunchDelegate]
    }

    def makeEvalEngine(stackFrame: ScalaStackFrame): ScalaEvaluationEngine = {
      val sld = getScalaLaunchDelegate(stackFrame.thread)
      val config = debugTarget.getLaunch.getLaunchConfiguration
      val evalEngine = new ScalaEvaluationEngine(sld.getClasspath(config), stackFrame.thread.getDebugTarget, stackFrame.thread)

      evalEngine.resetRepl()

      bindStackFrame(evalEngine, stackFrame, ScalaPlugin.plugin.asScalaProject(sld.getJavaProject(config).getProject).get)

      evalEngine
    }

    val thread = ScalaDebugger.currentThread
    val frame = thread.getTopStackFrame.asInstanceOf[ScalaStackFrame]
    val evaluationEngine = makeEvalEngine(frame)

    val bindings = ScalaEvaluationEngine.yieldStackFrameBindings(Option(frame), getScalaLaunchDelegate(frame.thread).scalaProject)

    for (b <- bindings)
      evaluationEngine.bind(b.name, b.value, true)(b.tpe)

    evaluationEngine.execute(expression, true, Nil).get
  }
}