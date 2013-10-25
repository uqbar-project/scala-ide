package scala.tools.eclipse.debug.debugged

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.reflect.NameTransformer
import java.io.File
import scala.io.Source
import scala.collection.mutable
import scala.tools.nsc.interpreter.NamedParamClass
import scala.tools.nsc.interpreter.NamedParamClass
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.SynchronizedMap

object ReplAssistance {
  private val threadLocalRepls = new mutable.HashMap[Long, IMain] with mutable.SynchronizedMap[Long, IMain]

  private var initialized = false
  private val replAssistanceCL = ReplAssistance.getClass().getClassLoader()
  private var toolbox: ToolBox[reflect.runtime.universe.type] = null

  private type EvalType = Seq[Any] => Any
  private val cachedEvalMethods = new mutable.HashMap[String, EvalType] with SynchronizedMap[String, EvalType]

  private def initializeToolBox() = synchronized {
    if (!initialized) {
      val toolboxFactory = runtimeMirror(replAssistanceCL)
      toolbox = toolboxFactory.mkToolBox()
      initialized = true
    }
  }

  /**
   * Compile a conditional breakpoint expression.
   *
   * Wraps the conditional breakpoint expression into a function that takes all the used names in the expression as sequence, and then manually stores each casts each seq entry into a val of the proper type.
   * For example, the expression "x == y" where 'x' and 'y' are of type String, becomes the following code:
   * {{{
   * (params$$: Seq[Any]) => {
   *    val x: String = params$$(0).asInstanceOf[String]
   *    val y: String = params$$(1).asinstanceOf[String]
   *    x == y
   * }}}
   *
   * @param classpath - a classpath used to create the ToolBox
   * @param identifier - a unique identifier for the conditional breakpoint expression, used when invoking the expression with `ReplAssistance.invoke`.
   * @param paramNamesTypes - a seq of all the names (with their type) that `expressionString` uses. The expected format of each entry is: "<name> : <type>"
   * @param expressionString - The actual expression from the conditional breakpoint.
   */
  def createEvalMethod(identifier: String, paramNamesTypes: Seq[String], expressionString: String): Boolean = synchronized {
    initializeToolBox()
    val decls = for(i <- 0 until paramNamesTypes.size) yield {
      val param = paramNamesTypes(i)
      val tpe = param.split(":")(1)
      s"val $param = params$$($i).asInstanceOf[$tpe]"
    }
    val code =
      s"""(params$$: Seq[Any]) => {
             ${ decls.mkString("\n") }
             $expressionString
           }"""
    println("Generate code: " + code)
    Try(toolbox.compile(toolbox.parse(code))) match {
      case Success(compiledResult) => {
        cachedEvalMethods(identifier) = compiledResult().asInstanceOf[EvalType]
        true
      }
      case Failure(e) => {
        println(e)
        false
      }
    }
  }

  def invoke(sourceName: String, params: Seq[Any]): Boolean = {
    cachedEvalMethods.get(sourceName) match {
      case Some(f) => f(params) match {
        case x: Boolean => x
        case x: java.lang.Boolean => x
        case _ => false
      }
      case _ => false
    }
  }

  /* Assumes that classpath will never change over the lifetime of the application */
  def createRepl(classpath: List[String]): IMain = {
    lazy val repl = {
      //      scala.tools.nsc.interpreter.replProps.debug.enable() // TODO: remove in production
      val settings = new Settings
      settings.embeddedDefaults(replAssistanceCL)
      for (cp <- classpath)
        settings.bootclasspath.append(cp)
      val newRepl = new IMain(settings)
      newRepl.initializeSynchronous()
      newRepl
    }

    threadLocalRepls.getOrElseUpdate(Thread.currentThread().getId(), repl)
    //    repl
  }

  def getClassName(obj: Any): String = {
    val typeparams = obj.getClass.getTypeParameters()
    // strip suffix in case it is an object
    NameTransformer.decode(obj.getClass.getName) + (if (!typeparams.isEmpty) s"[${typeparams.map(s => "_").mkString(",")}]" else "")
  }

  def createNamedParamClass(name: String, boundType: String, value: Any): NamedParamClass =
    NamedParamClass(name, boundType, value)
}