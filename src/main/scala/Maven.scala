import java.io.{PrintWriter, StringWriter}

import scala.collection.mutable
import java.util.zip.ZipFile

import org.apache.commons.io.IOUtils
import jdk.internal.org.objectweb.asm.ClassReader
import jdk.internal.org.objectweb.asm.tree.ClassNode
import jdk.internal.org.objectweb.asm.tree.InsnList
import jdk.internal.org.objectweb.asm.util.Textifier
import jdk.internal.org.objectweb.asm.util.Printer
import jdk.internal.org.objectweb.asm.util.TraceMethodVisitor

import collection.JavaConverters._

object Maven {

  val repository: String = "http://repo1.maven.org/maven2/"

  def getElements(target: String): ZipFile = {
    //val file = new File(target)
    val zipFile = new ZipFile(target)
    zipFile
  }

  def jarElements(url: String): Unit = {
    val zipFile = getElements(url)

    val entries = zipFile.entries.asScala
    val elements = new mutable.HashSet[String]()

    entries.filter(x => x.getName.endsWith(".class") && x.getName != "module-info.class" && !x.getName.contains("$")).
      foreach { entry =>
        val content = IOUtils.toByteArray(zipFile.getInputStream(entry))

        try {
          val node: ClassNode = new ClassNode
          val reader: ClassReader = new ClassReader(content)
          reader.accept(node, 0)

          val className = node.name.replaceAll("/", ".")

          println(className)
          println()

          val printer: Printer = new Textifier
          val methodVisitor: TraceMethodVisitor = new TraceMethodVisitor(printer)

          for (element <- node.methods.asScala) {
            println(element.attrs)

            val instructionList: InsnList = element.instructions
            for (i <- 0 until instructionList.size) {
              instructionList.get(i).accept(methodVisitor)

              val writer: StringWriter = new StringWriter
              printer.print(new PrintWriter(writer))
              printer.getText.clear()
              println(writer.toString)
            }
          }
      } catch {
        case _: IllegalArgumentException =>
      }

      return
    }
  }

  //    elements.toSet
  //  }
}
