import org.slf4j.{Logger, LoggerFactory}
import org.clyze.jphantom.Driver

import scala.collection.JavaConverters.seqAsJavaList

abstract class Extractor(options: ExtractorOptions) {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  initExtractor()

  def extract(): Unit
  def getTransformer: MethodsTransformer

  private def initExtractor(): Unit = {
    var sootClassPath: String = null

    if (options.useJPhantom) {
      logger.info("Invoking JPhantom...")
      assert(options.outPhantomJar != null)

      var classPath: List[String] = List[String]()
      classPath = options.sootClassPath.split(":").foldLeft(classPath)(
        (classPath, y) => y :: classPath)
      if (options.processDir != null) {
        classPath = options.processDir.foldLeft(classPath)(
          (classPath, y) => y :: classPath)
      }

      Driver.createPhantomClassJar(seqAsJavaList(classPath), options.outPhantomJar)

      sootClassPath = options.sootClassPath + ":" + options.outPhantomJar
      }
      else {
        sootClassPath = options.sootClassPath
      }

      logger.info("Initializing soot...")
      if (null == options.processDir)
        SootHelper.configure(sootClassPath, options.configCode)

      else
        SootHelper.configure(sootClassPath, options.configCode, seqAsJavaList(options.processDir))
    }
}
