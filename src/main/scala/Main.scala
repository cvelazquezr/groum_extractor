import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scopt.OptionParser

object Main {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  case class GroumOptions (
                            visualize: Boolean = false,
                            sootClassPath: String = null,
                            useJPhantom : Boolean = false,
                            outPhantomJar : String = null,
                            sliceFilter : String = null,
                            processDir: String = null,
                            className: String = null,
                            outputDir: String = null
                          )

  def main(args: Array[String]): Unit = {
//    Maven.jarElements("packages/junit-4.0.jar")

    val parser = new OptionParser[GroumOptions]("groumExtractor") {
      head("groumExtractor", "0.1")

      opt[Boolean]('v', "visualize") action { (x, c) =>
        c.copy(visualize = x) } text "Set to true to visualize an embedding/isomorphism"

      opt[String]('l', "classPath") action { (x, c) =>
        c.copy(sootClassPath = x) } text "classPath is the soot classpath"

      opt[Boolean]('j', "jphanthom") action { (x, c) =>
        c.copy(useJPhantom = x) } text "Set to true to use JPhantom"

      opt[String]('z', "jphantom-folder") action { (x, c) =>
        c.copy(outPhantomJar = x) } text "Path to the generated JPhantom classes"

      opt[String]('f', "slice-filter").action { (x, c) =>
        c.copy(sliceFilter = x) } text "Package prefixes to use as seed for slicing separated with (:) prefix"

      opt[String]('p', "process-dir") action { (x, c) =>
        c.copy(processDir = x) } text "Comma (,) separated list of input directories to process"

      opt[String]('c', "class-name") action { (x, c) =>
        c.copy(className = x) } text "Name of the class to be processed"

      opt[String]('o', "output-dir").required().action { (x, c) =>
        c.copy(outputDir = x) } text "Path of the output directory for the groum"
    }

    parser.parse(args, GroumOptions()) match {
      case Some(groumOpt) if groumOpt.visualize =>
        println("Visualizing")
      case Some(groumOpt) if ! groumOpt.visualize => {
        logger.debug("cp: {}", groumOpt.sootClassPath)
        logger.debug("jphantom: {}\n", groumOpt.useJPhantom)
        logger.debug("jphantom-folder: {}\n", groumOpt.outPhantomJar)
        logger.debug("slice-filter: {}", groumOpt.sliceFilter)
        logger.debug("process-dir: {}", groumOpt.processDir)
        logger.debug("class-name: {}", groumOpt.className)
        logger.debug("output-dir: {}", groumOpt.outputDir)

        if (null == groumOpt.processDir &&
          (null == groumOpt.className)) {
          logger.error("You must set one between process dir and class name")
          System.exit(1)
        }
        if (null != groumOpt.processDir &&
          (null != groumOpt.className)) {
          logger.error("The process-dir option is mutually exclusive with the class-name and method-name options")
          System.exit(1)
        }

        val configCode = SootHelper.READ_FROM_BYTECODE

        val options: ExtractorOptions = new ExtractorOptions

        options.className = groumOpt.className
        options.useJPhantom = groumOpt.useJPhantom
        options.outPhantomJar = groumOpt.outPhantomJar
        options.configCode = configCode
        options.sootClassPath = groumOpt.sootClassPath
        options.outputDir = groumOpt.outputDir

        if (null != groumOpt.processDir) {
          val myArray : Array[String] = groumOpt.processDir.split(":")
          options.processDir = myArray.toList
        }

        if (null != groumOpt.sliceFilter) {
          options.sliceFilter = groumOpt.sliceFilter.split(":").toList
        }

        val extractor: Extractor =
          if (options.processDir == null) new MethodExtractor(options)
          else new MultipleExtractor(options)

        extractor.extract()
      }

      case None => System.exit(1)
    }

    logger.info("Terminated extraction ...")
    System.exit(0)

  }

}
