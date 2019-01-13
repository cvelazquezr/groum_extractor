import java.io._

import sys.process._
import java.nio.file.Paths
import java.util
import java.util.concurrent.Callable

import ProtoAcdfg.Acdfg.SourceInfo
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import soot.toolkits.graph.{ExceptionalUnitGraph, UnitGraph}
import soot.toolkits.graph.pdg.EnhancedUnitGraph
import soot.{Body, BodyTransformer, Printer, SootClass, SootMethod}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.collection.JavaConverters.seqAsJavaList

class MethodsTransformer(options: ExtractorOptions) extends BodyTransformer {
  val groumListBuffer: ListBuffer[Acdfg] = ListBuffer[Acdfg]()
  val logger : Logger = LoggerFactory.getLogger(this.getClass)

  override protected def internalTransform(body: Body,
                                           phase: String,
                                           transformOpt: util.Map[String, String]): Unit = {
    val method: SootMethod = body.getMethod
    val sootClass: SootClass = method.getDeclaringClass

    val num: Regex = raw"(\d+)".r

    if (num.findAllIn(sootClass.getName).toList.isEmpty)
      extractMethod(sootClass, method)
  }

  def sliceBody(sc : SliceCriteria, jimpleUnitGraph: EnhancedUnitGraph,
                body : Body) : Option[(APISlicer, Body)] = {

    logger.info("Creating the slicer...")
    val slicer: APISlicer = new APISlicer(jimpleUnitGraph, body)
    logger.info("Slicer created...")
    logger.info("Slicing...")
    val slicedJimple: Body = slicer.slice(sc)
    logger.info("Slicing done...")
    Some((slicer, slicedJimple))
  }

  def extractMethod(sootClass : SootClass, sootMethod : SootMethod) : Unit = {
    logger.debug("Extracting graph for - class {} - method: {}{}",
      sootClass.getName, sootMethod.getName, "")

    assert(sootMethod.isConcrete)

    val name : String = sootClass.getName + "_" + sootMethod.getName
    val outputFile: File = getAcdfgOutName(options.outputDir + "/provenance/", sootMethod.getName)
    if (outputFile.exists()) {
      // Do not overwrite a graph
      logger.error("File {} already exists, skipping it...", outputFile)
      return
    }

    val body: Body = sootMethod.retrieveActiveBody()

    var sliceCriteria: SliceCriteria = null

    if (null == options.sliceFilter) {
      logger.error("No groum.graphs.slice criteria defined in the options !!!")
      return
    }
    else
      sliceCriteria = new MethodPackageSeed(seqAsJavaList(options.sliceFilter))

    logger.debug("Jimple slicing...")
    val jimpleSlicer = new JimpleSlicing(body, sliceCriteria)
    jimpleSlicer.sliceJimple()
    logger.debug("Jimple slicing end...")

    logger.info("Creating the enhanced unit graph...")
    val jimpleUnitGraph: EnhancedUnitGraph = new EnhancedUnitGraph(body)
    logger.info("Enhanched unit graph created... (size = " + jimpleUnitGraph.size() + ")" )

    val sliceResult : Option[(APISlicer, Body)] = sliceBody(sliceCriteria, jimpleUnitGraph, body)

    val bodyToUse =
      sliceResult match {
        case None => body
        case Some((slicer, slicedJimple)) => slicedJimple
      }

//    val bodyToUse = body

    if (null == bodyToUse) {
      logger.warn("Empty slice for - class {} - method: {}\nFilter: {}\n\n",
        sootClass.getName, sootMethod.getName, sliceCriteria.getCriterionDescription)
    } else {
      logger.debug("CDFG construction...")

      val simplifiedBody : BodySimplifier =
        if (null != options.sliceFilter) {
          new BodySimplifier(new ExceptionalUnitGraph(bodyToUse), seqAsJavaList(options.sliceFilter))
        } else {
          logger.error("No groum.graphs.slice criteria defined in the options !!!")
          return
        }

      val cdfg: UnitCdfgGraph = new UnitCdfgGraph(simplifiedBody.getSimplifiedBody)
      logger.debug("CDFG built...")

//      val dataNodes: DataNodes = new DataNodes(cdfg, sootClass.getName, sootMethod.getName)
//      dataNodes.makeData()

      val sourceBuilder: SourceInfo.Builder = SourceInfo.getDefaultInstance.toBuilder

      sourceBuilder.setPackageName(sootClass.getJavaPackageName)
      sourceBuilder.setClassName(sootClass.getName)
      sourceBuilder.setMethodName(sootMethod.getName)
      sourceBuilder.setClassLineNumber(SootHelper.getLineNumber(sootClass))
      sourceBuilder.setMethodLineNumber(SootHelper.getLineNumber(sootMethod))
      sourceBuilder.setSourceClassName(SootHelper.getFileName(sootClass))
      sourceBuilder.setAbsSourceClassName(SootHelper.getAbsFileName(sootClass))

      val sourceInfo: SourceInfo = sourceBuilder.build()

      logger.debug("ACDFG construction...")

      val acdfg: Acdfg = new Acdfg(cdfg, sourceInfo, name + ".html")

      logger.debug("ACDFG built...")


      if (null != options.outputDir) {

        sliceResult match {
          case None => {
            writeData(name, acdfg, cdfg, body, None, None)
            logger.debug("Created graph for - class {} - method: {}{}",
              sootClass.getName, sootMethod.getName, "")
          }
          case Some((slicer, slicedJimple)) => {
            writeData(name, acdfg, cdfg, body, Some(slicedJimple),
              Some(slicer.getCfg))
            logger.debug("Created graph for - class {} - method: {}{}",
              sootClass.getName, sootMethod.getName, "")
          }
        }
      }
      else {
        logger.warn("Disabled data writing for - class {} - method: {}{}",
          sootClass.getName, sootMethod.getName, "")
      }
    }
  }

  protected def writeJimple(body : Body, fileName : String) : Unit = {
    val streamOut : OutputStream = new FileOutputStream(fileName)
    val writerOut : PrintWriter = new PrintWriter(new OutputStreamWriter(streamOut))
    Printer.v().printTo(body, writerOut)
    writerOut.flush()
    streamOut.close()
  }

  private def getAcdfgOutName(outputDir : String, outFileNamePrefix : String) : File = {
    val acdfgFileName : String = Paths.get(outputDir, outFileNamePrefix + ".acdfg.bin").toString
    val outputFile: File = new File(acdfgFileName)
    outputFile
  }

  /**
    * Write the data to the output folder
    */
  private def writeData(outFileNamePrefix : String,
                        acdfg : Acdfg,
                        cdfg : UnitCdfgGraph,
                        body : Body,
                        slicedBodyOption : Option[Body],
                        slicedCfgOption : Option[UnitGraph]
                       ) : Unit = {

    val Array(className, methodName) = outFileNamePrefix.split("_")

    val currentDir = System.getProperty("user.dir")
    var outputDir = if (null == options.outputDir) currentDir else options.outputDir
    outputDir += className + "/" + methodName

    logger.debug("Writing ACDFG data to: {}", outputDir)

    // Write the acdfg
    val acdfgBinPath: String = outputDir + "/provenance/"
    val outputFile: File = getAcdfgOutName(acdfgBinPath, methodName)
    val outputDirPath = new File(acdfgBinPath)
    try {
      if (! outputDirPath.exists()) {
        val created = outputDirPath.mkdirs()
        if (! created) {
          throw new Exception("Error creating " + acdfgBinPath)
        }
      }
    }
    catch {
      case ex: Exception =>
        logger.error("Unable to create required new output directory")
        throw ex
    }
    val output : FileOutputStream = new FileOutputStream(outputFile)
    val protobuf = acdfg.toProtobuf
    protobuf.writeTo(output)

    output.close()

    // Write the provenance information
    if (options.outputDir != null) {
      logger.debug("Writing provenance data to: {}", outputDir)
      val filePrefixHtml : String = Paths.get(outputDir, methodName).toString
      val filePrefixProvenance: String = Paths.get(outputDir + "/provenance", methodName).toString

      try {
        val provFile : File = new File(filePrefixProvenance)
        if (! provFile.getParentFile.exists) {
          provFile.getParentFile.mkdir
        }
      }
      catch {
        case ex: Exception =>
          logger.error("Unable to create required new provenance directory")
          throw ex
      }

      // ACDFG DOT
      val acdfg_dot: String = filePrefixProvenance + ".acdfg.dot"
      val dotGraph: AcdfgToDotGraph = new AcdfgToDotGraph(acdfg)
      dotGraph.draw().plot(acdfg_dot)

      // CFG
      val cfg_dot: String = filePrefixProvenance + ".cfg.dot"
      slicedCfgOption match {
        case None => ()
        case Some(cfg) => SootHelper.dumpToDot(cfg, cfg.getBody, cfg_dot)
      }

      // CDFG
      val cdfg_dot: String = filePrefixProvenance + ".cdfg.dot"
      SootHelper.dumpToDot(cdfg, cdfg.getBody, cdfg_dot)

      // JIMPLE
      val jimpleFileName: String = filePrefixProvenance + ".jimple"
      writeJimple(body, jimpleFileName)

      // SLICED JIMPLE
      slicedBodyOption match {
        case None => ()
        case Some(slicedBody) => {
          val slicedJimpleName : String = filePrefixProvenance + ".sliced.jimple"
          writeJimple(slicedBody, slicedJimpleName)
        }
      }

      val toPng: String = "bash tools/dot_to_png.sh " + outputDir + " " + methodName
      toPng.!

      val provenance: Provenance = new Provenance(body, "provenance/" + methodName,
                                    slicedBodyOption, slicedCfgOption, cdfg, acdfg)

      try {
        val provFileName : String = filePrefixHtml + ".html"
        val provFile : File = new File(provFileName)
        val writer: Writer = new BufferedWriter(
          new OutputStreamWriter(new FileOutputStream(provFile),"utf-8")
        )
        writer.write(provenance.toHtml.toString)
        writer.close()
      }
      catch {
        case ex: Exception =>
          logger.error("Unable to write to " + filePrefixHtml + ".html")
          logger.error(ex.getMessage)
      }
    }
  }

  private class TimeOutExecutor(transformer : MethodsTransformer,
                                sootClass : SootClass, sootMethod : SootMethod)
    extends Callable[Unit] {

    @throws(classOf[Exception])
    override def call() : Unit = {
      try {
        transformer.extractMethod(sootClass, sootMethod)
      }
      catch {
        case e : StackOverflowError => logger.error("StackOverflowError processing class {}, method {}{}",
                                        sootClass.getName, sootMethod.getName, "")
                                        logger.error("Exception {}:", e)
      }
    }
  }
}
