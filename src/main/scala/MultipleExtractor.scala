import soot.{PackManager, PhaseOptions, Transform}
import soot.options.Options


class MultipleExtractor(options: ExtractorOptions) extends Extractor(options) {
  val transformer = new MethodsTransformer(options)

  def extract(): Unit = {
    assert(null != options.processDir || null != options.className)
    assert(null == options.processDir || null == options.className)

    // Inject the analysis tagger into Soot
    PackManager.v().getPack("jtp").add(new Transform("jtp.myTransform",
      transformer))
    PhaseOptions.v().setPhaseOption("jtp", "on")

    // Invoke soot.Main with arguments given
    if (null != options.className) {
      Options.v().set_main_class(options.className)
    }
    val args : Array[String] =
      if (options.className != null) Array(options.className) else Array[String]()
    SootHelper.run(args)
  }

  def getTransformer: MethodsTransformer = transformer
}
