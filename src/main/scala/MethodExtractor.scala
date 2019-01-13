import soot.PackManager
import soot.Transform

class MethodExtractor(options: ExtractorOptions) extends Extractor(options) {
  val transformer: MethodsTransformer = new MethodsTransformer(options)

  def extract(): Unit = {
    assert(null != options.className)

    // Inject the graph extractor into Soot
    PackManager.v().getPack("jtp").add(new Transform("jtp.graphExtractor", transformer))
    SootHelper.run(Array(options.className))
  }

  def getTransformer: MethodsTransformer = transformer

}
