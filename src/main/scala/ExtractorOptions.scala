class ExtractorOptions {
  // Soot options
  var sootClassPath : String = _
  var configCode : Int = SootHelper.READ_FROM_BYTECODE

  // JPhantom options
  var useJPhantom : Boolean = false
  var outPhantomJar : String = _

  // slicing options
  var sliceFilter : List[String] = null

  // Input options
  var processDir : List[String] = _
  var className : String = _
  var methodName : String = _

  // Output options
  var outputDir : String = _
}
