import scalatags.Text.all._
import soot.Body
import soot.toolkits.graph.DirectedGraph

class Provenance (body : Body,
                  prefix : String,
                  slicedBodyOption : Option[Body],
                  cfgOption : Option[DirectedGraph[_]],
                  cdfg : UnitCdfgGraph,
                  acdfg : Acdfg) {
  def toHtml = {
    val sourceInfo = acdfg.toProtobuf.getSourceInfo

    var packageName : String = sourceInfo.getPackageName
    var className : String = sourceInfo.getClassName
    var methodName : String  = sourceInfo.getMethodName
    var methodLine : Int  = sourceInfo.getMethodLineNumber
    var fileName : String  = sourceInfo.getSourceClassName

    try {
      val sootMethod = body.getMethod
      val sootClass = sootMethod.getDeclaringClass
      /* Fallback to body if tags are not present */
      if (className == "") {
        className = sootClass.getName
      }
      if (methodName == "") methodName = sootMethod.getName
    } catch {
      case _ : Throwable => ()
    }

    val fullyQualifiedName = s"${className}.${methodName}"

    val sliceStr =
      slicedBodyOption match {
        case None => "Body was not sliced"
        case Some(slicedBody) => slicedBody.toString
      }

    scalatags.Text.all.html(
      scalatags.Text.all.head(
      ),
      scalatags.Text.all.body(
        scalatags.Text.all.h1(fullyQualifiedName),
        scalatags.Text.all.div(
          scalatags.Text.all.p(s"File (line: ${methodLine}): ${fileName}",
          ),
          scalatags.Text.all.p("Package: ", packageName),
          scalatags.Text.all.p("Class: ", className),
          scalatags.Text.all.p(s"Method: ${methodName}")
        ),

        scalatags.Text.all.h2("Abstract Control Data Flow Graph (ACDFG)"),
         scalatags.Text.all.img(src := prefix + ".acdfg.dot.png"),
        scalatags.Text.all.div(id := "acdfg",""),

        scalatags.Text.all.h2("Debug information"),
        scalatags.Text.all.div(
          scalatags.Text.all.h3("Jimple"),
          scalatags.Text.all.pre(scalatags.Text.all.code(body.toString)),
          scalatags.Text.all.h3("Sliced Jimple"),
          scalatags.Text.all.pre(scalatags.Text.all.code(sliceStr)),
          scalatags.Text.all.h3("Control Flow Graph (CFG)"),
          scalatags.Text.all.div(id := "cfg",
            scalatags.Text.all.img(src := prefix + ".cfg.dot.png")
          ),
          scalatags.Text.all.h3("Control Data Flow Graph (CDFG)"),
          scalatags.Text.all.div(id := "cdfg",
            scalatags.Text.all.img(src := prefix + ".cdfg.dot.png")
          ),
          // Footer
          scalatags.Text.all.p(scalatags.Text.all.em(
            "University of Colorado at Boulder"
          )),
          scalatags.Text.all.p(scalatags.Text.all.em(
            "CUPLV Lab"
          )),
          scalatags.Text.all.p(scalatags.Text.all.em(
            "DARPA MUSE Round II, Fixr 2016"
          )),
          scalatags.Text.all.p(scalatags.Text.all.em(
            "Sergio Mover, Rhys Braginton Pettee Olsen, Sriram Sankaranarayanan (PI)"
          ))
        ) // end of debug div
      )
    )
  }

}
