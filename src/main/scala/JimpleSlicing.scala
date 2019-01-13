import org.slf4j.{Logger, LoggerFactory}
import soot.jimple.InvokeExpr
import soot.{Body, PatchingChain, Unit => SootUnit}

import scala.collection.JavaConverters._

/**
  * Remove the method invocations from body that to not match the SliceCriteria
  *
  * Possibly we lose the def and use of variables to methods that will be thrown away
  * in the groum.graphs.slice.
  *
  */

class JimpleSlicing(body : Body, sliceCriteria: SliceCriteria) {

  val logger : Logger = LoggerFactory.getLogger(this.getClass)

  def sliceJimple() {
    def sliceJimpleRec(unitIter: Iterator[SootUnit],
                       unitsToRemove: List[SootUnit]): List[SootUnit] = {
      if (unitIter.hasNext) {
        val unit: SootUnit = unitIter.next()
        val unit_is_seed = sliceCriteria.is_seed(unit)

        val removeBoxes = unit.getUseBoxes.asScala
        val toRemove = removeBoxes.foldLeft(false) { (res, valBox) => {
            val v = valBox.getValue

            if (v.isInstanceOf[InvokeExpr] && (!unit_is_seed)) {
              logger.info("Removing..." + unit.toString())
              true
            }
            else {
              res
            }
          }
        }
        if (toRemove) {
          sliceJimpleRec(unitIter, unit :: unitsToRemove)
        }
        else {
          sliceJimpleRec(unitIter, unitsToRemove)
        }
      }
      else {
        unitsToRemove
      }
    }

    val pc: PatchingChain[SootUnit] = body.getUnits
    // Find all the units that are not method calls and do not match the seed
    val unitsToRemove = sliceJimpleRec(pc.iterator.asScala, List[SootUnit]())
    // Remove all these units
    unitsToRemove.foreach(u => pc.remove(u))
  }

}
