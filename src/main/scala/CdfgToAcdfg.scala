import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet
import org.slf4j.LoggerFactory
import org.slf4j.Logger
import soot.Value
import soot.Local
import soot.RefType
import soot.Body
import soot.jimple.IntConstant
import soot.jimple.Jimple
import soot.jimple.Expr
import soot.jimple.{ExprSwitch, StmtSwitch}
import soot.jimple.Constant
import soot.jimple.AssignStmt
import soot.jimple.AssignStmt
import soot.jimple.BreakpointStmt
import soot.jimple.EnterMonitorStmt
import soot.jimple.ExitMonitorStmt
import soot.jimple.GotoStmt
import soot.jimple.IdentityStmt
import soot.jimple.IdentityStmt
import soot.jimple.IfStmt
import soot.jimple.InvokeStmt
import soot.jimple.InvokeStmt
import soot.jimple.LookupSwitchStmt
import soot.jimple.NopStmt
import soot.jimple.RetStmt
import soot.jimple.ReturnStmt
import soot.jimple.ReturnVoidStmt
import soot.jimple.Stmt
import soot.jimple.TableSwitchStmt
import soot.jimple.ThrowStmt
import soot.jimple.FieldRef
import soot.jimple.InstanceFieldRef
import soot.jimple.internal.AbstractInstanceInvokeExpr
import soot.jimple.EqExpr
import soot.jimple.NeExpr
import soot.jimple.GeExpr
import soot.jimple.GtExpr
import soot.jimple.LeExpr
import soot.jimple.LtExpr
import soot.jimple.AndExpr
import soot.jimple.OrExpr
import soot.jimple.XorExpr
import soot.jimple.InterfaceInvokeExpr
import soot.jimple.SpecialInvokeExpr
import soot.jimple.StaticInvokeExpr
import soot.jimple.VirtualInvokeExpr
import soot.jimple.DynamicInvokeExpr
import soot.jimple.CmpExpr
import soot.jimple.CmpgExpr
import soot.jimple.CmplExpr
import soot.jimple.RemExpr
import soot.jimple.LengthExpr
import soot.jimple.ShlExpr
import soot.jimple.ShrExpr
import soot.jimple.UshrExpr
import soot.jimple.SubExpr
import soot.jimple.NewExpr
import soot.jimple.NewArrayExpr
import soot.jimple.NewMultiArrayExpr
import soot.jimple.DivExpr
import soot.jimple.MulExpr
import soot.jimple.AddExpr
import soot.jimple.NegExpr
import soot.jimple.InstanceOfExpr
import soot.jimple.CastExpr
import soot.toolkits.exceptions.ThrowAnalysisFactory
import soot.toolkits.exceptions.ThrowableSet
import soot.toolkits.graph.MHGDominatorsFinder
import soot.toolkits.graph.MHGPostDominatorsFinder
import soot.jimple.StaticFieldRef

/** Represent a simple transformation on the acdfg.
  * The semantic of remap info is the following:
  * - srcNode and dstNode are two nodes with a control edge
  * - intNodesList is a list of intermediate nodes used to create
  *   new edges
  *
  * The transformation tells to replace the edge (srcNode, dstNode)
  * with the set of edges:
  * (srcNode, n1), (n1,n2), ..., (n_k-1, n_k), (n_k, dstNode)
  * where n1,...,nk are the nodes in intNodesList
  */
case class RemapInfo(
                      srcNode : soot.Unit,
                      dstNode : soot.Unit,
                      intNodesList : List[Long]
                    )

/**
  *  Populates a acdfg from a cdfg
  *
  *  Assume the acdfg does not have any node/edges
  */
class CdfgToAcdfg(val cdfg : UnitCdfgGraph, val acdfg : Acdfg) {
  val logger : Logger = LoggerFactory.getLogger(classOf[CdfgToAcdfg])

  val ug = cdfg.asInstanceOf[soot.toolkits.graph.DirectedGraph[soot.Unit]]
  val dominators : MHGDominatorsFinder[soot.Unit] =
    new MHGDominatorsFinder[soot.Unit](ug)
  val postDominators : MHGPostDominatorsFinder[soot.Unit] =
    new MHGPostDominatorsFinder[soot.Unit](ug)
  val exceptionMap = CdfgToAcdfg.getExceptionMap(cdfg)

  val defEdges = cdfg.defEdges()
  val useEdges = cdfg.useEdges()

  /* Map from a soot object (value, unit) to a node id */
  var sootObjToId = scala.collection.mutable.HashMap[Any, Long]()
  /* Map from node IDs to the corresponding edge */
  var edgePairToId = scala.collection.mutable.HashMap[(Long, Long), Long]()

  /* Map from unit (the source node of the edges) to a list of remap
   * info.
   *
   * This is a representation of a list of edges that has been removed
   * from the cdfg and replaced in the acdfg
   *
   * These edges should be ignored in the creation of control edges.
   *
   * NOTE: this is a list and not a set, since we may have multiple
   * edges in from the same source/destination nodes.
   *
   */
  var remappedEdges = scala.collection.mutable.HashMap[soot.Unit,List[RemapInfo]]()

  /* Map from acdfg node ids to cdfg units.
   * The map is used to compute the domniator/post-dominator relationship
   */
  var unitsForDominator = scala.collection.mutable.HashMap[Long, soot.Unit]()


  /* helper class used to create an acdfg node from a node unit */
  val nodeCreator = new AcdfgSootStmtSwitch(this)
  val exprNodeCreator = new AcdfgSootExprSwitch(this)

  def lookupNodeId(v : Any) : Option[Long] = sootObjToId.get(v)

  def lookupOrCreateNode(v : Any) : Long = {
    val nodeId = lookupNodeId(v)

    val idVal = nodeId match {
      case Some(id) => id
      case _ => {
        val nodeId = v match {
          case local : Local => {
            val id = acdfg.getNewId
            val node = new VarDataNode(id, local.getName, local.getType.toString)
            sootObjToId += ((v, id))
            acdfg.addNode(node)
            id
          }
          case constant : Constant => {
            val id = acdfg.getNewId
            val node = new ConstDataNode(id, constant.toString, constant.getType.toString)
            sootObjToId += ((v, id))
            acdfg.addNode(node)
            id
          }
          case unit : soot.Unit => {
            unit.apply(nodeCreator)
            val nodeId = lookupNodeId(unit)
            nodeId.get
          }
          case arrayRef : soot.jimple.ArrayRef => {
            val valueBase = arrayRef.getBase()
            val id = lookupOrCreateNode(valueBase)
            lookupOrCreateNode(arrayRef.getIndex())
            id
          }
          case _ =>
            /* We do not know the type of v here, so we cannot create
             * a node. */
            throw new RuntimeException("Cannot create an ACDFG node for this object")
            0
        }
        nodeId
      }
    }
    idVal
  }

  /** Add a method node
    */
  def addMethodNode(unit : soot.Unit, assignee : Option[Long],
                    invokee : Option[Long], name : String, arguments : List[Long])  : Node = {
    val node = addMethodNodeAux(assignee, invokee, name, arguments)
    sootObjToId += ((unit, node.id))
    node
  }

  /** Add a method node
    */
  def addMethodNodeAux(assignee : Option[Long],
                       invokee : Option[Long], name : String, arguments : List[Long])  : Node = {
    val id = acdfg.getNewId
    val node = new MethodNode(id, assignee, invokee, name, arguments.toVector)
    acdfg.addNode(node)

    /* add a use edge for all the method nodes */
    arguments.foreach { fromId => addUseEdge(fromId, id) }
    node
  }


  def addMiscNode(unit : soot.Unit) : Node = {
    val id = acdfg.getNewId
    val node = new MiscNode(id)
    acdfg.addNode(node)
    sootObjToId += ((unit, id))
    node
  }

  def addUseEdge(fromId : Long, toId : Long): Unit = {
    val id = acdfg.getNewId
    val edge = new UseEdge(id, fromId, toId)
    acdfg.addEdge(edge)
    edgePairToId += (((fromId, toId), id))
  }

  def addDefEdges(unit : soot.Unit, unitId : Long): Unit = {
    def addDefEdge(fromId : Long, toId : Long): Unit = {
      val id = acdfg.getNewId
      val edge = new DefEdge(id, fromId, toId)
      acdfg.addEdge(edge)
      edgePairToId += (((fromId, toId), id))
    }

    if (!defEdges.containsKey(unit)) {
      return
      // defensive programming; don't know if defEdges has a value for every unit
    }
    val localIds : Array[Long] = defEdges.get(unit).iterator().map({local : soot.Local =>
      val toId = sootObjToId.get(local)
      toId match {
        case Some(id) => id
        case None => lookupOrCreateNode(local)
      }
    }).toArray
    localIds.foreach({localId : Long => addDefEdge(unitId, localId)
    })
  }

  private def addUseEdges(local : soot.Local, localId : Long): Unit = {
    if (!useEdges.containsKey(local)) {
      return
      // defensive programming; don't know if useEdges has a value for every local
    }
    val unitIds : Array[Long] = useEdges.get(local).iterator().map({unit : soot.Unit =>
      sootObjToId(unit)
    }).toArray
    unitIds.foreach({unitId : Long =>
      this.edgePairToId.get((localId, unitId)) match {
        case Some(x) => Unit
        case None => addUseEdge(localId, unitId)
      }
    })
  }

  private def addControlEdges(unit : soot.Unit, unitId : Long): Unit = {
    def addControlEdge(fromId : Long, toId : Long, labels : Acdfg.LabelsSet): Unit = {
      val id = acdfg.getNewId
      val edge = new ControlEdge(id, fromId, toId)
      acdfg.addEdge(edge, labels)
      edgePairToId += (((fromId, toId), id))
    }

    def addExceptionalEdge(fromId : Long, toId : Long,
                           exceptions : List[String], labels : Acdfg.LabelsSet): Unit = {
      val id = acdfg.getNewId
      val edge = new ExceptionalControlEdge(id, fromId, toId, exceptions)
      acdfg.addEdge(edge, labels)
      edgePairToId += (((fromId, toId), id))
    }

    def addControEdgeAux(from : soot.Unit, to : soot.Unit,
                         fromId : Long, toId : Long) : Unit = {

      val labelSet = CdfgToAcdfg.getLabelSet(from, to,
        dominators, postDominators)

      exceptionMap.get((from,to)) match {
        case Some(exceptions) =>
          addExceptionalEdge(fromId, toId, exceptions, labelSet)
        case None => addControlEdge(fromId, toId, labelSet)
      }

    }

    /** Find the first element in succToSkip that has succUnit as
      * dstNode.
      * Return in a pair the list without the found element and the
      * element (a list with the original elements and None if
      * succUnit is not found)
      *
      * NOTE: the order of the list is reversed at every call.
      */
    def findElem (succToSkip : List[RemapInfo], succUnit : soot.Unit) = {
      succToSkip.foldLeft ((succToSkip, Option.empty[RemapInfo])) ({ (res, x) => {
        res match {
          case (succToSkip, intEdge) =>
            if (! intEdge.isDefined) {
              if (x.dstNode == succUnit) (succToSkip, Some(x))
              else (x::succToSkip, intEdge)
            }
            else (x::succToSkip, intEdge)
        }
      }})
    }

    val unitId = sootObjToId(unit)

    /* find the list of nodes that should be remapped */
    val succToSkip : List[RemapInfo] = remappedEdges.get(unit) match {
      case Some(l) => l
      case _ => List[RemapInfo]()
    }

    /* Iterates through the successsors of unit.
     For the same unit, we try to find the edges to be redefined.
     Note that in each iteration of the foldLeft we remove such edge
     if it has been found.
     */
    cdfg.getSuccsOf(unit).iterator().foldLeft (succToSkip) (
      (succToSkip, succUnit) => {

        /* find the edges to be redifend */
        val res = findElem(succToSkip, succUnit)

        res match {
          case (newSuccToSkip, intEdge) =>
            intEdge match {
              case Some(remapEdge) => {
                def addRemapEdge(first : Option[soot.Unit], last : soot.Unit, intNodes : List[Long]) : Unit = {
                  def addFirst(x : Long) = {
                    val srcUnitId = sootObjToId(first.get)
                    addControEdgeAux(first.get, succUnit, srcUnitId, x)
                  }

                  def addLast(x : Long) = {
                    val srcUnit : soot.Unit = unitsForDominator.get(x).get
                    addControEdgeAux(srcUnit, last, x, sootObjToId(last))
                  }

                  intNodes match {
                    case Nil => ()
                    case x::Nil => {
                      /* first element */
                      if (first.isDefined) addFirst(x)
                      addLast(x)
                    }
                    case x :: y :: xs => {
                      if (first.isDefined) addFirst(x)
                      /* intermediary element  */
                      val srcUnit = unitsForDominator.get(x).get
                      val dstUnit = unitsForDominator.get(y).get
                      addControEdgeAux(srcUnit, dstUnit, x, y)
                      addRemapEdge(None, last, xs)
                    }
                    case _ => ()
                  }
                }
                addRemapEdge(Some(unit), succUnit, remapEdge.intNodesList)
              }
              case None => addControEdgeAux(unit, succUnit, unitId, sootObjToId(succUnit))
            }
            newSuccToSkip
        }
      })
  }

  private def addTransControlEdge(fromId : Long, toId : Long,
                                  labels : Acdfg.LabelsSet): Unit = {
    val id = acdfg.getNewId
    val edge = new TransControlEdge(id, fromId, toId)
    acdfg.addEdge(edge, labels)
    edgePairToId += (((fromId, toId), id))
  }

  private def computeTransClosure(): Unit = {
    val commandNodesMap = acdfg.nodes.filter(_._2.isInstanceOf[CommandNode])
    val commandNodes = commandNodesMap.values.toVector
    val commandNodeCount = commandNodes.size

    val idToUnit = sootObjToId map {_.swap}
    for ((k,v) <- unitsForDominator) {
      idToUnit += ((k,v))
    }


    var idToAdjIndex = new scala.collection.mutable.HashMap[Long, Int]
    commandNodesMap.zipWithIndex.foreach {
      case (((id : Long, _),index : Int)) =>
        idToAdjIndex += ((id, index))
    }
    var commandAdjMatrix = Array.ofDim[Boolean](commandNodeCount, commandNodeCount)
    var transAdjMatrix = Array.ofDim[Boolean](commandNodeCount, commandNodeCount)
    var stack      = new scala.collection.mutable.Stack[Node]
    var discovered = new scala.collection.mutable.ArrayBuffer[Node]

    /* initialize the stack */
    commandNodes.filter {node => true}.foreach{ n => stack.push(n) }

    // assemble adjacency matrix of commands w/out back-edges from DFS
    while (stack.nonEmpty) {
      val node = stack.pop()
      if ((!discovered.contains(node)) && (!stack.contains(node))) {
        discovered += node
        acdfg.edges.filter { case ((id, edge)) =>
          edge.from == node.id && idToAdjIndex.contains(edge.to)
        }.foreach { case ((id, edge)) =>
          val fromId = idToAdjIndex.get(edge.from).get
          val toId   = idToAdjIndex.get(edge.to).get
          commandAdjMatrix(fromId)(toId) = true
          val newNode = commandNodes(toId)
          if (!discovered.contains(newNode)) {
            stack.push(newNode)
          }
        }
      }
    }

    // assemble adjacency list of transitive closure w/ Floyd-Warshall
    // O((Vertices) ^ 3)
    val indices = 0 until commandNodeCount

    /*
     * NOTE: k,i,j major-to-minor order required;
     * although i,k,j major-to-minor order is best for locality,
     * a data dependency requires k,i,j order
     * to maintain the dynamic programming invariant
     */

    indices.foreach { k =>
      indices.foreach { i =>
        indices.foreach { j =>
          if (
            (commandAdjMatrix(i)(k) || transAdjMatrix(i)(k)) &&
              (commandAdjMatrix(k)(j) || transAdjMatrix(k)(j)) &&
              (!commandAdjMatrix(i)(j)) && (!transAdjMatrix(i)(j))
          ) {
            //changed = true
            transAdjMatrix(i)(j) = true
            val fromNode = idToUnit(commandNodes(i).id)
            val toNode = idToUnit(commandNodes(j).id)
            assert(fromNode.isInstanceOf[soot.Unit])
            assert(toNode.isInstanceOf[soot.Unit])

            val labelSet = CdfgToAcdfg.getLabelSet(
              fromNode.asInstanceOf[soot.Unit],
              toNode.asInstanceOf[soot.Unit], dominators, postDominators)
            addTransControlEdge(commandNodes(i).id, commandNodes(j).id, labelSet)
          }
        }
      }
    }
  }

  /** Fill the ACDFG visiting the graph
    */
  def fillAcdfg() = {
    val visited = HashSet[soot.Unit]()

    /* creates all the nodes and some def edges  */
    cdfg.getHeads().foreach(head =>
      createNodes(head, visited)
    )

    cdfg.getBody().getTraps().foreach { headTrap =>
      val head = headTrap.getBeginUnit()
      val handlerUnit = headTrap.getHandlerUnit()
      createNodes(head, visited)
      createNodes(handlerUnit, visited)
    }


    /* Add use edges */
    cdfg.localsIter().foreach {
      case n : Local => {
        if (! sootObjToId.contains(n)) {
          lookupOrCreateNode(n)
        }
        addUseEdges(n, sootObjToId(n))
      }
      case m =>
        logger.debug("    Data node of unknown type; ignoring...")
    }

    /* creates all the control edges */
    cdfg.unitIterator.foreach { n =>
      if (! sootObjToId.contains(n)) lookupOrCreateNode(n)

      addControlEdges(n, sootObjToId(n))
    }

    for ((key,value) <- sootObjToId) {
      if (key.isInstanceOf[soot.tagkit.Host]) {
        acdfg.addLine(value, SootHelper.getLineNumber(key.asInstanceOf[soot.tagkit.Host]))
      }
    }

    /* computes transitive clouse */
    logger.debug("### Computing transitive closure down to DFS of command edges...")
    computeTransClosure()
  }

  /** Fill the acdfg with all the nodes reachable from head
    */
  private def createNodes(unit : soot.Unit,
                          visited : HashSet[soot.Unit]) : Unit = {
    if (! visited.contains(unit)) {
      /* create node */
      try {
        unit.apply(nodeCreator)
      } catch {
        case e : scala.NotImplementedError =>
          println(unit)
          e.printStackTrace()
          throw e
      }

      /* create children */
      visited += unit
      cdfg.getSuccsOf(unit).iterator().foreach{ (succ) => createNodes(succ, visited) }
    }
  }
}

object CdfgToAcdfg {
  type TrapMap = scala.collection.immutable.HashMap[(soot.Unit,soot.Unit),
    List[String]]

  /** Return the set of labels for the edge fromfromUnit to toUnit
    *
    */
  def getLabelSet(fromUnit : soot.Unit, toUnit : soot.Unit,
                  dominators : MHGDominatorsFinder[soot.Unit],
                  postDominators : MHGPostDominatorsFinder[soot.Unit]) : Acdfg.LabelsSet = {

    val dominates = dominators.isDominatedBy(toUnit, fromUnit)
    val postDominated = postDominators.isDominatedBy(fromUnit, toUnit)

    val l1 = if (dominates) List(EdgeLabel.SRC_DOMINATE_DST) else List[EdgeLabel.Value]()
    val l2 = if (postDominated) EdgeLabel.DST_POSDOMINATE_SRC::l1 else l1

    val labelSet = scala.collection.immutable.HashSet[EdgeLabel.Value]() ++ l2
    labelSet
  }

  /** Return a map from couples of units to a list of exceptions */
  def getExceptionMap(cdfg : UnitCdfgGraph) : TrapMap = {
    /* Process all the traps in cdfg */

    val units = cdfg.getBody().getUnits()
    val trapList = cdfg.getBody().getTraps().toList
    /* Possible optimization: reuse the throwable analyis in the exceptional
     control flow graph */
    val throwAnalysis = ThrowAnalysisFactory.checkInitThrowAnalysis();


    val initMap = new scala.collection.immutable.HashMap[(soot.Unit,soot.Unit),List[String]]()
    val exceptionMap = trapList.foldLeft (initMap) {
      (exceptionMap, trap) => {
        val catcher : RefType = trap.getException().getType()
        var handler : soot.Unit = trap.getHandlerUnit();
        val trapException : String = trap.getException().toString
        val lastUnitInTrap : soot.Unit = units.getPredOf(trap.getEndUnit())

        def processUnits(trapUnitIter : Iterator[soot.Unit],
                         trapMap : TrapMap) : TrapMap = {
          if (trapUnitIter.hasNext) {
            val srcUnit = trapUnitIter.next()
            val thrownSet : ThrowableSet = throwAnalysis.mightThrow(srcUnit)
            val caughtException : ThrowableSet =
              thrownSet.whichCatchableAs(catcher).getCaught()

            if (! caughtException.equals(ThrowableSet.Manager.v().EMPTY)) {
              val key = (srcUnit, handler)

              val newTrapMap : TrapMap = trapMap.get(key) match {
                case Some(exceptionList) => {
                  val newList = trapException :: exceptionList
                  trapMap + (key -> newList)
                }
                case None => trapMap + (key -> List[String](trapException))
              }

              processUnits(trapUnitIter, newTrapMap)
            }
            else processUnits(trapUnitIter, trapMap)
          }
          else {
            trapMap
          }
        } /* processUnits */

        val trapUnitIter = units.iterator(trap.getBeginUnit(), lastUnitInTrap)

        processUnits(trapUnitIter, exceptionMap)
      } /* foldLeft on traps */
    }
    exceptionMap
  }
}

/**
  * Implements the case switch on the soot statement found in the cdfg units
  *
  */
class AcdfgSootStmtSwitch(cdfgToAcdfg : CdfgToAcdfg) extends
  StmtSwitch {
  private def getBody() : Body = cdfgToAcdfg.cdfg.getBody()

  private def addMisc(stmt : Stmt) : Node = {
    val miscNode = cdfgToAcdfg.addMiscNode(stmt)
    cdfgToAcdfg.addDefEdges(stmt, miscNode.id)
    miscNode
  }

  private def addMethod(stmt : Stmt, assignee : Option[Value]) = {
    val invokeExpr = stmt match {
      case s : InvokeStmt => s.asInstanceOf[InvokeStmt].getInvokeExpr
      case s : AssignStmt => stmt.getInvokeExpr
    }

    val declaringClass = invokeExpr.getMethod.getDeclaringClass.getName
    val methodName = invokeExpr.getMethod.getName
    val fullyQualName = declaringClass + "." + methodName

    val reversedNodeArgs = invokeExpr.getArgs.foldLeft (List[Long]()) {
      (nodeArgs, arg : Value) =>
        cdfgToAcdfg.lookupOrCreateNode(arg) :: nodeArgs
    }
    val nodeArgs = reversedNodeArgs.reverse

    val assigneeId = assignee match {
      case Some(a) => Some(cdfgToAcdfg.lookupOrCreateNode(a))
      case None => None
    }

    /* TODO check for static method invocation, better way to get the invokee */
    val invokee = invokeExpr match {
      case x : AbstractInstanceInvokeExpr =>
        Some(cdfgToAcdfg.lookupOrCreateNode(x.getBase()))
      case _ => None
    }

    val mNode = cdfgToAcdfg.addMethodNode(stmt, assigneeId,
      invokee, fullyQualName, nodeArgs)
    cdfgToAcdfg.addDefEdges(stmt, mNode.id)
  }


  /**
    * Add a predicate node with the give condition
    */
  private def addPredNode(condition : Value, negate : Boolean) : Long = {
    if (condition.isInstanceOf[Local] ||
      condition.isInstanceOf[Constant]) {
      val predName = if (negate) Predicates.IS_FALSE else Predicates.IS_TRUE
      val condNode = cdfgToAcdfg.lookupOrCreateNode(condition)
      val mNode = cdfgToAcdfg.addMethodNodeAux(None,
        None, predName, List[Long](condNode))
      mNode.id
    }
    else if (condition.isInstanceOf[Expr]) {
      /* create the node */
      cdfgToAcdfg.exprNodeCreator.polarity = ! negate
      condition.apply(cdfgToAcdfg.exprNodeCreator)
      val node = cdfgToAcdfg.exprNodeCreator.res.get
      node.id
    }
    else {
      throw new RuntimeException("Undhandled node type")
    }
  }

  def fieldAsMethodCall(stmt : soot.Unit,
                        field : FieldRef,
                        prefix : String,
                        assignee : Option[Long]) = {
    val base =
      if (field.isInstanceOf[InstanceFieldRef]) {
        val base = field.asInstanceOf[InstanceFieldRef].getBase()
        val baseId = cdfgToAcdfg.lookupOrCreateNode(base)
        Some(baseId)
      } else None

    val typeStr = field.getField.getType.toString()
    val fieldName = field.getField.getName
    val declClass = field.getField.getDeclaringClass

    val methodName =
      prefix + "." +
        declClass + "."  +
        fieldName + "_" +
        typeStr

    val mNode = cdfgToAcdfg.addMethodNode(stmt, assignee,
      base, methodName, List[Long]())
  }

  def caseAssignOrIdentity(stmt : soot.jimple.DefinitionStmt) : Unit = {
    val assignee = stmt.getLeftOp
    val rhs = stmt.getRightOp
    if (stmt.containsInvokeExpr()) {
      addMethod(stmt, Some(assignee))
    }
    else if (assignee.isInstanceOf[InstanceFieldRef] ||
      assignee.isInstanceOf[StaticFieldRef]) {
      fieldAsMethodCall(stmt, assignee.asInstanceOf[FieldRef],
        FakeMethods.SET_METHOD, None)
    }
    else if (rhs.isInstanceOf[InstanceFieldRef] ||
      rhs.isInstanceOf[StaticFieldRef]) {
      val assigneeId = cdfgToAcdfg.lookupOrCreateNode(assignee)
      fieldAsMethodCall(stmt, rhs.asInstanceOf[FieldRef],
        FakeMethods.GET_METHOD, Some(assigneeId))
    }
    else {
      val assigneeId = cdfgToAcdfg.lookupOrCreateNode(assignee)
      addMisc(stmt)
    }
  }

  override def caseAssignStmt(stmt : AssignStmt): Unit = caseAssignOrIdentity(stmt)

  override def caseBreakpointStmt(stmt: BreakpointStmt): Unit = addMisc(stmt)

  override def caseEnterMonitorStmt(stmt: EnterMonitorStmt): Unit = addMisc(stmt)

  override def caseExitMonitorStmt(stmt: ExitMonitorStmt): Unit = addMisc(stmt)

  override def caseGotoStmt(stmt: GotoStmt): Unit = addMisc(stmt)

  override def caseIdentityStmt(stmt: IdentityStmt): Unit = caseAssignOrIdentity(stmt)

  override def caseIfStmt(stmt: IfStmt): Unit = {
    val condition = stmt.getCondition()
    val trueBranch = stmt.getTarget()
    val falseBranch = getBody().getUnits().getSuccOf(stmt)

    /* The IfStmt node has two successors, the true and false branch.
     In the acdfg:
       - The IfStmt node becomes a MiscNode
       - We add a node in the true branch with the true condition
       - We add a node in the false branch with the negation of the true condition
     */

    /* create the misc node for the statment */
    val stmtNode = addMisc(stmt)
    /* create new node for the true successor */
    val trueBranchId = addPredNode(condition, false)
    cdfgToAcdfg.unitsForDominator += ((trueBranchId, trueBranch))
    val falseBranchId = addPredNode(condition, true)
    cdfgToAcdfg.unitsForDominator += ((falseBranchId, falseBranch))

    /* remap the edges */
    val trueRemap = RemapInfo(stmt, trueBranch, List(trueBranchId))
    val falseRemap = RemapInfo(stmt, falseBranch, List(falseBranchId))
    cdfgToAcdfg.remappedEdges += ((stmt, List(trueRemap, falseRemap)))
  }

  override def caseLookupSwitchStmt(stmt: LookupSwitchStmt): Unit = {
    val stmtNode = addMisc(stmt)

    val key : Value = stmt.getKey()

    val res : (Int, List[RemapInfo]) =
      stmt.getTargets().foldLeft ( (0, List[RemapInfo]()) ) ( (res, dstUnit) => {
        val (index, edges) = res

        val lookupValue : Int = stmt.getLookupValue(index);
        val lookupCondition = Jimple.v().newEqExpr(key,
          IntConstant.v(lookupValue))
        val currentBranchId = addPredNode(lookupCondition, false)

        cdfgToAcdfg.unitsForDominator += ((currentBranchId, dstUnit))
        val remap = RemapInfo(stmt, dstUnit, List(currentBranchId))

        (index + 1, remap :: edges)

      })

    /* warning: we have to reverse the list after foldLef */
    val (index, edges) = res match {
      case (index, edges) => (index, edges.reverse)
    }
    cdfgToAcdfg.remappedEdges += ((stmt, edges))
  }

  override def caseTableSwitchStmt(stmt: TableSwitchStmt): Unit = {
    val stmtNode = addMisc(stmt)
    val key : Value = stmt.getKey()

    def getEdgeListAux(stmt : TableSwitchStmt,
                       index : Int,  targetIndex : Int,
                       edges : List[RemapInfo]) : (Int, Int, List[RemapInfo]) = {

      if (index <= stmt.getHighIndex()) {
        val lookupCondition = Jimple.v().newEqExpr(key,
          IntConstant.v(index))
        val currentBranchId = addPredNode(lookupCondition, false)
        val dstUnit = stmt.getTarget(targetIndex)
        cdfgToAcdfg.unitsForDominator += ((currentBranchId, dstUnit))
        val remap = RemapInfo(stmt, dstUnit, List(currentBranchId))
        getEdgeListAux(stmt, index + 1, targetIndex + 1, remap :: edges)
      }
      else {
        (index, targetIndex, edges)
      }
    }

    val res : (Int, Int, List[RemapInfo]) =
      getEdgeListAux(stmt, stmt.getLowIndex(), 0, List[RemapInfo]())

    /* warning: we have to reverse the list after foldLef */
    val (_, _, edges) = res match {
      case (i, ti, edges) => (i, ti, edges.reverse)
    }
    cdfgToAcdfg.remappedEdges += ((stmt, edges))
  }

  override def caseInvokeStmt(stmt: InvokeStmt): Unit = addMethod(stmt, None)

  override def caseNopStmt(stmt: NopStmt): Unit = addMisc(stmt)

  override def caseRetStmt(stmt: RetStmt): Unit = addMisc(stmt)

  override def caseReturnStmt(stmt: ReturnStmt): Unit = {
    val retVal = stmt.getOp()
    val retValId = cdfgToAcdfg.lookupOrCreateNode(retVal)

    /* add a fake method node for the return with parameter */
    val mNode = cdfgToAcdfg.addMethodNode(stmt, None, None,
      FakeMethods.RETURN_METHOD, List[Long](retValId))
  }

  override def caseReturnVoidStmt(stmt: ReturnVoidStmt): Unit = {
    /* add a fake method node for the void return */
    val mNode = cdfgToAcdfg.addMethodNode(stmt, None, None,
      FakeMethods.RETURN_METHOD, List[Long]())
  }

  override def caseThrowStmt(stmt: ThrowStmt): Unit = addMisc(stmt)

  override def defaultCase(stmt: Any): Unit =
    throw new RuntimeException("Uknown statement " + stmt.toString)
}

/**
  * Generate predicate nodes (method call with particular names)
  * corresponding to the boolean condition used e.g. in a if, case ...
  *
  * The class can be extended to deal with other kind of predicates
  * (e.g. other expression as sums...)
  */
class AcdfgSootExprSwitch(cdfgToAcdfg : CdfgToAcdfg) extends ExprSwitch {

  var polarity : Boolean = true
  /* keep the last result */
  var res : Option[Node] = None

  def methodNodeHelper(predName : String, pList : List[Any]) = {

    val parList = pList.foldLeft (List[Long]()) (
      (res, element) => {
        val nodeId = cdfgToAcdfg.lookupOrCreateNode(element)
        nodeId :: res
      }
    ).reverse

    val mNode = cdfgToAcdfg.addMethodNodeAux(None,
      None, predName, parList)

    res = Some(mNode)
  }

  def handleBinary(v : Expr,
                   negFun : (Value,Value) => Expr,
                   op1 : Value, op2 : Value,
                   predName : String) : Unit = {
    if (! polarity) {
      val negated = negFun(op1, op2)
      polarity = true
      negated.apply(this)
    } else {
      methodNodeHelper(predName, List(op1, op2))
    }
  }

  /* base cases */
  def caseEqExpr(v : EqExpr) : Unit =
    handleBinary(v, Jimple.v().newNeExpr, v.getOp1, v.getOp2, Predicates.EQ)

  def caseNeExpr(v : NeExpr) : Unit =
    handleBinary(v, Jimple.v().newEqExpr, v.getOp1, v.getOp2, Predicates.NEQ)

  def caseGeExpr(v : GeExpr) : Unit =
    handleBinary(v, Jimple.v().newLtExpr, v.getOp1, v.getOp2, Predicates.GE)

  def caseGtExpr(v : GtExpr) : Unit =
    handleBinary(v, Jimple.v().newLeExpr, v.getOp1, v.getOp2, Predicates.GT)

  def caseLeExpr(v : LeExpr) : Unit =
    handleBinary(v, Jimple.v().newGtExpr, v.getOp1, v.getOp2, Predicates.LE)

  def caseLtExpr(v : LtExpr) : Unit =
    handleBinary(v, Jimple.v().newGeExpr, v.getOp1, v.getOp2, Predicates.LT)

  /* recursive cases */
  def caseAndExpr(v : AndExpr) : Unit = {
    if (! polarity) {
      val negated = Jimple.v().newOrExpr(Jimple.v().newNegExpr(v.getOp1),
        Jimple.v().newNegExpr(v.getOp2))
      polarity = true
      negated.apply(this)
    }
    else {
      methodNodeHelper(Predicates.AND, List(v.getOp1, v.getOp2))
    }
  }

  def caseOrExpr(v : OrExpr) : Unit = {
    if (! polarity) {
      val negated = Jimple.v().newAndExpr(Jimple.v().newNegExpr(v.getOp1),
        Jimple.v().newNegExpr(v.getOp2))
      polarity = true
      negated.apply(this)
    }
    else {
      methodNodeHelper(Predicates.OR, List(v.getOp1, v.getOp2))
    }
  }

  def caseXorExpr(v : XorExpr) : Unit = {
    if (! polarity) {
      val negated = Jimple.v().newNegExpr(v)
      polarity = true
      negated.apply(this)
    }
    else {
      methodNodeHelper(Predicates.XOR, List(v.getOp1, v.getOp2))
    }
  }

  def caseNegExpr(v : NegExpr) : Unit = {
    if (! polarity) {
      methodNodeHelper(Predicates.IS_FALSE, List(v.getOp))
    }
    else {
      methodNodeHelper(Predicates.NOT, List(v.getOp))
    }
  }

  /* they are not used as a predicate in Jimple*/
  def caseInterfaceInvokeExpr(v : InterfaceInvokeExpr) : Unit = defaultCase(v)
  def caseSpecialInvokeExpr(v : SpecialInvokeExpr) : Unit = defaultCase(v)
  def caseStaticInvokeExpr(v : StaticInvokeExpr) : Unit = defaultCase(v)
  def caseVirtualInvokeExpr(v : VirtualInvokeExpr) : Unit = defaultCase(v)
  def caseDynamicInvokeExpr(v : DynamicInvokeExpr) : Unit = defaultCase(v)

  /* unknown */
  def caseCmpExpr(v : CmpExpr) : Unit = defaultCase(v)
  def caseCmpgExpr(v : CmpgExpr) : Unit = defaultCase(v)
  def caseCmplExpr(v : CmplExpr) : Unit = defaultCase(v)
  def caseRemExpr(v : RemExpr) : Unit = defaultCase(v)
  def caseLengthExpr(v : LengthExpr) : Unit = defaultCase(v)
  def caseShlExpr(v : ShlExpr) : Unit = defaultCase(v)
  def caseShrExpr(v : ShrExpr) : Unit = defaultCase(v)
  def caseUshrExpr(v : UshrExpr) : Unit = defaultCase(v)
  def caseSubExpr(v : SubExpr) : Unit = defaultCase(v)

  /* Not used as a predicate in Jimple now   */
  def caseNewExpr(v : NewExpr) : Unit = defaultCase(v)
  def caseNewArrayExpr(v : NewArrayExpr) : Unit = defaultCase(v)
  def caseNewMultiArrayExpr(v : NewMultiArrayExpr) : Unit = defaultCase(v)
  def caseDivExpr(v : DivExpr) : Unit = defaultCase(v)
  def caseMulExpr(v : MulExpr) : Unit = defaultCase(v)
  def caseAddExpr(v : AddExpr) : Unit = defaultCase(v)
  def caseInstanceOfExpr(v : InstanceOfExpr) : Unit = defaultCase(v)
  def caseCastExpr(v : CastExpr) : Unit = defaultCase(v)

  def defaultCase(obj : Object) : Unit =
  /* if this exception is raised then we are not handling some expression
   that can be used in a conditional */
    throw new RuntimeException("Unhandled expression " + obj.toString)
}
