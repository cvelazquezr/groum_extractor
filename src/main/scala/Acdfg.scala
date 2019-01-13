import ProtoAcdfg.Acdfg.SourceInfo
import org.slf4j.Logger
import org.slf4j.LoggerFactory

abstract class Node {
  def id : Long

  override def toString = this match {
    case n : MethodNode =>
      n.getClass.getSimpleName + "(" +
        "id: "+ n.id.toString + ", " +
        "assignee" + n.assignee + ", " +
        "invokee: " + n.invokee.toString + ", " +
        "name: " + n.name.toString + ", " +
        "arguments: [" + n.argumentIds.map(_.toString).mkString(", ") + "]" +
        ")"
    case n : ConstDataNode =>
      n.getClass.getSimpleName + "(" +
        "id: " + n.id.toString + ", " +
        "value: " + n.name +
        "type: " + n.datatype +
        ")"
    case n : DataNode =>
      n.getClass.getSimpleName + "(" +
        "id: "+ n.id.toString + ", " +
        "name: "   + n.name +
        "type: "   + n.datatype +
        ")"
    case n => this.getClass.getSimpleName + "(" + id.toString + ")"
  }
}

abstract class CommandNode extends Node

abstract class DataNode() extends Node {
  override val id : Long
  val name : String
  val datatype : String
}

case class VarDataNode(override val id : Long,
                       override val name : String,
                       override val datatype : String) extends DataNode

case class ConstDataNode(
                          override val id : Long,
                          override val name : String, /* the value of the constant */
                          override val datatype : String) extends DataNode

case class MethodNode(override val id : Long,
                      assignee : Option[Long],
                      invokee : Option[Long],
                      name : String,
                      argumentIds : Vector[Long]
                     ) extends CommandNode

case class MiscNode(override val id : Long) extends CommandNode

/* Edges */
abstract class Edge {
  val to   : Long
  val from : Long
  val id   : Long

  override def toString = this match {
    case n : ExceptionalControlEdge =>
      this.getClass.getSimpleName +
        "(id: "     + n.id.toString   +
        ", to: "    + n.to.toString   +
        ", from: "  + n.from.toString +
        ", exceptions: [" + n.exceptions.mkString(",") +
        "])"
    case n =>
      this.getClass.getSimpleName +
        "(id: "     + id.toString   +
        ", to: "    + to.toString   +
        ", from: "  + from.toString +
        ")"
  }
}



case class DefEdge(
                    override val id   : Long,
                    override val from : Long,
                    override val to   : Long
                  ) extends Edge

case class UseEdge(
                    override val id   : Long,
                    override val from : Long,
                    override val to   : Long
                  ) extends Edge

case class ControlEdge(
                        override val id   : Long,
                        override val from : Long,
                        override val to   : Long
                      ) extends Edge

case class TransControlEdge(
                             override val id   : Long,
                             override val from : Long,
                             override val to   : Long
                           ) extends Edge

/** Represent an exceptional control flow edge
  *  The edge represent the change of control flow from a node to another
  *  node due to an exception.
  *
  *  The same edge can be labeled with multiple exceptions.
  */
case class ExceptionalControlEdge (
                                    override val id   : Long,
                                    override val from : Long,
                                    override val to   : Long,
                                    /* list of exceptions (string) catched by the exceptional edge */
                                    val exceptions : List[String]
                                  ) extends Edge

case class AdjacencyList(nodes : Vector[Node], edges : Vector[Edge])

object EdgeLabel extends Enumeration {
  type EdgeLabel = Value
  val SRC_DOMINATE_DST, DST_POSDOMINATE_SRC = Value
}


/** Defines the name of the artificial methods introduced
  * when building the ACDFG
  */
object FakeMethods {
  val RETURN_METHOD = "return"
  val GET_METHOD = "<get>"
  val SET_METHOD = "<set>"
}

/** Defines a set of artificial predicates that we treat as
  *  method call
  */
object Predicates {
  val IS_TRUE= "is_true"
  val IS_FALSE= "is_false"
  val EQ = "EQ"
  val NEQ = "NEQ"
  val GE = "GE"
  val GT = "GT"
  val LE = "LE"
  val LT = "LT"
  val NOT = "NOT"
  val AND = "AND"
  val OR = "OR"
  val XOR = "XOR"
}

class Acdfg(adjacencyList: AdjacencyList,
            cdfg : UnitCdfgGraph,
            protobuf : ProtoAcdfg.Acdfg,
            sourceInfo : SourceInfo,
            provenancePath : String) {

  val logger : Logger = LoggerFactory.getLogger(classOf[Acdfg])

  /*
   * Edges and nodes
   * Design rationale: our graph will be very sparse; want indexing by
   * ID to be fast
   */
  var edges = scala.collection.mutable.HashMap[Long, Edge]()
  var nodes = scala.collection.mutable.HashMap[Long, Node]()
  var nodesToLineNumber = scala.collection.mutable.HashMap[Long, Int]()

  /**
    *  Map from the edge id to a set of labels
    */
  var edgesLabel = scala.collection.mutable.HashMap[Long, Acdfg.LabelsSet]()

  var methodBag = new scala.collection.mutable.ArrayBuffer[String]()
  var freshIds = new scala.collection.mutable.PriorityQueue[Long]().+=(0)


  private def prepareMethodBag() = {
    logger.debug("### Preparing bag of methods...")
    nodes.filter(_._2.isInstanceOf[MethodNode]).foreach { case (_, node) =>
      methodBag.append(node.asInstanceOf[MethodNode].name)
    }
    methodBag = methodBag.sorted
  }

  /* Internal Protobuf value generated as needed

   [SM] What does it happen if someone access the protobuffer field,
   then changes the ACDFG, and re-access the pb field?
   I think the pb field will not be updated.
   This is a bug.
  */
  private lazy val pb : ProtoAcdfg.Acdfg = {
    var builder : ProtoAcdfg.Acdfg.Builder = ProtoAcdfg.Acdfg.newBuilder()

    /* add the edges */
    edges.foreach {
      case (id : Long, edge : ControlEdge) =>
        val protoControlEdge: ProtoAcdfg.Acdfg.ControlEdge.Builder =
          ProtoAcdfg.Acdfg.ControlEdge.newBuilder()
        protoControlEdge.setId(id)
        protoControlEdge.setFrom(edge.from)
        protoControlEdge.setTo(edge.to)
        builder.addControlEdge(protoControlEdge)
      case (id : Long, edge : DefEdge) =>
        val protoDefEdge: ProtoAcdfg.Acdfg.DefEdge.Builder =
          ProtoAcdfg.Acdfg.DefEdge.newBuilder()
        protoDefEdge.setId(id)
        protoDefEdge.setFrom(edge.from)
        protoDefEdge.setTo(edge.to)
        builder.addDefEdge(protoDefEdge)
      case (id : Long, edge : UseEdge) =>
        val protoUseEdge: ProtoAcdfg.Acdfg.UseEdge.Builder =
          ProtoAcdfg.Acdfg.UseEdge.newBuilder()
        protoUseEdge.setId(id)
        protoUseEdge.setFrom(edge.from)
        protoUseEdge.setTo(edge.to)
        builder.addUseEdge(protoUseEdge)
      case (id : Long, edge : TransControlEdge) =>
        val protoTransEdge: ProtoAcdfg.Acdfg.TransEdge.Builder =
          ProtoAcdfg.Acdfg.TransEdge.newBuilder()
        protoTransEdge.setId(id)
        protoTransEdge.setFrom(edge.from)
        protoTransEdge.setTo(edge.to)
        builder.addTransEdge(protoTransEdge)
      case (id : Long, edge : ExceptionalControlEdge) =>
        val protoEdge = ProtoAcdfg.Acdfg.ExceptionalControlEdge.newBuilder()
        protoEdge.setId(id)
        protoEdge.setFrom(edge.from)
        protoEdge.setTo(edge.to)
        edge.exceptions.foreach { x => protoEdge.addExceptions(x) }
        builder.addExceptionalEdge(protoEdge)
    }

    /* Add the node labels */
    edgesLabel.foreach {
      case (id : Long, label : Acdfg.LabelsSet) => {
        val edgeBuilder = ProtoAcdfg.Acdfg.LabelMap.newBuilder()
        edgeBuilder.setEdgeId(id)
        label.foreach { x => x match {
          case x if x == EdgeLabel.SRC_DOMINATE_DST => edgeBuilder.addLabels(ProtoAcdfg.Acdfg.EdgeLabel.DOMINATE)
          case x if x == EdgeLabel.DST_POSDOMINATE_SRC => edgeBuilder.addLabels(ProtoAcdfg.Acdfg.EdgeLabel.POSTDOMINATED)
          case _ => ???
        }
        }
        builder.addEdgeLabels(edgeBuilder)
      }
    }

    var methodBag : scala.collection.mutable.ArrayBuffer[String] =
      new scala.collection.mutable.ArrayBuffer[String]()

    /* Add the nodes */
    nodes.foreach {
      case (id : Long, node : DataNode) =>
        val protoDataNode : ProtoAcdfg.Acdfg.DataNode.Builder =
          ProtoAcdfg.Acdfg.DataNode.newBuilder()
        protoDataNode.setId(id)
        protoDataNode.setName(node.name)
        protoDataNode.setType(node.datatype)

        node match {
          case x : VarDataNode => protoDataNode.setDataType(ProtoAcdfg.Acdfg.DataNode.DataType.DATA_VAR)
          case x : ConstDataNode => protoDataNode.setDataType(ProtoAcdfg.Acdfg.DataNode.DataType.DATA_CONST)
        }

        builder.addDataNode(protoDataNode)
      case (id : Long, node : MiscNode) =>
        val protoMiscNode : ProtoAcdfg.Acdfg.MiscNode.Builder =
          ProtoAcdfg.Acdfg.MiscNode.newBuilder()
        protoMiscNode.setId(id)
        builder.addMiscNode(protoMiscNode)
      case (id : Long, node : MethodNode) =>
        val protoMethodNode : ProtoAcdfg.Acdfg.MethodNode.Builder =
          ProtoAcdfg.Acdfg.MethodNode.newBuilder()
        protoMethodNode.setId(id)
        if (node.assignee.isDefined) {
          protoMethodNode.setAssignee(node.assignee.get)
        }
        if (node.invokee.isDefined) {
          protoMethodNode.setInvokee(node.invokee.get)
        }
        node.argumentIds.foreach(protoMethodNode.addArgument)
        protoMethodNode.setName(node.name)
        builder.addMethodNode(protoMethodNode)

        // add method to bag of methods representation
        methodBag.append(node.name)
    }

    /* copy the repotag informations */
//    if (null != this.gitHubRecord) {
//      val protoRepoTag = ProtoAcdfg.Acdfg.RepoTag.newBuilder()
//      protoRepoTag.setUserName(this.gitHubRecord.userName)
//      protoRepoTag.setRepoName(this.gitHubRecord.repoName)
//      protoRepoTag.setUrl(this.gitHubRecord.url)
//      protoRepoTag.setCommitHash(this.gitHubRecord.commitHash)
//      builder.setRepoTag(protoRepoTag)
//    }

    if (null != this.sourceInfo) {
      val protoSrcTag = ProtoAcdfg.Acdfg.SourceInfo.newBuilder()
      protoSrcTag.setPackageName(sourceInfo.getPackageName)
      protoSrcTag.setClassName(sourceInfo.getClassName)
      protoSrcTag.setMethodName(sourceInfo.getMethodName)
      protoSrcTag.setClassLineNumber(sourceInfo.getClassLineNumber)
      protoSrcTag.setMethodLineNumber(sourceInfo.getMethodLineNumber)
      protoSrcTag.setSourceClassName(sourceInfo.getSourceClassName)
      protoSrcTag.setAbsSourceClassName(sourceInfo.getAbsSourceClassName)
      builder.setSourceInfo(protoSrcTag)
    }

    // Add nodes to lines mapping
    for ((nodeId, nodeLine) <- nodesToLineNumber) {
      val protoLine : ProtoAcdfg.Acdfg.LineNum.Builder =
        ProtoAcdfg.Acdfg.LineNum.newBuilder()
      protoLine.setId(nodeId)
      protoLine.setLine(nodeLine)

      builder.addNodeLines(protoLine)
    }

    // add bag of methods
    val protoMethodBag = ProtoAcdfg.Acdfg.MethodBag.newBuilder()
    methodBag.sorted.foreach(protoMethodBag.addMethod)
    builder.setMethodBag(protoMethodBag)

    builder.setProvenancePath(provenancePath)

    builder.build()
  } /* creation of pb */

  // TODO: fix
  def getNewId : Long = {
    val newId = freshIds.dequeue()
    if (freshIds.isEmpty) {
      // Must maintain invariant: freshIds always has at least one fresh id
      freshIds.enqueue(newId + 1)
    }
    newId
  }

  private def removeId(id : Long) = freshIds.enqueue(id)

  def addEdge(edge : Edge) : Unit = {
    addEdge(edge, scala.collection.immutable.HashSet[EdgeLabel.Value]())
  }

  def addEdge(edge : Edge, labels : Acdfg.LabelsSet) : Unit = {
    edges += ((edge.id, edge))
    edgesLabel += ((edge.id, labels))
  }

  def addNode(node : Node) : (Long, Node) = {
    val oldCount = nodes.size
    nodes.+=((node.id, node))
    val newCount = nodes.size
    assert(oldCount + 1 == newCount)
    (node.id, node)
  }

  def addLine(id : Long, lineNumber : Int) = {
    nodesToLineNumber.+=((id,lineNumber))
  }


  def removeEdge(to : Long, from : Long) = {
    val id = edges.find {pair => (pair._2.from == from) && (pair._2.to == to) }.get._1
    edges.remove(id)
  }

  def removeEdgesOf(id : Long) = {
    val edgesOfId = edges.find({
      pair => (pair._2.from == id) || pair._2.to == id
    })
    edgesOfId.foreach(pair => {
      edges.remove(pair._1)
      edgesLabel.remove(pair._1)
    })
  }

  def removeDataNode(name : String) = {
    nodes.find(pair => pair._2.isInstanceOf[DataNode]).foreach(
      pair => {
        if (pair._2.asInstanceOf[DataNode].name == name) {
          nodes.remove(pair._1)
        }
      }
    )
  }

  def removeNode(id : Long) = {
    nodes.remove(id)
    removeId(id)
  }

  /**
    * Creates a ACDFG from a CDFG
    */
  def this(cdfg : UnitCdfgGraph, sourceInfo : SourceInfo, prov_path : String) = {
    this(null, cdfg, null, sourceInfo, prov_path)
//    assert(this.gitHubRecord == gitHubRecord)

    // the following are used to make lookup more efficient

    val converter = new CdfgToAcdfg(cdfg, this)
    converter.fillAcdfg()

    prepareMethodBag()
  }

//  /**
//    *  Creates the ACDFG structure from the protobuf representation
//    */
//  def this(protobuf : ProtoAcdfg.Acdfg) = {
//    this(null, null, protobuf, null, null, protobuf.getProvenancePath)
////      GitHubRecord(
////        if (protobuf.getRepoTag.hasUserName)
////          protobuf.getRepoTag.getUserName else "",
////        if (protobuf.getRepoTag.hasRepoName)
////          protobuf.getRepoTag.getRepoName else "",
////        if (protobuf.getRepoTag.hasUrl)
////          protobuf.getRepoTag.getUrl else "",
////        if (protobuf.getRepoTag.hasCommitHash)
////          protobuf.getRepoTag.getCommitHash else ""
////      ),
////      SourceInfo(protobuf.getSourceInfo.getPackageName,
////        protobuf.getSourceInfo.getClassName,
////        protobuf.getSourceInfo.getMethodName,
////        protobuf.getSourceInfo.getClassLineNumber,
////        protobuf.getSourceInfo.getMethodLineNumber,
////        protobuf.getSourceInfo.getSourceClassName,
////        protobuf.getSourceInfo.getAbsSourceClassName
////      ),
////      protobuf.getProvenancePath
////    )
//
//    /* add data nodes */
//    protobuf.getDataNodeList.foreach { dataNode =>
//      var node = dataNode.getDataType match {
//        case x if x == ProtoAcdfg.Acdfg.DataNode.DataType.DATA_VAR =>
//          new VarDataNode(dataNode.getId, dataNode.getName, dataNode.getType)
//        case x if x == ProtoAcdfg.Acdfg.DataNode.DataType.DATA_CONST =>
//          new ConstDataNode(dataNode.getId, dataNode.getName, dataNode.getType)
//      }
//
//      addNode(node)
//    }
//    /* method nodes */
//    protobuf.getMethodNodeList.foreach { methodNode =>
//      val invokee = if (methodNode.hasInvokee) Some(methodNode.getInvokee) else None
//      val assignee = if (methodNode.hasAssignee) Some(methodNode.getAssignee) else None
//      val node = new MethodNode(methodNode.getId, assignee, invokee, methodNode.getName,
//        methodNode.getArgumentList.asScala.toVector.map(_.longValue()))
//      addNode(node)
//      (methodNode.getId, node)
//    }
//    /* misc nodes */
//    protobuf.getMiscNodeList.foreach { miscNode =>
//      val node = new MiscNode(miscNode.getId)
//      addNode(node)
//      (miscNode.getId, node)
//    }
//
//    /* edges */
//    protobuf.getControlEdgeList.foreach { protoEdge =>
//      val edge = new ControlEdge(protoEdge.getId, protoEdge.getFrom, protoEdge.getTo)
//      addEdge(edge)
//    }
//
//    protobuf.getUseEdgeList.foreach { protoEdge =>
//      val edge = new UseEdge(protoEdge.getId, protoEdge.getFrom, protoEdge.getTo)
//      addEdge(edge)
//    }
//
//    protobuf.getDefEdgeList.foreach { protoEdge =>
//      val edge = new DefEdge(protoEdge.getId, protoEdge.getFrom, protoEdge.getTo)
//      addEdge(edge)
//    }
//
//    protobuf.getTransEdgeList.foreach { protoEdge =>
//      val edge = new TransControlEdge(protoEdge.getId, protoEdge.getFrom, protoEdge.getTo)
//      addEdge(edge)
//    }
//
//    protobuf.getExceptionalEdgeList.foreach { protoEdge =>
//      val exception = protoEdge.getExceptionsList.toList
//      val edge = new ExceptionalControlEdge(protoEdge.getId, protoEdge.getFrom,
//        protoEdge.getTo, exception)
//      addEdge(edge)
//    }
//
//    /* get the edge labels */
//    protobuf.getEdgeLabelsList.foreach { labelMap => {
//      val labelsList = labelMap.getLabelsList.foldLeft(List[EdgeLabel.Value]())({
//        (res, x) => {
//          x match {
//            case x if x == ProtoAcdfg.Acdfg.EdgeLabel.DOMINATE =>
//              EdgeLabel.SRC_DOMINATE_DST :: res
//            case x if x == ProtoAcdfg.Acdfg.EdgeLabel.POSTDOMINATED =>
//              EdgeLabel.DST_POSDOMINATE_SRC :: res
//            case _ => ???
//          }
//        }
//      })
//      val labelSet = scala.collection.immutable.HashSet[EdgeLabel.Value]() ++ labelsList
//      this.edgesLabel += ((labelMap.getEdgeId, labelSet))
//    }}
//
//    // Add nodes to lines mapping
//    protobuf.getNodeLinesList.foreach { protoLine =>
//      this.addLine(protoLine.getId, protoLine.getLine)
//    }
//
//    if ((!protobuf.getMethodBag.isInitialized) ||
//      (protobuf.getMethodBag.getMethodCount == 0)) {
//      prepareMethodBag()
//    } else {
//      protobuf.getMethodBag.getMethodList.foreach { method => methodBag.append(method) }
//    }
//  } /* end of constructor from protobuf */


  def union(that : Acdfg) : AdjacencyList =
    AdjacencyList(
      (this.nodes.values.toSet | this.nodes.values.toSet).toVector,
      (this.edges.values.toSet | this.edges.values.toSet).toVector
    )
  def |(that : Acdfg) = union(that)

  def disjointUnion(that : Acdfg) : AdjacencyList =
    AdjacencyList(
      ((this.nodes.values.toSet &~ that.nodes.values.toSet) ++
        (that.nodes.values.toSet -- this.nodes.values.toSet)).toVector,
      ((this.edges.values.toSet -- that.edges.values.toSet) ++
        (that.edges.values.toSet -- this.edges.values.toSet)).toVector
    )
  def +|(that : Acdfg) = disjointUnion(that)

  def intersection(that : Acdfg) : AdjacencyList =
    AdjacencyList(
      (this.nodes.values.toSet | this.nodes.values.toSet).toVector,
      (this.edges.values.toSet | this.edges.values.toSet).toVector
    )
  def &(that : Acdfg) = intersection(that)

  def diff(that : Acdfg) : AdjacencyList =
    AdjacencyList(
      (this.nodes.values.toSet -- this.nodes.values.toSet).toVector,
      (this.edges.values.toSet -- this.edges.values.toSet).toVector
    )
  def --(that : Acdfg) = diff(that)

  def equals(that : Acdfg) : Boolean = {
    val du = this +| that
    val eqnodes = du.nodes.isEmpty
    val eqedges = du.edges.isEmpty
//    val eqgh = this.gitHubRecord.equals(that.getGitHubRecord)
//    val eqsource = this.sourceInfo.equals(that.getSourceInfo)

    eqnodes && eqedges //&& eqgh && eqsource
  }

  def == (that : Acdfg) : Boolean =
    if (that != null) this.equals(that) else false


  def toProtobuf = pb
//  def getGitHubRecord = gitHubRecord
//  def getSourceInfo = sourceInfo

  override def toString = {
    // Inefficient - TODO: use buffer instead of string concat
    var output : String = "ACDFG:" + "\n"

    output += ("  " + "Nodes:\n")
    nodes.foreach(node => output += ("    " + node.toString()) + "\n")
    output += "\n"

    output += ("  " + "Edges:\n")
    edges.foreach(edge => {
      output += ("    " + edge.toString()) + "\n"
      this.edgesLabel.get(edge._1) match {
        case Some(labelsList) => {
          val labels = labelsList.foldLeft ("    Labels:"){ (res,x) => res + " " + x.toString }

          output += labels + "\n"
        }
        case None => ()
      }
    })

    output
  }
}

object Acdfg {
  type LabelsSet = scala.collection.immutable.Set[EdgeLabel.Value]

  def isFakeMethod(methodName : String) : Boolean = {
    return methodName.startsWith(FakeMethods.RETURN_METHOD) ||
      methodName.startsWith(FakeMethods.GET_METHOD) ||
      methodName.startsWith(FakeMethods.SET_METHOD)
  }
}
