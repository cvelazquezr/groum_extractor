import soot.util.dot.{DotGraph, DotGraphConstants, DotGraphEdge, DotGraphNode}

/**
  * AcdfgToDotGraph
  *   Class implementing conversion from abstract control data flow graph (ACDFG)
  *   to .dot graph format.
  *
  *   @author Rhys Braginton Pettee Olsen <rhol9958@colorado.edu>
  *   @group  University of Colorado at Boulder CUPLV
  */

class AcdfgToDotGraph(acdfg : Acdfg) extends CFGToDotGraph {
  private def getDataNodeName(optNode : Option[Node]) : String = {
    val invokeeNode : Option[Node] = optNode
    val invokeeName = invokeeNode match {
      case Some(x) =>
        if (x.isInstanceOf[DataNode]) (x.asInstanceOf[DataNode]).name
        else "_"
      case None => "_"
    }
    invokeeName
  }


  def draw2() : DotGraph = {
    var canvas     : DotGraph       = initDotGraph(null)
    canvas.setGraphLabel("ACDFG")
    for (n <- acdfg.nodes) {
      var dotNode : DotGraphNode = canvas.drawNode(n._1.toString)
      n match {
        case n@(id : Long, node : DataNode) =>
          // "#" + id.toString + ": " + node.datatype.toString + " " + node.name
          val label = s"${node.name} : ${node.datatype} (# ${id})"
          dotNode.setLabel(label)
          dotNode.setStyle(DotGraphConstants.NODE_STYLE_DASHED)
          dotNode.setAttribute("shape", "ellipse")
        case n@(id : Long, node : MethodNode) =>
          val (invokeeId, invokeeName) =
            if (node.invokee.isDefined) {
              val invokeeName = getDataNodeName(acdfg.nodes.get(node.invokee.get))
              (s"(#${node.invokee.get}).", s"${invokeeName}.")
            } else ("","")

          val initVal = (List[String](),List[String]())
          val (argsId, argsName) = node.argumentIds.foldLeft(initVal)(
            (pair, id : Long) => {
              val varName = getDataNodeName(acdfg.nodes.get(id))
              val lres = s"(#${id.toString()})" :: pair._1
              val rres = varName ::  pair._2
              (lres, rres)
            }
          )

          val argsNameList = argsName.mkString(",")
          val argsIdList = argsId.mkString(",")
          val label = s"${invokeeName}${node.name}(${argsNameList})"
          val labelId = s"${invokeeId}(#${node.id})(${argsIdList})"

          dotNode.setLabel(s"${label}\\n${labelId}")
        case n@(id : Long, node : MiscNode) =>
          dotNode.setLabel("#" + id.toString)
        case n => Nil
      }
    }

    /* Print 1. Control edges */
    acdfg.edges.foldLeft(List[Edge]())((res : List[Edge], values : (Long, Edge)) => {
      val edge = values._2
      if (edge.isInstanceOf[ControlEdge] && ! edge.isInstanceOf[TransControlEdge])
        edge :: res
      else res
    })

    for (e <- acdfg.edges) {
      e match {
        case e@(id : Long, edge : DefEdge) => {
          var dotEdge : DotGraphEdge = canvas.drawEdge(e._2.from.toString, e._2.to.toString)
          dotEdge.setAttribute("color", "blue")
        }
        case e@(id : Long, edge : UseEdge) => {
          var dotEdge : DotGraphEdge = canvas.drawEdge(e._2.from.toString, e._2.to.toString)
          dotEdge.setAttribute("color", "red")
          dotEdge.setAttribute("Damping", "0.7")
        }
        case e@(id : Long, edge : TransControlEdge) => ()
        case e@(id : Long, edge : ControlEdge) => {
          var dotEdge : DotGraphEdge = canvas.drawEdge(e._2.from.toString, e._2.to.toString)
          dotEdge.setAttribute("color", "black")
          dotEdge.setAttribute("Damping", "0.7")
        }
        case _ => null
      }
    }

    canvas
  }


  def drawNode(canvas: DotGraph, id : Long) : scala.Unit = {
    val n : Node = acdfg.nodes(id)
    var dotNode : DotGraphNode = canvas.drawNode(id.toString)
    n match {
      case (node : DataNode) =>
        // "#" + id.toString + ": " + node.datatype.toString + " " + node.name
        val label = s"${node.name} : ${node.datatype} (# ${id})"
        dotNode.setLabel(label)
        dotNode.setStyle(DotGraphConstants.NODE_STYLE_DASHED)
        dotNode.setAttribute("shape", "ellipse")
        dotNode.setAttribute("group", "1")
      case (node : MethodNode) =>
        val (invokeeId, invokeeName) =
          if (node.invokee.isDefined) {
            val invokeeName = getDataNodeName(acdfg.nodes.get(node.invokee.get))
            (s"(#${node.invokee.get}).", s"${invokeeName}.")
          } else ("","")
        val initVal = (List[String](),List[String]())
        val (argsId, argsName) = node.argumentIds.foldLeft(initVal)(
          (pair, id : Long) => {
            val varName = getDataNodeName(acdfg.nodes.get(id))
            val lres = s"(#${id.toString()})" :: pair._1
            val rres = varName ::  pair._2
            (lres, rres)
          }
        )
        val argsNameList = argsName.mkString(",")
        val argsIdList = argsId.mkString(",")
        val label = s"${invokeeName}${node.name}(${argsNameList})"
        val labelId = s"${invokeeId}(#${node.id})(${argsIdList})"

        dotNode.setLabel(s"${label}\\n${labelId}")
        dotNode.setAttribute("group", "0")
        ()
      case (node : MiscNode) =>
        dotNode.setLabel(s"(#${id.toString})")
        dotNode.setAttribute("group", "0")
        ()
      case n => ()
    }
    ()
  }

  def drawEdge(canvas : DotGraph, e : Edge, id : Long, ignoreData : Boolean,
               edgeToDraw : List[Long]) : List[Long] = {
    e match {
      case (edge : DefEdge) => {
        if (! ignoreData) {
          var dotEdge : DotGraphEdge = canvas.drawEdge(e.from.toString, e.to.toString)
          dotEdge.setAttribute("color", "blue")
          edgeToDraw
        } else {
          id :: edgeToDraw
        }
      }
      case (edge : UseEdge) if (! ignoreData) => {
        var dotEdge : DotGraphEdge = canvas.drawEdge(e.from.toString, e.to.toString)
        dotEdge.setAttribute("color", "red")
        dotEdge.setAttribute("Damping", "0.7")
        edgeToDraw
      }
      case (edge : TransControlEdge) => edgeToDraw
      case (edge : ControlEdge) => {
        var dotEdge : DotGraphEdge = canvas.drawEdge(e.from.toString, e.to.toString)
        dotEdge.setAttribute("color", "black")
        dotEdge.setAttribute("Damping", "0.7")
        edgeToDraw
      }
      case (edge : ExceptionalControlEdge) => {
        var dotEdge : DotGraphEdge = canvas.drawEdge(e.from.toString, e.to.toString)
        dotEdge.setAttribute("color", "purple")
        dotEdge.setAttribute("Damping", "0.7")
        edgeToDraw
      }
      case _ => edgeToDraw
    }
  }

  def draw() : DotGraph = {

    var canvas : DotGraph = initDotGraph(null)
    var sourceInfo = acdfg.toProtobuf.getSourceInfo
    var acdfgLabel = if (null != sourceInfo && (sourceInfo.getClassName != "" ||
      sourceInfo.getMethodName != ""))
      s"${sourceInfo.getClassName}.${sourceInfo.getMethodName}"
    else "ACDFG"

    canvas.setGraphLabel(acdfgLabel)

    /* build a node -> edge map */
    val roots = scala.collection.mutable.HashSet[Long]()
    val node2edge = scala.collection.mutable.HashMap[Long, List[Long]]()
    acdfg.nodes.foreach { x => {
      node2edge += x._1 -> List[Long]()
      roots.add(x._1)
    }}
    acdfg.edges.foreach { (values : (Long, Edge)) => {
      val edge : Edge = values._2
      val edgeId : Long = values._1
      node2edge(edge.from) = edgeId :: node2edge(edge.from)
      if (roots.contains(edge.to)) roots.remove(edge.to)
    }}

    /* visit all the roots in dfs  */
    val visited = scala.collection.mutable.HashSet[Long]()
    def visitNodes(nodeId : Long, ignoreData : Boolean,
                   edgeToDraw : List[Long]) : List[Long] = {

      if (! visited.contains(nodeId)) {
        val node : Node = acdfg.nodes(nodeId)
        if (! (node.isInstanceOf[DataNode] && ignoreData)) {
          visited.add(nodeId)
          drawNode(canvas, nodeId)
          if (node2edge.contains(nodeId)) {
            val edges : List[Long] = node2edge(nodeId)
            var newEdgeToDraw : List[Long] = edgeToDraw
            newEdgeToDraw = edges.foldLeft (newEdgeToDraw) { (edgeToDraw, edgeId) => {
              val edge = acdfg.edges(edgeId)
              val newEdge = drawEdge(canvas, edge, edgeId, ignoreData, edgeToDraw)
              visitNodes(edge.to, ignoreData, newEdge)
            }}
            newEdgeToDraw
          }
          else edgeToDraw
        }
        else edgeToDraw
      }
      else edgeToDraw
    }

    /* vusut all the control edges */
    var useEdgesToAdd : List[Long] = roots.foldLeft (List[Long]()) {
      (list, x) => visitNodes(x, true, list) }

    /* visit the rest */
    useEdgesToAdd = acdfg.nodes.foldLeft(useEdgesToAdd) {
      (list,x) => visitNodes(x._1, false, list) }

    /* draw the use edges */
    useEdgesToAdd.foreach { id => drawEdge(canvas, acdfg.edges(id), id, false,
      List[Long]())}

    canvas
  }

}
