import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import soot.Body;
import soot.Local;
import soot.Unit;
import soot.util.dot.DotGraph;
import soot.util.dot.DotGraphConstants;
import soot.util.dot.DotGraphEdge;
import soot.util.dot.DotGraphNode;

public class CDFGToDotGraph extends CFGToDotGraph {
    public CDFGToDotGraph() {
    }

    public DotGraph drawCFG(UnitCdfgGraph graph, Body body) {
        DotGraph canvas = initDotGraph(body);
        DotNamer namer = new DotNamer((int)(graph.size()/0.7f), 0.7f);
        NodeComparator comparator = new NodeComparator(namer);

        Set<Object> varNodes = new HashSet<Object>();
        Set<Object> useEdges= new HashSet<Object>();
        Set<Object> defEdges= new HashSet<Object>();

        // To facilitate comparisons between different groum.graphs of the same
        // method, prelabel the nodes in the order they appear
        // in the iterator, rather than the order that they appear in the
        // graph traversal (so that corresponding nodes are more likely
        // to have the same label in different groum.graphs of a given method).
        for (Iterator nodesIt = graph.iterator(); nodesIt.hasNext(); ) {
            String junk = namer.getName(nodesIt.next());
        }

        for (Iterator<Local> iterLocals = graph.localsIter(); iterLocals.hasNext(); ) {
            Local var = iterLocals.next();
            namer.getName(var);
            Object b = canvas.drawNode(namer.getName(var));
            varNodes.add(b);
        }

        for (Iterator nodesIt = graph.iterator(); nodesIt.hasNext(); ) {
            Object node = nodesIt.next();
            canvas.drawNode(namer.getName(node));

            try {
                for (Iterator succsIt = sortedIterator(graph.getSuccsOf((Unit) node), comparator);
                     succsIt.hasNext(); ) {
                    Object succ = succsIt.next();
                    canvas.drawEdge(namer.getName(node), namer.getName(succ));
                }
            } catch (RuntimeException e) {
                // TODO better deal with nodes that are not in the pred or successor list
            }

            Collection<Local> defVars = graph.getDefVars((Unit) node);
            if (null != defVars) {
                for (Local succ : defVars) {
                    Object e = canvas.drawEdge(namer.getName(node), namer.getName(succ));
                    defEdges.add(e);
                }
            }
        }

        for (Iterator<Local> iterLocals = graph.localsIter(); iterLocals.hasNext(); ) {
            Local v = iterLocals.next();
            for (Unit u : graph.getUseUnits(v)) {
                Object e = canvas.drawEdge(namer.getName(v), namer.getName(u));
                useEdges.add(e);
            }
        }

        setStyle(graph.getHeads(), canvas, namer,
                DotGraphConstants.NODE_STYLE_FILLED, headAttr);
        setStyle(graph.getTails(), canvas, namer,
                DotGraphConstants.NODE_STYLE_FILLED, tailAttr);

        for (Object b : varNodes) {
            ((DotGraphNode) b).setStyle(DotGraphConstants.NODE_STYLE_DASHED);
            ((DotGraphNode) b).setAttribute("shape", "ellipse");
        }
        for (Object b : useEdges) {
            ((DotGraphEdge) b).setAttribute("color","red");
        }
        for (Object b : defEdges) {
            ((DotGraphEdge) b).setAttribute("color","blue");
        }

        if (! isBrief) {
            formatNodeText(body, canvas, namer);
        }

        return canvas;
    }
}
