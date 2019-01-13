import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import soot.Unit;
import soot.toolkits.graph.DirectedGraph;
import soot.toolkits.graph.UnitGraph;

public class DataDependencyGraph implements DirectedGraph<Unit> {

    protected UnitGraph srcGraph;

    /* Nodes of the graph */
    protected List<Unit> graphNodes;
    protected List<Unit> heads; /* graph heads */
    protected List<Unit> tails; /* graph tails */
    protected Map<Unit, List<Unit>> preds;
    protected Map<Unit, List<Unit>> succ;
    protected ReachingDefinitions reachingDefinitions;

    /**
     * @return the reachingDefinitions
     */
    public ReachingDefinitions getReachingDefinitions() {
        // BAD practice, the internal implementation of the dependency
        // graph should be hidden
        return reachingDefinitions;
    }

    /**
     * Generate a data dependency graph from a CFG
     *
     * @param graph
     */
    public DataDependencyGraph(UnitGraph graph) {
        this.srcGraph = graph;

        graphNodes = new ArrayList<Unit>();
        heads = new ArrayList<Unit>();
        tails = new ArrayList<Unit>();
        preds = new HashMap<Unit, List<Unit>>();
        succ = new HashMap<Unit, List<Unit>>();

        reachingDefinitions = new ReachingDefinitions(graph);
        buildGraph();
    }

    /**
     * Build the dependency graph of this.srcGraph
     */
    protected void buildGraph()
    {
        /* Compute the dependency graph.
         *
         * Gets all the units and the units that defines a variables that they use.
         * Add an edge between a unit and all the ones that define a variable used there.
         * Iterate the process with the new units unitl a fixpoint is reached.
         */
        for (Unit u : this.srcGraph) {
            Collection<Unit> defsOfUnit = getDefsOf(u);
            List<Unit> predList = getPredsOf(u);

            graphNodes.add(u);

            for (Unit reachedUnits : defsOfUnit) {
                List<Unit> succList = getSuccsOf(reachedUnits);
                succList.add(u);
                predList.add(reachedUnits);
            }
        }

        /* compute heads and tails */
        for (Unit u : graphNodes) {
            List<Unit> succList = getSuccsOf(u);
            List<Unit> predList = getPredsOf(u);

            if (succList.isEmpty()) tails.add(u);
            if (predList.isEmpty()) heads.add(u);
        }
    }

    private List<Unit> getListFromMap(Map<Unit, List<Unit>> map, Unit u)
    {
        List<Unit> list = map.get(u);
        if (null == list) {
            list = new ArrayList<Unit>();
            map.put(u, list);
        }
        return list;
    }

    /**
     * Returns all the units that define a variable used in srcUnit.
     *
     * @param unit
     * @return
     */
    protected Collection<Unit> getDefsOf(Unit srcUnit) {
        return DataDependencyGraph.getDefsOf(srcUnit, this.reachingDefinitions);
    }

    /**
     * Returns all the units that define a variable used in srcUnit.
     *
     * @param unit
     * @return
     */
    public static Collection<Unit> getDefsOf(Unit srcUnit, ReachingDefinitions rd) {
        Set<Unit> defsOf = new HashSet<Unit>();

        for (Unit u : rd.getReachableAt(srcUnit)) {
            if (rd.unitDefines(srcUnit, u, true)) {
                defsOf.add(u);
            }
        }
        return defsOf;
    }

    @Override
    public List<Unit> getHeads() {
        return heads;
    }

    @Override
    public List<Unit> getPredsOf(Unit u) {
        return getListFromMap(preds, u);
    }

    @Override
    public List<Unit> getSuccsOf(Unit u) {
        return getListFromMap(succ, u);
    }

    @Override
    public List<Unit> getTails() {
        return tails;
    }

    @Override
    public Iterator<Unit> iterator() {
        return graphNodes.iterator();
    }

    @Override
    public int size() {
        return graphNodes.size();
    }
}
