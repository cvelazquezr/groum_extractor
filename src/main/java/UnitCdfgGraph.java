import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import soot.Body;
import soot.Local;
import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class UnitCdfgGraph extends ExceptionalUnitGraph {
    protected List<Local> localsList = null;
    protected List<Unit>  unitsList  = null;
    protected Map<Local, List<Unit>> useEdges = null;
    protected Map<Unit, List<Local>> defEdges = null;
    protected DataDependencyGraph ddg = null;

    public UnitCdfgGraph(Body body) {
        super(body);
        this.ddg = new DataDependencyGraph(this);

        addDataDependentNodes();
        pruneDataDependent();

    /* we require to have at least one tail.

       If there is no tail (it is set only if there are return
       statements, which may not be the case after slicing)

       We have a tail in the graph, we just need to find it.
     */
        if (this.tails.size() == 0) {
            List<Unit> tailList = new ArrayList<Unit>();
            for (Unit u : body.getUnits()) {
                if (getSuccsOf(u).size() > 0) {
                    tailList.add(u);
                }
            }
            tailList = java.util.Collections.unmodifiableList(tailList);
        }

    }

    public Map<Local, List<Unit>> useEdges() {return useEdges;}
    public Map<Unit, List<Local>> defEdges() {return defEdges;}


    public Iterator<Unit> unitIterator() {
        return unitChain.iterator();
    }

    private void addDataDependentNodes()
    {
        /* The construction should happen at most once */
        assert useEdges == null && defEdges == null;
        useEdges = new HashMap<Local, List<Unit>>();
        defEdges = new HashMap<Unit, List<Local>>();

        /* Generate the list of all the locals */
        localsList = new ArrayList<Local>();
        for (Local l : this.getBody().getLocals()) {
            localsList.add(l);
            assert ! useEdges.containsKey(l);
            useEdges.put(l, new ArrayList<Unit>());
        }

        /* Generate the list of the all units */
        unitsList = new ArrayList<Unit>();
        for (Unit u : this.getBody().getUnits()) {
            unitsList.add(u);

            assert ! defEdges.containsKey(u);
            defEdges.put(u, new ArrayList<Local>());
        }

        /* Add the define snf use edges
         *
         * NOTE: now we are not computing the transitive closure
         */
        for (Unit u : unitsList) {
            ReachingDefinitions rd = ddg.getReachingDefinitions();
            List<Local> defsInU = defEdges.get(u);
            if (null == defsInU) {
                defsInU = new ArrayList<Local>();
                defEdges.put(u, defsInU);
            }
            defsInU.addAll(rd.getDefLocals(u, true));

            /* Add the use edges
             * We have a use edge
             *
             * */
            for (Unit pred : ddg.getPredsOf(u)) {
                Collection<Local> defsInPred = defEdges.get(pred);
                if (null != defsInPred) {
                    for (Local l : defsInPred) {
                        useEdges.get(l).add(u);
                    }
                }
            }
        }
    }

    public Iterator<Local> localsIter()
    {
        return localsList.iterator();
    }

    public List<Local> getDefVars(Unit u) {
        List<Local> defsList = defEdges.get(u);
        assert defsList != null;
        return defsList;
    }

    public List<Unit> getUseUnits(Local l) {
        List<Unit> unitList = useEdges.get(l);
        if (null == unitList) return new ArrayList<Unit>();
        else return unitList;
    }

    public void checkGraph() {
        int i = 0;

        for (Iterator<Unit> iter = this.unitIterator();
             iter.hasNext(); iter.next()) {
            i += 1;
        };
        System.out.println("FOUND " + i);
    }

    /**
     * Remove redundancies in the CDFG.
     *
     * Redundancies are locals node that are not connected to anything and
     * multiple use edges from a local to a unit.
     *
     *
     */
    private void pruneDataDependent() {
        /* set of locals that are in the CDFG */
        Set<Local> usedLocals = new HashSet<Local>();

        /* Process all the use edges:
         *  - collects the list of used locals
         *  - removes the duplicate units in the destination list
         * */
        for (Map.Entry<Local, List<Unit>> entry : useEdges.entrySet()) {
            List<Unit> localUseEdges = entry.getValue();
            Local local = entry.getKey();

            if (localUseEdges.size() > 0) {
                usedLocals.add(local);

                /* remove duplicates from localUseEdges */
                Set<Unit> setUnits = new HashSet<Unit>(localUseEdges);
                localUseEdges.clear();
                localUseEdges.addAll(setUnits);
            }
        }

        /* Add the locals used in a def edge */
        for (Map.Entry<Unit, List<Local>> entry : defEdges.entrySet()) {
            List<Local> unitDefaEdges = entry.getValue();
            usedLocals.addAll(unitDefaEdges);
        }

        /* Remove all the use edges that contains locals that have not been
         * in use/def edge
         */
        Set<Local> toRemove = new HashSet<Local>(localsList);
        toRemove.removeAll(usedLocals);
        for (Local l : toRemove) useEdges.remove(l);

        /* just keep the locals that are used in an edge */
        localsList.retainAll(usedLocals);
    }
}
