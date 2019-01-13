import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import soot.Body;
import soot.BooleanType;
import soot.PatchingChain;
import soot.RefType;
import soot.util.Chain;
import soot.SootClass;
import soot.SootMethod;
import soot.Trap;
import soot.Unit;
import soot.Value;
import soot.Local;
import soot.ValueBox;
import soot.dava.internal.javaRep.DIntConstant;
import soot.jimple.CaughtExceptionRef;
import soot.jimple.GotoStmt;
import soot.jimple.IdentityStmt;
import soot.jimple.IfStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.Jimple;
import soot.jimple.JimpleBody;
import soot.jimple.LookupSwitchStmt;
import soot.jimple.NopStmt;
import soot.jimple.SwitchStmt;
import soot.jimple.TableSwitchStmt;
import soot.jimple.internal.JTrap;
import soot.toolkits.graph.Block;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.toolkits.graph.UnitGraph;
import soot.toolkits.graph.pdg.ConditionalPDGNode;
import soot.toolkits.graph.pdg.EnhancedUnitGraph;
import soot.toolkits.graph.pdg.LoopedPDGNode;
import soot.toolkits.graph.pdg.PDGNode;
import soot.toolkits.exceptions.ThrowAnalysis;
import soot.toolkits.exceptions.ThrowAnalysisFactory;
import soot.toolkits.exceptions.ThrowableSet;

/**
 * Slice a CFG of a method using as seeds the nodes of the API calls.
 *
 * @author Sergio Mover
 *
 */

public class APISlicer {
    private UnitGraph cfg;
    private PDGSlicer pdg;
    private DataDependencyGraph ddg;
    private ThrowAnalysis throwAnalysis;
    /* map from trap to set of units */
    private Map<Trap, Set<Unit>> trapToUnit;
    /* map from caught unit to trap */
    private Map<Unit, Trap> caughtToTrap;

    public static boolean PRINT_DEBUG_GRAPHS = false;
    private Logger logger = LoggerFactory.getLogger(APISlicer.class);
    /**
     * @return the cfg
     */
    public UnitGraph getCfg() {
        return cfg;
    }

    /**
     * @return the pdg
     */
    public PDGSlicer getPdg() {
        return pdg;
    }

    /**
     * @return the ddg
     */
    public DataDependencyGraph getDdg() {
        return ddg;
    }

    public APISlicer(UnitGraph cfg, Body body) {
        super();

        if (null == cfg) {
            cfg= new EnhancedUnitGraph(body);
        }
        this.cfg = cfg;

        /* Computes the program dependency graph */
        logger.info("Creating PDG... (size = " + this.cfg.size() + ")" );
        this.pdg= new PDGSlicer((UnitGraph) this.cfg);
        logger.info("Creating DDG...");
        this.ddg = new DataDependencyGraph(this.cfg);
        logger.info("Processing traps...");
        this.throwAnalysis = ThrowAnalysisFactory.checkInitThrowAnalysis();
        buildTrapMaps();
    }

    public APISlicer(Body body) {
        this(null, body);
    }

    private void buildTrapMaps() {
        Chain<Unit> units = this.cfg.getBody().getUnits();
        trapToUnit = new HashMap<Trap, Set<Unit>>();
        caughtToTrap = new HashMap<Unit, Trap>();

        for (Trap trap : this.cfg.getBody().getTraps()) {
            HashSet<Unit> unitSet = new HashSet<Unit>();
            RefType catcher = trap.getException().getType();
            Unit lastUnitInTrap = units.getPredOf(trap.getEndUnit());
            Iterator<Unit> unitIt = units.iterator(trap.getBeginUnit(),
                    lastUnitInTrap);
            Unit handler = trap.getHandlerUnit();
            caughtToTrap.put(handler, trap);
            trapToUnit.put(trap, unitSet);

            while (unitIt.hasNext()) {
                Unit unit = unitIt.next();
                ThrowableSet thrownSet = throwAnalysis.mightThrow(unit);

                ThrowableSet.Pair catchableAs = thrownSet.whichCatchableAs(catcher);
                if (! catchableAs.getCaught().equals(ThrowableSet.Manager.v().EMPTY)) {
                    unitSet.add(unit);
                }
            }
        }
    }

    /**
     * Computes the groum.graphs.slice of this.cfg according to the android API method calls.
     *
     * @return a groum.graphs.slice for the sc criterion or null if there are no seeds.
     */
    public Body slice(SliceCriteria sc) {
        /* 2.1 Find the groum.graphs.slice units */
        Set<Unit> seeds = getSeeds(sc);

        if (seeds.size() > 0) {
            /* 2.2 Get the CFG units that are relevant for the groum.graphs.slice */
            Set<Unit> unitsInSlice = findReachableUnits(seeds);

            if (PRINT_DEBUG_GRAPHS)
            {
                ExceptionalUnitGraph c = new ExceptionalUnitGraph(this.cfg.getBody());
                SootHelper.dumpToDot(c, this.cfg.getBody(), "/tmp/eug.dot");

                SootHelper.dumpToDot(this.cfg, this.cfg.getBody(), "/tmp/cfg.dot");
                SootHelper.dumpToDot(ddg, this.cfg.getBody(), "/tmp/ddg.dot");
                SootHelper.dumpToDot(pdg, this.cfg.getBody(), "/tmp/pdg.dot");
            }

            /* Prune the method call in the groum.graphs.slice that are not from the seed
             * This is a workaround to remove from the groum.graphs.slice the methods that
             * do not belong to the android APIs, even if they are in the groum.graphs.slice.
             *
             * */
            Set<Unit> newUnitsInSlice = new HashSet<Unit>();
            for (Unit u : unitsInSlice) {
                boolean toAdd = true;

                for (ValueBox valBox : u.getUseBoxes()) {
                    Value v = valBox.getValue();
                    if (v instanceof InvokeExpr &&
                            (! sc.is_seed(u))) {
                        toAdd = false;
                        break;
                    }
                }
                if (toAdd) newUnitsInSlice.add(u);
            }

            /* 3. Construct the sliced body */
            SlicerGraph sg = new SlicerGraph(this.cfg, newUnitsInSlice);
            Body slice = sg.getSlicedBody(caughtToTrap);

      /* Creates the unit graph

         This simplifies the body and removes duplicates that are not useful.

         This has to be investigated further.
       */
            EnhancedUnitGraph slicedGraph = new EnhancedUnitGraph(slice);
            if (PRINT_DEBUG_GRAPHS) {
                SootHelper.dumpToDot(slicedGraph, slice, "/tmp/sliced.dot");
            }

            return slice;
        }
        else {
            return null;
        }
    }

    public static String getSlicedMethodName(String methodName) {
        return methodName + "__sliced__";
    }

    /**
     * Finds all the seeds (units) in the PDG according to the slicing criterion
     *
     * The method returns a pair (PDGNode,
     *
     * @return
     */
    private Set<Unit> getSeeds(SliceCriteria sc)
    {
        Set<Unit> seeds = new HashSet<>();
        for (Object node : pdg.getNodes()) {
            if (node instanceof PDGNode) {
                Object innerNode = ((PDGNode) node).getNode();
                if (innerNode instanceof Block) {
                    Block b = (Block) innerNode;
                    for (Iterator<Unit> iter = b.iterator(); iter.hasNext();) {
                        Unit u = iter.next();
                        if (sc.is_seed(u)) seeds.add(u);
                    }
                }
            }
        }

        return seeds;
    }

    /**
     * Find all the units that are <b>backward</b> reachable from the units
     * contained in seeds in the graph represented by the combination
     * of the PDG and the DDG.
     *
     * The combination of the PDG and DDG graph is trivial: the graph is formed
     * by the PDG and all the edges from unit to unit in the DDG (the PDG
     * already contains the units that may appear as a seed).
     *
     * @param seeds Set of units used as seeds
     * @return the set of backward reachable nodes in the PDG and DDG. graph
     */
    private Set<Unit> findReachableUnits(Set<Unit> seeds)
    {
        /* set of reachable units */
        Set<Unit> reachableUnits = new HashSet<Unit>();
        Set<Unit> reachableConditionals = new HashSet<Unit>();

        /* Set of visited nodes either in the PDG or in the DDG
         * The set may either contain a PDGNode or a Unit */
        Set<Object> visitedNodes = new HashSet<Object>();
        /* Stack used in the search */
        Stack<Object> toProcess = new Stack<Object>();

        /* 1. All the nodes in the seeds must be processed */
        for (Unit seed : seeds) toProcess.push(seed);

        /* 2. Reachability algorithm */
        while (! toProcess.isEmpty()) {
            Object current = toProcess.pop();
            if (visitedNodes.contains(current)) continue; /* skip visited nodes */

            if (current instanceof Unit) {
                Unit currentUnit = (Unit) current;

                if (currentUnit instanceof IfStmt) {
                    reachableConditionals.add(currentUnit);
                    reachableUnits.add(currentUnit);
                }
                else {
                    assert (null != currentUnit);
                    reachableUnits.add(currentUnit);
                }

                /* get the predecessors in the DDG */
                toProcess.addAll(this.ddg.getPredsOf(currentUnit));

                /* get the PDGNode that contains the unit node */
                PDGNode currentPdgNode = this.pdg.getPDGNodeFromUnit(currentUnit);
                assert (null != currentPdgNode);

                /* Add the "gotos" and catch (if they exists) in the cfg block */
                {
                    for (Unit blockLabel : getDependentUnits(currentPdgNode)) {
                        if (null != blockLabel) {
                            reachableUnits.add(blockLabel);

                            if (APISlicer.isCaughtException(blockLabel)) {
                                /* Include all the units in the try that can throw the exception */
                                Trap trap = this.caughtToTrap.get(blockLabel);
                                Set<Unit> unitsInTrap = this.trapToUnit.get(trap);
                                toProcess.addAll(unitsInTrap);
                            }
                        }
                    }
                }

                /* Get the predecessors of the pdg node.
                 * We do not add the unit's pdg node to the search, otherwise we would
                 * possibly end up adding all the units in the same pdg node
                 * (which can be executed in parallel if there are no data dependencies).
                 *
                 * Instead, we add the predecessors of the PDG node.
                 * */
                List<PDGNode> pdgPredecessors = this.pdg.getPredsOf(currentPdgNode);
                toProcess.addAll(pdgPredecessors);
            }
            else if (current instanceof PDGNode) {
                /* get predecessors in the PDG */
                PDGNode currentPdgNode = (PDGNode) current;

                if (currentPdgNode.getType() == PDGNode.Type.CFGNODE) {
                    /* Add *all* the unit in the CFG block in the nodes to be processed
                     * A subset of teh reachable units is control dependent from
                     * this PDGNode
                     *  */
                    Object pdgObject = currentPdgNode.getNode();
                    if (pdgObject instanceof Block &&
                            (currentPdgNode instanceof LoopedPDGNode ||
                                    currentPdgNode instanceof ConditionalPDGNode)) {
                        Block block = (Block) pdgObject;
                        for (Iterator<Unit> iter = (block).iterator(); iter.hasNext();) {
                            // TODO: check if we can get a better groum.graphs.slice considering
                            // dependencies
                            Unit unit = iter.next();
                            toProcess.add(unit);
                        }
                    }
                } else {
                    /* Region, do nothing  */
                }
                List<PDGNode> pdgPredecessors = this.pdg.getPredsOf(currentPdgNode);
                toProcess.addAll(pdgPredecessors);
            }
            else {
                throw new RuntimeException("APISlicer: Unknown type of node!");
            }

            visitedNodes.add(current);
        }
        return reachableUnits;
    }

    private static boolean isCaughtException(Unit u) {
        boolean isCaughtException = false;
        if (u instanceof IdentityStmt) {
            IdentityStmt identity = (IdentityStmt) u;
            isCaughtException = identity.getRightOp() instanceof CaughtExceptionRef;
        }
        return isCaughtException;
    }

    /**
     * Given a pdg node, returns the goto instruction, conditions and
     * catch expressions in the block.
     *
     * @param pdgNode a node in the pdg
     * @return the "dependent" units
     */
    private List<Unit> getDependentUnits(PDGNode pdgNode)
    {
        List<Unit> unitList = new ArrayList<Unit>();

        assert(pdgNode.getType() == PDGNode.Type.CFGNODE);
        Object pdgObject = pdgNode.getNode();

        if (pdgObject instanceof Block) {
            Block block = (Block) pdgObject;
            Unit appBlock = null;
            for (Iterator<Unit> iter = (block).iterator(); iter.hasNext();) {
                appBlock = iter.next();

                if (isCaughtException(appBlock) ||
                        appBlock instanceof GotoStmt || appBlock instanceof IfStmt) {
                    /* Assume at most one goto statement in the block */
                    unitList.add(appBlock);
                }
            }
        }

        return unitList;
    }


    /**
     * Class used to build the sliced body given the a set of relevant
     * units.
     *
     * The class is responsible for creating the correct groum.graphs.slice, taking into
     * account the transitive edges in the graph and building
     * a well formed body.
     *
     * @author Sergio Mover
     *
     */
    private class SlicerGraph {
        Body srcBody;
        /* Adjacency matrix, contains true if the edge i-j exists */
        boolean[][] edges;
        /* Labels of the edges */
        List<Object>[][] edgeLabels;
        /* unitsInSlice[i] is true if the i-th unit is in the groum.graphs.slice */
        boolean[]  unitsInSlice;
        /* idToUnit[i] is the i-th unit */
        Unit[] idToUnit;
        /* Map from units to ids */
        Map<Unit,Integer> unitToId;


        /**
         * Creates the SlicerGraph from a unit graph and a set of
         * units that must be kept in the groum.graphs.slice.
         *
         * @param g a unit graph
         * @param unitsInSlice units of g that must be kept in the groum.graphs.slice.
         */
        @SuppressWarnings({ "unchecked", "rawtypes" })
        public SlicerGraph(UnitGraph g, Set<Unit> unitsInSlice) {
            srcBody = g.getBody();
            int size = srcBody.getUnits().size();
            edges = new boolean[size][size];
            edgeLabels = new List[size][size];
            this.unitsInSlice = new boolean[size];
            idToUnit = new Unit[size];
            unitToId = new HashMap<Unit, Integer>();

            /* Initializes the nodes */
            int id = 0;
            for (Unit u : g.getBody().getUnits()) {
                idToUnit[id] = u;
                this.unitsInSlice[id] = unitsInSlice.contains(u);
                unitToId.put(u, new Integer(id));

                for (int j = 0; j < edges.length; j++) {
                    edges[id][j] = false;
                    edgeLabels[id][j] = new ArrayList();
                }

                id = id + 1;
            }

            /* Fill the edges matrix */
            for (Unit u : g.getBody().getUnits()) {
                List<Unit> successors = g.getSuccsOf(u);
                id = unitToId.get(u).intValue();

                LabelHandler handler = new LabelHandler(u, g, successors);

                for (Unit succ : successors) {
                    int dstId = unitToId.get(succ).intValue();
                    edges[id][dstId] = true;

                    List<Object> conditions = handler.getConditions(idToUnit[dstId]);
                    if (null != conditions)
                        edgeLabels[id][dstId].addAll(conditions);
                    else
                        edgeLabels[id][dstId].add(LabelHandler.EMPTY_LABEL);
                }
            }

            if (PRINT_DEBUG_GRAPHS)
                printGraph("/tmp/pre_slice.dot");

            /* computes the transitive edges across the nodes not in the groum.graphs.slice */
            transitiveClosure();

            /* remove all the edges (and labels) not in the groum.graphs.slice */
            for (int i = 0; i < edges.length; i++) {
                for (int j = 0; j < edges.length; j++) {
                    if (! isEdgeInSlice(i,j)) {
                        edges[i][j] = false;
                        edgeLabels[i][j] = null;
                    }
                }
            }

            if (PRINT_DEBUG_GRAPHS)
                printGraph("/tmp/post_slice.dot");
        }

        private void printGraph(String fname) {
            try {
                BufferedWriter writer = new BufferedWriter(new FileWriter(fname));
                writer.write("digraph G {\n");
                for (int i = 0; i < edges.length; i++) {
                    String s = idToUnit[i].toString();
                    s = s.replace('"', ' ');
                    if (unitsInSlice[i]) {
                        writer.write("n" + i + " [label = \"" + i + " " + s + "\", color=red];\n") ;
                    }
                    else {
                        writer.write("n" + i + " [label = \"" + i + " " + s + "\"];\n") ;
                    }
                }

                for (int i = 0; i < edges.length; i++) {
                    for (int j = 0; j < edges.length; j++) {
                        if (edges[i][j]) {
                            writer.write("n" + i + " -> " + "n" + j + "[ label = \"(" +
                                    i + "," + j + ") " +
                                    this.edgeLabels[i][j] + "\" ];\n") ;
                        }
                    }
                }
                writer.write("}\n");
                writer.close();
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        private boolean isEdgeInSlice(int i, int j)
        {
            return this.unitsInSlice[i] && this.unitsInSlice[j];
        }

        private boolean hasEdges(int srcUnitId) {
            boolean hasEdges = false;
            for (int j = 0; j < edges.length && ! hasEdges; j++)
                hasEdges = hasEdges || (edges[srcUnitId][j]);
            return hasEdges;
        }

        /**
         * Compute the closure of the transitions among the node not in the groum.graphs.slice.
         *
         * The transitive closure is computed only among nodes that are <bf>not</bf>
         * in the groum.graphs.slice.
         *
         * The idea is to compute the control flow from a unit in the groum.graphs.slice to another
         * unit in the groum.graphs.slice that flows through units that are not in the groum.graphs.slice.
         *
         * The procedure uses a modified version of Floyd-Warshall, which keeps into
         * account  the in groum.graphs.slice/not in groum.graphs.slice property and the labels on the edges.
         *
         * The method changes the internal data structures of the graph.
         */
        private void transitiveClosure() {
            for (int k = 0; k < edges.length; k++) {
                /* Skip all the executions where k (the intermediate node) is in the
                 * groum.graphs.slice.
                 *
                 * In this case we do not want to create a transitive edge.
                 * */
                if (this.unitsInSlice[k]) continue;
                for (int j = 0; j < edges.length; j++) {
                    for (int i = 0; i < edges.length; i++) {
                        if (edges[i][k] && edges[k][j]) {
                            edges[i][j] = true;

                            if (this.unitsInSlice[i]) {
                                /* set the label for the edge */
                                assert edgeLabels[i][j] != null;
                                edgeLabels[i][j].addAll(edgeLabels[i][k]);
                            }
                        }
                    }
                }
            }
        }

        /**
         * Creates an empty body from an existing body, associating it
         * to a "sliced" method in the class.
         *
         * @param srcBody the non-sliced body
         * @return an empty body
         */
        private Body getEmptyDstBody(Body srcBody)
        {
            SootMethod srcMethod = srcBody.getMethod();
            SootClass srcClass = srcMethod.getDeclaringClass();
            assert (null != srcMethod);
            String methodName = srcMethod.getName();
            methodName = getSlicedMethodName(methodName);

            SootMethod dstMethod = new SootMethod(methodName,
                    srcMethod.getParameterTypes(),
                    srcMethod.getReturnType(),
                    srcMethod.getModifiers());
            JimpleBody dstBody = Jimple.v().newBody(dstMethod);
            dstMethod.setActiveBody(dstBody);
            srcClass.addMethod(dstMethod);

            return dstBody;
        }

        /**
         * Get the status (non visited, visited in pre-order, visited in
         * post-order) of the unit u
         *
         * @param statusMap map that keeps the visited status
         * @param u unit to get the status
         * @return the status of u in the search
         */
        private int getStatus(Map<Unit, Integer> statusMap, Unit u) {
            int status = 0;
            Integer statusInt = statusMap.get(u);
            if (null != statusInt) status = statusInt.intValue();
            return status;
        }


        /* Returns the heads of the graph that are also in the groum.graphs.slice.
         *
         * @return a list of ids of the units that are in the groum.graphs.slice and are an head.
         */
        private List<Integer> getHeadsInSlice() {
            List<Integer> heads = new ArrayList<Integer>();
            for (int i = 0; i < edges.length; i++) {
                if (unitsInSlice[i]) {
                    boolean isHead = true;
                    for (int j = 0; (isHead && j < edges.length); j++)
                        isHead = isHead && ! edges[j][i];
                    if (isHead) heads.add(i);
                }
            }
            return heads;
        }

        /**
         * Builds the sliced body
         *
         * @return the sliced body
         */
        public Body getSlicedBody(Map<Unit, Trap> caughtToTrap)
        {
            Stack<Integer> toVisit = new Stack<Integer>();
            Map<Unit, Integer> statusMap = new HashMap<Unit, Integer>();
            Unit[] idToDstUnit = new Unit[idToUnit.length];
            List<Integer> handlerList = new ArrayList<Integer>();
            HashMap<Object, Object> bindings = new HashMap<Object, Object>();

            Body dstBody = getEmptyDstBody(srcBody);
            PatchingChain<Unit> dstChain = dstBody.getUnits();

            Unit dstFirst = null;
            Unit dstLast = null;

            /* Visit the graph (after the slicing we possibly have a forest. */
            List<Integer> heads = getHeadsInSlice();
            for (Integer currentHeadId : heads) {
                if (null == dstLast) {
                    dstFirst = Jimple.v().newNopStmt();
                    dstChain.addFirst(dstFirst);
                    dstLast = Jimple.v().newNopStmt();
                    dstChain.addLast(dstLast);
                }
                else {
          /*
            - create a new head and a new end
            - connect the old head with the new head with an if
            - connect the new tail with the old tail

           */
                    Unit newFirst = Jimple.v().newNopStmt();
                    Unit newLast = Jimple.v().newGotoStmt(dstLast);
                    Unit toNewFirst = Jimple.v().newIfStmt(DIntConstant.v(1, BooleanType.v()),
                            newFirst);
                    Unit toNewLast = Jimple.v().newIfStmt(DIntConstant.v(1, BooleanType.v()),
                            newLast);

                    dstChain.insertAfter(newFirst, dstLast);
                    dstChain.insertAfter(newLast, newFirst);
                    dstChain.insertAfter(toNewFirst, dstFirst);
                    dstChain.insertAfter(toNewLast, dstLast);

                    dstFirst = newFirst;
                    dstLast = newLast;
                }

                toVisit.push(currentHeadId);
                visitHead(toVisit, bindings, dstFirst, dstLast, statusMap,
                        idToDstUnit, dstChain, handlerList);
            }

            {
                {
                    // Clone local units.
                    Chain<Local> dstLocalChain = dstBody.getLocals();
                    for (soot.Local srcLocal : srcBody.getLocals()) {
                        soot.Local dstLocal = (soot.Local) srcLocal.clone();

                        // Add cloned unit to our trap list.
                        dstLocalChain.addLast(dstLocal);

                        // Build old <-> new mapping.
                        bindings.put(srcLocal, dstLocal);
                    }
                }

                /* Process exceptions */
                Chain<Trap> dstTrapsChain = dstBody.getTraps();
                for (Integer handlerId : handlerList) {
                    Unit dstHandler = idToDstUnit[handlerId];
                    Unit handler = idToUnit[handlerId];
                    Trap trap = caughtToTrap.get(handler);

                    if (trap == null) {
                        /* The trap can be null here.
                         * Consider the example of the Bug051.
                         *
                         * The handler node that we insert in the list can *not* belong to
                         * a trap at all, by transitivity.
                         *
                         * If it is the case, it means that there are no nodes in the
                         * trap and are also in the groum.graphs.slice. Hence, we can disregard the trap.
                         *
                         */
                        continue;
                    }
                    assert (trap != null);
                    Unit trapEndUnit = trap.getEndUnit();
                    assert (null != trapEndUnit);

                    Unit lastToVisit = srcBody.getUnits().getPredOf(trapEndUnit);
                    Iterator<Unit> unitIt = srcBody.getUnits().iterator(trap.getBeginUnit(),
                            lastToVisit);

                    int firstUnitInTrap = -1;
                    int lastUnitInTrap = -1;
                    while (unitIt.hasNext()) {
                        Unit u = unitIt.next();
                        int unitId = this.unitToId.get(u);
                        if (this.unitsInSlice[unitId]) {
                            if (firstUnitInTrap < 0) firstUnitInTrap = unitId;
                            lastUnitInTrap = unitId;
                        }
                    }
                    if (firstUnitInTrap >= 0 && lastUnitInTrap >= 0) {
                        Unit dstFirstUnitInTrap = idToDstUnit[firstUnitInTrap];
                        Unit dstLastUnitInTrap = idToDstUnit[firstUnitInTrap];
                        dstLastUnitInTrap = dstChain.getSuccOf(dstLastUnitInTrap);

                        Trap dstTrap = new JTrap(trap.getException(),
                                dstFirstUnitInTrap, dstLastUnitInTrap,
                                dstHandler);
                        dstTrapsChain.addLast(dstTrap);
                        bindings.put(trap, dstTrap);
                    }
                    else {
                        logger.warn("Skipping trap...");
                    }

                }

                {
                    // backpatching all local variables.
                    for (ValueBox vb : dstBody.getUseBoxes()) {
                        if(vb.getValue() instanceof Local)
                            vb.setValue((Value) bindings.get(vb.getValue()));
                    }
                    for (ValueBox vb : dstBody.getDefBoxes()) {
                        if(vb.getValue() instanceof Local)
                            vb.setValue((Value) bindings.get(vb.getValue()));
                    }
                }
            }
            return dstBody;
        }

        private void visitHead(Stack<Integer> toVisit,
                               HashMap<Object, Object> bindings,
                               Unit dstFirst, Unit dstLast,
                               Map<Unit, Integer> statusMap,
                               Unit[] idToDstUnit,
                               PatchingChain<Unit> dstChain,
                               List<Integer> handlerList)
        {
            while (! toVisit.isEmpty()) {
                int srcUnitId = toVisit.pop().intValue();
                Unit srcUnit = this.idToUnit[srcUnitId];
                Unit dstUnit = null;
                int status = getStatus(statusMap, srcUnit);

                switch (status) {
                    case 0:
          /* Never visited node:
             - Copy the node
             - Add the binding
             - Add node to the dst patching chain
             - Schedule the visit of all the children
           */
                        dstUnit = (Unit) srcUnit.clone();
                        idToDstUnit[srcUnitId] = dstUnit;
                        dstUnit.addAllTagsOf(srcUnit);
                        bindings.put(srcUnit, dstUnit);
                        dstChain.insertBeforeNoRedirect(dstUnit, dstLast);

          /* Schedule the visit of all the children.
             Here we insert as LAST element the next element in the patching
             chain, while before we insert all the branches.
          */
                        List<Integer> successors = new ArrayList<Integer>();
                        boolean hasEmpty = false;
                        for (int j = 0; j < edges[srcUnitId].length; j++) {
                            if (! unitsInSlice[j]) continue;

                            if (edges[srcUnitId][j] && !statusMap.containsKey(new Integer(j))) {
                                int succStatus = getStatus(statusMap, idToUnit[j]);
                                if (this.edgeLabels[srcUnitId][j].contains(LabelHandler.EMPTY_LABEL)) {
                                    /* the empty label should be on just one edge */
                                    assert ! hasEmpty;
                                    hasEmpty = true;

                                    if (succStatus == 0) {
                                        successors.add(new Integer(j));
                                    }
                                    else {
                                        /* already added. Add a goto */
                                        dstChain.insertAfter(Jimple.v().newGotoStmt(idToDstUnit[j]),dstUnit);
                                    }
                                }
                                else {
                                    /* visit it later to change the jumps */
                                    if (succStatus == 0) {
                                        successors.add(0, new Integer(j));
                                    }
                                    else {
                                        /* Do nothing, already visited */
                                    }
                                }
                            }
                        }
                        if (! hasEmpty && hasEdges(srcUnitId)) {
                            /* add a jump to the last unit */
                            dstChain.insertAfter(Jimple.v().newGotoStmt(dstLast),dstUnit);
                        }

                        /* add elements to the stack.
                         * Ensure to visit all the children before getting back to
                         * srcUnit
                         */
                        toVisit.push(srcUnitId);
                        for (Integer i : successors) {
                            if (i != srcUnitId) {
                                /* Avoid to visit the current node before its other children
                                 * Otherwise, some children can be unexplored, leading to
                                 * errors.
                                 * */
                                toVisit.push(i);
                            }
                        }

                        statusMap.put(srcUnit, 1);

                        break;
                    case 1:
          /* Already visited once in preorder.

             - Adjust the gotos to the children.
             The children have been created, we just need to fix the gotos.
          */
                        dstUnit = idToDstUnit[srcUnitId];

                        /* redirect gotos */
                        LabelHandler labelHandler = new LabelHandler(dstUnit, null, null);
                        Map<Object, List<Unit>> c2t = getConditions2Targets(srcUnitId, idToDstUnit);
                        List<Unit> handlers = labelHandler.fixGotos(c2t, dstLast);
                        for (Unit dstHandler : handlers) {
                            int i = 0;
                            for (; i < idToDstUnit.length; i++) {
                                if (idToDstUnit[i] == dstHandler) {
                                    break;
                                }
                            }

                            handlerList.add(i);
                        }

                        /* redirect to final node */
                        if (! hasEdges(srcUnitId)) {
                            dstChain.insertAfter(Jimple.v().newGotoStmt(dstLast), dstUnit);
                        }

                        /* Visited in post-order */
                        statusMap.put(srcUnit, 2);

                        break;
                    case 2:
                        /* already visited in postorder - do nothing */
                        break;
                    default:
                        assert false;
                }
            }
        }

        /**
         * Given the id of a unit (and the map to get its unit) builds a map
         * from a condition (the label of an edge) to a set of targets.
         *
         *
         *
         * @param srcUnitId
         * @param idToDstUnit
         * @return
         */
        private Map<Object, List<Unit>> getConditions2Targets(int srcUnitId,
                                                              Unit[] idToDstUnit)
        {
            Map<Object, List<Unit>> c2t = new HashMap<Object, List<Unit>>();

            for (int j = 0; j < edges[srcUnitId].length; j++) {
                if (! this.edges[srcUnitId][j]) continue;
                boolean edgeInSlice = isEdgeInSlice(srcUnitId, j);
                if (edgeInSlice) {
                    boolean edgeHasJumpLabel = (! this.edgeLabels[srcUnitId][j].contains(LabelHandler.EMPTY_LABEL)) ||
                            (this.edgeLabels[srcUnitId][j].size() > 1);
                    if (edgeInSlice && edgeHasJumpLabel) {
                        for (Object condition : this.edgeLabels[srcUnitId][j]) {
                            if (condition == LabelHandler.EMPTY_LABEL) continue;
                            List<Unit> targets = c2t.get(condition);
                            if (null == targets) {
                                targets = new ArrayList<Unit>();
                                c2t.put(condition, targets);
                            }
                            assert idToDstUnit[j] != null;
                            targets.add(idToDstUnit[j]);
                        }
                    }
                }
            }

            return c2t;
        }

        private class LabelHandler {
            UnitGraph g;
            Unit sourceUnit;
            List<Unit> successors;
            private Map<Unit,List<Object>> target2conditions;
            private Map<Object,List<Unit>> condition2targets;

            private final static String DEFAULT_LABEL = "default_label";
            private final static String EMPTY_LABEL = "empty_label";
            private final static String CATCH_LABEL = "catch_label";

            public LabelHandler(Unit sourceUnit,
                                UnitGraph g,
                                List<Unit> successors)
            {
                this.sourceUnit = sourceUnit;
                this.successors = successors;
                this.g = g;
                buildMaps();
            }

            public List<Object> getConditions(Unit target) {
                List<Object> conditions = this.target2conditions.get(target);
                return conditions;
            }

            private void setTargetSwitch(Map<Object, List<Unit>> c2t,
                                         Unit defaultTarget,
                                         SwitchStmt switchStmt,
                                         Object condition,
                                         int targetIndex) {

                Unit target = defaultTarget;

                List<Unit> targets = null;
                if (c2t.containsKey(condition)) {
                    targets = c2t.get(condition);
                }

                if (targets != null && targets.size() > 1) {
                    assert targets.size() > 0;
                    target = targets.get(0);
                    targets.remove(0); /* consume the target */
                }
                switchStmt.setTarget(targetIndex, target);
            }

            public List<Unit> fixGotos(Map<Object, List<Unit>> c2t,
                                       Unit defaultTarget) {
                List<Unit> handlerList = new ArrayList<Unit>();

                if (sourceUnit instanceof NopStmt) {
                    List<Unit> targets = c2t.get(CATCH_LABEL);
                    if (null != targets) {
                        for (Unit target : targets) {
                            assert (null != target);
                            handlerList.add(target);
                        }
                    }
                } else if (sourceUnit instanceof GotoStmt) {
                    GotoStmt gotoStmt = (GotoStmt) sourceUnit;
                    Unit target = defaultTarget;

                    // assert(c2t.containsKey(DEFAULT_LABEL));
                    List<Unit> targets = c2t.get(DEFAULT_LABEL);
                    if (null != targets) {
                        // assert targets.size() == 1;
                        target = targets.get(0);
                    }
                    assert (null != target);
                    gotoStmt.setTarget(target);
                } else if (sourceUnit instanceof IfStmt) {
                    IfStmt ifstmt = (IfStmt) sourceUnit;
                    Unit target = defaultTarget;

                    // assert(c2t.containsKey(DEFAULT_LABEL));
                    List<Unit> targets = c2t.get(DEFAULT_LABEL);
                    if (targets != null) {
                        // assert targets.size() == 1;
                        target = targets.get(0);
                    }
                    assert (null != target);
                    ifstmt.setTarget(target);
                }
                else if (sourceUnit instanceof LookupSwitchStmt) {
                    LookupSwitchStmt switchStmt = (LookupSwitchStmt) sourceUnit;

                    for (int i = 0; i < switchStmt.getTargets().size(); i ++) {
                        Object condition = switchStmt.getLookupValue(i);
                        setTargetSwitch(c2t, defaultTarget, switchStmt, condition, i);
                    }
                    setDefaultTargetSwitch(c2t, defaultTarget, switchStmt);
                }
                else if (sourceUnit instanceof TableSwitchStmt) {
                    TableSwitchStmt switchStmt = (TableSwitchStmt) sourceUnit;

                    for (int i = switchStmt.getLowIndex(),
                         j = 0;
                         i <= switchStmt.getHighIndex();
                         i++, j++) {
                        Object condition = new Integer(i);
                        setTargetSwitch(c2t, defaultTarget, switchStmt, condition, j);
                    }
                    setDefaultTargetSwitch(c2t, defaultTarget, switchStmt);
                }
                else {
                    assert(c2t.size() == 0);
                }
                return handlerList;
            }

            private void buildMaps()
            {
                if (successors == null || g == null) return;
                target2conditions = new HashMap<Unit,List<Object>>();
                condition2targets = new HashMap<Object,List<Unit>>();

                if (sourceUnit instanceof NopStmt) {
                    if (null != successors) {
                        for (Unit succ : successors) {
                            if (APISlicer.isCaughtException(succ)) {
                                /* use the target as label */
                                addToMapO(condition2targets, CATCH_LABEL, succ);
                                addToMapU(target2conditions, succ, CATCH_LABEL);
                            }
                        }
                    }
                }
                else if (sourceUnit instanceof GotoStmt) {
                    GotoStmt gotoStmt = (GotoStmt) sourceUnit;
                    Unit target = gotoStmt.getTarget();
                    target = this.getDefaultTarget(target);

                    /* use the target as label */
                    addToMapO(condition2targets, DEFAULT_LABEL, target);
                    addToMapU(target2conditions, target, DEFAULT_LABEL);
                }
                else if (sourceUnit instanceof IfStmt) {
                    IfStmt ifstmt = (IfStmt) sourceUnit;
                    Unit target = ifstmt.getTarget();
                    target = this.getDefaultTarget(target);

                    addToMapO(condition2targets, DEFAULT_LABEL, target);
                    addToMapU(target2conditions, target, DEFAULT_LABEL);
                }
                else if (sourceUnit instanceof LookupSwitchStmt) {
                    LookupSwitchStmt switchStmt = (LookupSwitchStmt) sourceUnit;

                    int i = 0;
                    for (Unit target : switchStmt.getTargets()) {
                        Object condition = switchStmt.getLookupValue(i);
                        addToMapO(condition2targets, condition, target);
                        addToMapU(target2conditions, target, condition);

                        i = i + 1;
                    }

                    Unit defaultTarget = switchStmt.getDefaultTarget();
                    if (defaultTarget != null) {
                        addToMapO(condition2targets, DEFAULT_LABEL, defaultTarget);
                        addToMapU(target2conditions, defaultTarget, DEFAULT_LABEL);
                    }
                }
                else if (sourceUnit instanceof TableSwitchStmt) {
                    TableSwitchStmt switchStmt = (TableSwitchStmt) sourceUnit;

                    List<? extends Unit> targets = switchStmt.getTargets();
                    for (int i = switchStmt.getLowIndex(),
                         j = 0;
                         i <= switchStmt.getHighIndex();
                         i++, j++) {
                        Unit target = targets.get(j);
                        Object condition = new Integer(i);
                        addToMapO(condition2targets, condition, target);
                        addToMapU(target2conditions, target, condition);
                    }

                    Unit defaultTarget = switchStmt.getDefaultTarget();
                    if (defaultTarget != null) {
                        addToMapO(condition2targets, DEFAULT_LABEL, defaultTarget);
                        addToMapU(target2conditions, defaultTarget, DEFAULT_LABEL);
                    }
                }
            }

            /**
             * Handle the default target in the case of exceptions
             *
             * @param target
             * @return
             */
            private Unit getDefaultTarget(Unit target) {
                if (successors.contains(target)) {
                    return target;
                }
                else {
                    /* search the target up to one level */
                    for (Unit successor : successors) {
                        for(Unit nextSucc : g.getSuccsOf(successor)) {
                            if (nextSucc == target) {
                                return successor;
                            }
                        }
                    }
                    throw new RuntimeException("Default successors not found!");

                }
            }

            private void setDefaultTargetSwitch(Map<Object, List<Unit>> c2t,
                                                Unit defaultTarget, SwitchStmt switchStmt)
            {
                /* add default */
                List<Unit> targets = c2t.get(DEFAULT_LABEL);
                if (targets != null && targets.size() > 0)
                    switchStmt.setDefaultTarget(targets.get(0));
                else switchStmt.setDefaultTarget(defaultTarget);
            }


            private void addToMapU(Map<Unit, List<Object>> map, Unit key, Object element) {
                List<Object> list = map.get(key);
                if (null == list) {
                    list = new ArrayList<Object>();
                    map.put(key, list);
                }
                list.add(element);
            }
            private void addToMapO(Map<Object, List<Unit>> map, Object key,
                                   Unit element) {
                List<Unit> list = map.get(key);
                if (null == list) {
                    list = new ArrayList<Unit>();
                    map.put(key, list);
                }
                list.add(element);
            }
        }
    }

}
