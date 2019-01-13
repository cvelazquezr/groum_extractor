import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import soot.Body;
import soot.Local;
import soot.PatchingChain;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.AssignStmt;
import soot.jimple.CastExpr;
import soot.toolkits.graph.UnitGraph;

/**
 * Simplifies a Soot body of a method.
 *
 * @author Sergio Mover
 *
 */
public class BodySimplifier {
    private Body body;
    protected DataDependencyGraph ddg = null;
    private Logger logger = LoggerFactory.getLogger(APISlicer.class);
    private Set<String> packageSet = null;

    public BodySimplifier(UnitGraph graph, Collection<String> packages) {
        this.body = graph.getBody();
        this.ddg = new DataDependencyGraph(graph);
        this.packageSet = new HashSet<String>();
        for (String s : packages) {
            this.packageSet.add(s);
        }
    }

    public Body getSimplifiedBody() {
        ReachingDefinitions rd = this.ddg.getReachingDefinitions();
        Map<Unit, Set<Unit>> duChain = rd.getDefinedUnits();

        // inline the equalities - recompute the duChain
        inlineEqualities(this.body, duChain);

        return (Body) body;
    }

    private void inlineEqualities(Body body, Map<Unit, Set<Unit>> duChain) {
        logger.warn("Inlining equalities...");
        boolean fixPoint = false;

        /* get all the assignments */
        Set<Unit> assignments = new HashSet<Unit>();
        for (Unit u : body.getUnits())
            if (isUnitAssignment(u))
                assignments.add(u);

        PatchingChain<Unit> pc = body.getUnits();

        while (! fixPoint) {
            fixPoint = true;
            Set<Unit> newAssignments = new HashSet<Unit>();
            newAssignments.addAll(assignments);

            for (Unit u: assignments) {
                if (isUnitAssignment(u)) {
                    Unit nextUnit = pc.getSuccOf(u);
                    Set<Unit> useUnits = duChain.get(u);

                    if (isUnitAssignment(nextUnit) &&
                            null != useUnits &&
                            1 == useUnits.size() &&
                            useUnits.contains(nextUnit)) {
                        // We can in principle add new cases
                        AssignStmt nextAssignStmt = (AssignStmt) nextUnit;
                        Value nextAssignStmtRhs = nextAssignStmt.getRightOp();
                        boolean isLocal = nextAssignStmtRhs instanceof Local;
                        boolean isCast = false;
                        if (nextAssignStmtRhs instanceof CastExpr) {
                            soot.Type castType = ((CastExpr) nextAssignStmtRhs).getType();

                            // Get rid of all the array types (at some point there should be
                            // a base type
                            while (castType instanceof soot.ArrayType) {
                                castType = ((soot.ArrayType) castType).baseType;
                            }

                            // Look at the ref type.
                            if (castType instanceof soot.RefType) {
                                soot.RefType refType = (soot.RefType) castType;
                                for (String s : this.packageSet) {
                                    if (refType.getClassName().startsWith(s)) {
                                        isCast = true;
                                    }
                                }
                            }
                        } // End of check for casts

                        if (isLocal || isCast) {
                            if (isLocal) {
                                // Remove u and replaces the occurrences of the lhs of u
                                // in nextUnit with the rhs of u
                                replaceValueInUnit(nextUnit,
                                        ((AssignStmt) u).getLeftOp(),
                                        ((AssignStmt) u).getRightOp());
                            } else if (isCast) {
                                // Get rid of the cast and assign the lhs of u to nextAssignStmt
                                // directly
                                nextAssignStmt.setRightOp(((AssignStmt) u).getRightOp());
                            }

                            pc.remove(u);
                            newAssignments.add(nextUnit); /* it should be already there */
                            newAssignments.remove(u);
                            updateDuChain(duChain, u, nextUnit);

                            fixPoint = false;
                        }
                    }
                }
            }
            assignments = newAssignments;
        }
    }

    /**
     *
     * @param u
     * @return true iff u is an assisngment that involves locals
     */
    private boolean isUnitAssignment(Unit u) {
        if (u instanceof AssignStmt) {
            Value lhs = ((AssignStmt) u).getLeftOp();
            return (lhs instanceof Local);
        }
        return false;
    }

    /**
     * When we remove a unit because we performed the inlining we miss the fact
     * that it is USING some variables.
     *
     * We need to re-establish this relationship.
     *
     * Example:
     * x = 1
     * y = x
     * z = y
     *
     * When we remove y = x to obtain:
     * x = 1
     * z = x
     *
     * We miss the fact that x = 1 defines z = 1
     *
     * The function process the DU chain in order to:
     * - Move the uses to z = x (the removed unit) to z = x
     * - Remove the defines for y = x
     *
     * @param duChain
     * @param removedUnit
     * @param changedUnit
     */
    private void updateDuChain(Map<Unit, Set<Unit>> duChain, Unit removedUnit,
                               Unit changedUnit) {

        duChain.remove(removedUnit);

        for (Map.Entry<Unit, Set<Unit>> entry : duChain.entrySet()) {
            Set<Unit> uses = entry.getValue();

            if (null != uses) {
                if (uses.contains(removedUnit)) {
                    uses.remove(removedUnit);
                    uses.add(changedUnit);
                }
            }
        }
    }

    /**
     * Replaces all the instances of oldValue with newValue in the unit u
     */
    public void replaceValueInUnit(Unit u, Value oldValue, Value newValue) {
        for (ValueBox vbToReplace : u.getUseBoxes()) {
            if (vbToReplace.getValue().equals(oldValue))
                vbToReplace.setValue(newValue);
        }
    }
}
