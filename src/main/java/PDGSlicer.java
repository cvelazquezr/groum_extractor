import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import soot.Unit;
import soot.toolkits.graph.Block;
import soot.toolkits.graph.UnitGraph;
import soot.toolkits.graph.pdg.HashMutablePDG;
import soot.toolkits.graph.pdg.PDGNode;

/**
 * Extend the HashMutablePDG to provide an inverse link from Unit nodes to PDGNodes.
 *
 * @author Sergio Mover
 *
 */
public class PDGSlicer extends HashMutablePDG {

    protected Map<Unit, Block> unitToBlock;

    public PDGSlicer(UnitGraph cfg) {
        super(cfg);

        buildUnitToBlockMap();
    }

    protected void buildUnitToBlockMap()
    {
        unitToBlock = new HashMap<Unit,Block>();

        for (Iterator<Block> iter = m_blockCFG.iterator(); iter.hasNext(); ) {
            Block b = iter.next();
            for (Iterator<Unit> unitIter = b.iterator(); unitIter.hasNext(); ) {
                Unit u = unitIter.next();

                // Check if this assumption holds
                assert(! unitToBlock.containsKey(u));

                unitToBlock.put(u, b);
            }
        }
    }

    /**
     * Finds the PDGNode that contains the unit u, or u if unit is not in the PDG
     *
     * @param u
     * @return the PDGNode that contains u
     */
    public PDGNode getPDGNodeFromUnit(Unit u)
    {
        Block b = unitToBlock.get(u);
        if (u != null) {
            return getPDGNode(b);
        }
        else {
            return null;
        }
    }

}

