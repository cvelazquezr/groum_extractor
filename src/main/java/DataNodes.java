import org.apache.commons.io.FileUtils;
import soot.Local;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class DataNodes {
    private UnitCdfgGraph cdfgGraph;
    private String className;
    private String methodName;

    public DataNodes(UnitCdfgGraph cdfgGraph, String className, String methodName) {
        this.cdfgGraph = cdfgGraph;
        this.className = className;
        this.methodName = methodName;
    }


    public void makeData() {
        Set<Object> varNodes = new HashSet<Object>();

        for (Iterator<Local> iterLocals = cdfgGraph.localsIter(); iterLocals.hasNext(); ) {
            Local var = iterLocals.next();
            varNodes.add(var.getType().toString());
        }

        // Write the nodes names to a file
        try {
            FileUtils.writeLines(new File("dataNodes/" + className + "/" + methodName + ".txt"),
                        "UTF-8", varNodes);
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}
