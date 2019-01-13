import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import soot.Hierarchy;
import soot.RefType;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.InvokeExpr;

public class MethodPackageSeed implements SliceCriteria {

    private List<String> packagePrefixes;
    private Map<Unit, Boolean> isInSeed = new HashMap <>();

    private static Map<SootMethod, Boolean> methodOverride = new HashMap<>();

    public MethodPackageSeed(Collection<String> packages) {
        this.packagePrefixes = new LinkedList<>(packages);
        Collections.sort(packagePrefixes);
    }

    public MethodPackageSeed(String packagePrefix) {
        Collection<String> packageList = new LinkedList<String>();
        packageList.add(packagePrefix);

        this.packagePrefixes = new LinkedList<String>(packageList);
    }

    public boolean is_seed(Unit unit) {
        Boolean is_seed = isInSeed.get(unit);
        if (null != is_seed) return is_seed;

        for (ValueBox valBox : unit.getUseBoxes()) {
            Value v = valBox.getValue();
            if (v instanceof InvokeExpr) {
                InvokeExpr expr = (InvokeExpr) v;
                SootMethod method = expr.getMethod();
                SootClass sootClass = method.getDeclaringClass();
                String declaringClassName = sootClass.getName();

                if (isInPackageList(declaringClassName)) {
                    isInSeed.put(unit, true);
                    return true;
                }
                else {
                    /* static method do not override methods, they hide superclass
                     * methods */

                    boolean isSeed = (!method.isStatic()) &&
                            sootClass.hasSuperclass() &&
                            isSeed(method, sootClass.getSuperclass());
                    isInSeed.put(unit, isSeed);
                    return isSeed;

                }
            }
        }
        isInSeed.put(unit, false);
        return false;
    }

    private Boolean isInPackageList(String s) {
        for (String prefix : packagePrefixes) {
            if (s.startsWith(prefix)) {
                return true;
            }
        }
        return false;
    }

    private boolean isSeed(SootMethod method, SootClass superClass) {
        Boolean res = methodOverride.get(method);
        if (null != res) return res;
        if (null == superClass || superClass.isInterface()) {
            methodOverride.put(method, false);
            return false;
        }

        if (isInPackageList(superClass.getName())) {
            try {
                superClass.checkLevel(SootClass.SIGNATURES);

                for (SootMethod m : superClass.getMethods()) {
                    if(method.getName().equals(m.getName()) &&
                            method.getParameterTypes().equals(m.getParameterTypes()) &&
                            isSubType(method.getReturnType(), m.getReturnType())) {
                        methodOverride.put(method, true);
                        return true;
                    }
                }
            } catch (java.lang.RuntimeException re) {
                /* superClass.checkLevel(SIGNATURE) returns a runtime exception.
                 *
                 * In this case we generate a warning and we return false.
                 * We do not want to block the extraction because some class was not
                 * resolved.
                 */
            }
        }

        /* try to look in the superclass */
        boolean isSeed = superClass.hasSuperclass() &&
                isSeed(method, superClass.getSuperclass());
        methodOverride.put(method, isSeed);
        return isSeed;
    }


    /**
     *
     * @return true if t1 is a (unproper) subtype of t2
     */
    public boolean isSubType(Type t1, Type t2) {
        if (t1.equals(t2)) return true;
        else if (t1 instanceof RefType && t2 instanceof RefType) {
            SootClass c1 = ((RefType) t1).getSootClass();
            SootClass c2 = ((RefType) t2).getSootClass();

            Hierarchy h = Scene.v().getActiveHierarchy();
            boolean isSubclass = h.isClassSubclassOfIncluding(c1, c2);
            return isSubclass;
        }
        else {
            /* primitive types */
            return false;
        }
    }

    public String getCriterionDescription() {
        StringBuilder sBuffer = new StringBuilder("MethodPacakgeSeed criterion\n" +
                "The seed are all the method invocation that belongs to the " +
                "following packages:\n");
        for (String packageName : this.packagePrefixes) {
            sBuffer.append(packageName + "\n");
        }
        return sBuffer.toString();
    }

}