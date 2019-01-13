import soot.Unit;

/**
 * Defines a Slice Criteria: when a unit of a CFG must be a seed of a groum.graphs.slice.
 *
 */

public interface SliceCriteria {

    /**
     * Returns true if the instruction represented by unit should be a seed for the slicing
     *
     * @param unit The instruction currently analyzed
     * @return True if unit is a seed
     */

    public boolean is_seed(Unit unit);

    /**
     * Returns a string representation of the criteria.
     * Used for debug purposes.
     * @return a string that describes the slicing criteria
     */

    public String getCriterionDescription();

}
