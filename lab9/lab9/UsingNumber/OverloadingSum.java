package lab9.UsingNumber;
/* Program:    OverloadingSum.java
 * Student:    Kevin Orr
 * Desc:       Defines the sum between 2 or 3 Numbers
 */

public class OverloadingSum {
    /* In this and the following classes I chose to write a single method accepting
     * and returning Number instances for each number of arguments that sum can
     * accept. This dramatically reduces the amount of duplicated code and makes,
     * in my opinion, for a much more readable and maintainable source. In all
     * "correct" (i.e. all but the ZeroSum) implementations it is first checked
     * whether both Numbers are integral (i.e. num % 1 == 0). If both are, each
     * longValue is extracted, and the resultant sum is a long, autoboxed into a
     * Number. If at least one Number has a decimal part, then each doubleValue
     * is extracted. The reason for choosing long and double is that they are
     * the most precise integral and decimal primitives respectively , and so no
     * loss of precision will be experienced unless using BigInteger or BigDecimal.
     */
    
    public Number sum(Number n1, Number n2) {
        // Are both numbers integral? If so, extract the long values and return a long
        if (n1.doubleValue() % 1 == 0 && n2.doubleValue() % 1 == 0) {
            return n1.longValue() + n2.longValue();
        } else {
            // Else extract and return doubles
            // Note that integral numbers > 2^53 cannot be exactly represented by a double
            return n1.doubleValue() + n2.doubleValue();
        }
    }

    public Number sum(Number n1, Number n2, Number n3) {
        return sum(n1, sum(n2, n3));
    }
}
