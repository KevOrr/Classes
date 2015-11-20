package lab9.UsingNumber;
/* Program:    RealSum.java
 * Student:    Kevin Orr
 * Desc:       Extends RealSum to return correct sums
 */

public class RealSum extends ZeroSum {
    /* I chose here to use the already-defined OverloadingSum#sum methods to
     * emphasize the modern programming tenets of code re-use and portability
     */

    @Override
    public Number sum(Number n1, Number n2) {
        return new OverloadingSum().sum(n1, n2);
    }

    @Override
    public Number sum(Number n1, Number n2, Number n3) {
        return sum(n1, sum(n2, n3));
    }
    
    public static void hi() {
        
    }
}
