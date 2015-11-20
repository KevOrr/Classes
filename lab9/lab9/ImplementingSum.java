package lab9;
/* Program:    ImplementingSum.java
 * Student:    Kevin Orr
 * Desc:       Implements the methods specified by InterSum and Multiply
 */

public class ImplementingSum implements InterSum, Multiply {
    /* Again, I chose to reuse the OverloadingSum#sum methods rather than
     * redefining them
     */

    public Number sum(Number n1, Number n2) {
        return new OverloadingSum().sum(n1, n2);
    }

    public Number sum(Number n1, Number n2, Number n3) {
        return sum(n1, sum(n2, n3));
    }

    public Number multiply(Number n1, Number n2) {
        // Are both numbers integral? If so, extract the int values and return an int
        if (n1.doubleValue() % 1 == 0 && n2.doubleValue() % 1 == 0) {
            return n1.longValue() * n2.longValue();
        } else {
            // Else extract and return doubles
            // Note that integral numbers > 2^53 cannot be exactly represented by a double
            return n1.doubleValue() * n2.doubleValue();
        }
    }

    public Number multiply(Number n1, Number n2, Number n3) {
        return multiply(n1, multiply(n2, n3));
    }
}
