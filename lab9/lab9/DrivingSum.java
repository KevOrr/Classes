package lab9;
/* Program:    DrivingSum.java
 * Student:    Kevin Orr
 * Desc:       Tests the methods in ZeroSum and RealSum
 */

public class DrivingSum {
    public static void main(String[] args) {
        int x = 10;
        int y = 20;
        // x + y = 30....yay?
        ZeroSum zero= new ZeroSum();
        RealSum real= new RealSum();
        
        System.out.println("Calling ZeroSum: " + zero.sum(x, y) );
        System.out.println("Calling RealSum: " + real.sum(x, y) );
    }
}
