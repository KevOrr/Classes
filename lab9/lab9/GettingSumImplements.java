/* Program:    GettingSumImplements.java
 * Student:    Kevin Orr
 * Desc:       Tests ImplementingSum.java
 */

package lab9;

import java.text.DecimalFormat;

public class GettingSumImplements {

    public static void main(String[] args) {
        ImplementingSum impSum = new ImplementingSum();
        DecimalFormat fmt = new DecimalFormat("0.####");
        Number[][] nums = {
                              {(short) 2,  5.8, 6.3, 4L,   5.8f},
                              {(byte) 127, 84L, 24,  9.84, 8.5},
                              {      4.56, 4.8, 24L, 8.4,  14L}
                          };

        System.out.println("Sums:");
        for (int i=0; i<nums[0].length; i++) {
            System.out.print("" + nums[0][i] + " + " + nums[1][i] + " + "
                            + nums[2][i] + " = ");
            Number res = impSum.sum(nums[0][i], nums[1][i], nums[2][i]);
            System.out.println(fmt.format(res));
        }

        System.out.println("\nProducts:");
        for (int i=0; i<nums[0].length; i++) {
            System.out.print("" + nums[0][i] + " * " + nums[1][i] + " * "
                            + nums[2][i] + " = ");
            Number res = impSum.multiply(nums[0][i], nums[1][i], nums[2][i]);
            System.out.println(fmt.format(res));
        }
    }

}
