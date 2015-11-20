/*
Program:    PowersOf2.java
Student:    Kevin Orr
Desc:       Prints n powers of 2, starting at 2^0 == 1
*/

import java.util.Scanner;

public class PowersOf2 {
    public static void main(String[] args) {
        int numPowersOf2;
        int nextPowerOf2 = 1;
        int exponent = 0;
        Scanner scanner = new Scanner(System.in);

        // Ask how many to print
        System.out.println("How many powers of 2 would you like printed? ");
        numPowersOf2 = scanner.nextInt();

        // Confirm selection
        System.out.println("Here are the first " + numPowersOf2 + " powers of 2:");

        // Print powers
        do {
            System.out.printf("2^%d = %d\n", exponent, nextPowerOf2);
            exponent++;
            nextPowerOf2 *= 2;
        } while (exponent < numPowersOf2);
    }
}
