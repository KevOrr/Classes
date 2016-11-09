package lab3;
/*
Program:    Torus.java
Student:    Kevin Orr
Desc:       Computes geometric properties of Torus objects
*/

import java.text.DecimalFormat;
import java.util.Scanner;

public class Torus {

    public static void main(String[] args) {
        double r1, r2, R1, R2;
        double surfaceAreaDiff, volumeDiff;
        final double PI2 = Math.pow(Math.PI, 2); 

        DecimalFormat fmt = new DecimalFormat("0.000");
        Scanner scanner = new Scanner(System.in);

        // Get torus 1 parameters
        System.out.println("Torus 1");
        System.out.print("Major radius (from center hole to tube center): ");
        R1 = scanner.nextDouble();
        System.out.print("Minor radius (tube radius): ");
        r1 = scanner.nextDouble();

        // Get torus 2 parameters
        System.out.println("\nTorus 2");
        System.out.print("Major radius (from center hole to tube center): ");
        R2 = scanner.nextDouble();
        System.out.print("Minor radius (tube radius): ");
        r2 = scanner.nextDouble();

        // Compute differences
        surfaceAreaDiff = 4 * PI2 * Math.abs(R1 * r1 - R2 * r2);
        volumeDiff = 2 * PI2 * (R1 * Math.pow(r1, 2) - R2 * Math.pow(r2, 2));
        volumeDiff = Math.abs(volumeDiff);

        // Print results
        System.out.println("\nSurface area difference: " + fmt.format(surfaceAreaDiff));
        System.out.println("Volume difference: " + fmt.format(volumeDiff));
    }
}
