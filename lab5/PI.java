/*
Program:    PI.java
Student:    Kevin Orr
Desc:       Estimates Pi using the Monte Carlo Method.
            Background/proof: http://bit.ly/1OZE2Iw
*/

import java.util.Scanner;
import java.util.Random;
import java.text.DecimalFormat;

public class PI {
    public static void main(String[] args) {
        float x, y; // the x and y values for each dart
        double magnitude; // the distance of the dart to the origin sqrt(x^2 + y^2) 
        int successes, trials; // the numbers of successful and total darts
        double piEstimate; // the estimated value of pi

        Scanner scanner = new Scanner(System.in);
        Random rand = new Random(System.currentTimeMillis());
        DecimalFormat dfmt = new DecimalFormat("#0.0");

        // get sample size
        System.out.print("Number of darts: ");
        trials = scanner.nextInt();

        // run simulation and count successes
        successes = 0;
        for (int i=0; i < trials; i++) {
            x = rand.nextFloat();
            y = rand.nextFloat();
            magnitude = Math.sqrt((double)(x) * x + (double)(y) * y);
            if (magnitude < 1) { // dart landed inside circular quadrant
                successes++;
            }
        }
        System.out.println(successes + " successes (" +
                           dfmt.format(100.0d * successes / trials) + "%)");

        // Estimate pi using simulation results (http://bit.ly/1OZE2Iw)
        piEstimate = 4 * (double) successes / trials;
        System.out.println("The estimated value of pi is " + piEstimate);
    }
}
