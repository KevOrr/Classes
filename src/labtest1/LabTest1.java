package labtest1;

/*
Student: Kevin Orr
Program: LabTest1.java
Desc:    Calculates the necessary side length of a square that has twice the area
         of a circle with specified radius. Then calculates the associated
         cost of each per unit length of permieter.
*/

import java.util.Scanner;
import java.text.DecimalFormat;
import java.text.NumberFormat;

public class LabTest1 {
    public static void main(String[] args) {
        final double COST_PER_UNIT = 2.25; // per unit length
        double radius, area, areaDoubled, sideLength;
        double cost1, cost2;

        // initialize Scanner and Formatters
        Scanner scanner = new Scanner(System.in);
        DecimalFormat decFmt = new DecimalFormat("0.0000");
        NumberFormat currFmt = NumberFormat.getCurrencyInstance();

        // print given description
        System.out.println(
            "This program will find new lengths and widths for a square\n"
           +"with double the area of an inputted circle.\n"
           +"The new length will have 4 points of decimal precision in the\n"
           +"formatted output."
        );

        // get radius
        System.out.print("Enter radius: ");
        radius = scanner.nextDouble();

        // calculate area, areaDoubled, sideLength
        area = Math.PI * Math.pow(radius, 2);
        areaDoubled = 2 * area;
        sideLength = Math.sqrt(areaDoubled);

        // calculate costs
        cost1 = 2 * Math.PI * radius * COST_PER_UNIT;
        cost2 = 4 * sideLength * COST_PER_UNIT;

        // output results
        System.out.println("\nCircle Area: " + decFmt.format(area));
        System.out.println("Doubled Area: " + decFmt.format(areaDoubled));
        System.out.println("New length: " + decFmt.format(sideLength));
        System.out.println("New width: " + decFmt.format(sideLength));
        System.out.println("Old Cost: " + currFmt.format(cost1));
        System.out.println("New Square Cost: " + currFmt.format(cost2));

        // difference always positive, since 2pi*r > 4 * sqrt(2 * pi*r^2) for all r
        System.out.println("Difference of Costs: " + currFmt.format(cost2 - cost1));
    }
}
