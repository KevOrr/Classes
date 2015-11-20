/*
Program:    Paint.java
Student:    Kevin Orr
Desc:       Determine how much paint is needed to paint the walls of a room
            given its length, width, and height
*/

import java.util.Scanner;

public class Paint {

    public static void main(String[] args) {

        // Declare some useful values
        final int COVERAGE = 350; //paint covers 350 sq ft/gal
        int length, width, height;
        double totalSqFt, paintNeeded;
        Scanner scanner = new Scanner(System.in);

        // Get room length
        System.out.print("Length of room: ");
        length = scanner.nextInt();

        // Get room width
        System.out.print("Width of room: ");
        width = scanner.nextInt();

        // Get room height
        System.out.print("Height of room: ");
        height = scanner.nextInt();

        // Compute the total square feet to be painted
        totalSqFt = 2 * height * (length + width);

        // Compute the amount of paint needed
        paintNeeded = totalSqFt / COVERAGE;

        // Print the length, width, and height of the room and the
        // number of gallons of paint needed.
        System.out.println("A room with dimensions " + length + "' x " +
                           width + "' x " + height + "' would require " +
                           paintNeeded + " gal paint\n");
        
        // Adjust area for windows and doors
        System.out.print("Number of doors (20 sq ft each) in room: ");
        totalSqFt -= 20 * scanner.nextInt();
        System.out.print("Number of windows (15 sq ft each) in room: ");
        totalSqFt -= 15 * scanner.nextInt();
        
        paintNeeded = totalSqFt / COVERAGE;
        
        // Print final volume
        System.out.println("This room will require " + paintNeeded +
                           " gal paint");

    }

}
