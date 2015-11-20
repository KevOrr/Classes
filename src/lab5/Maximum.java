package lab5;
/*
Program:    Maximum.java
Student:    Kevin Orr
Desc:       Finds the number of occurrences of the maximum of a list of integers 
*/

import java.util.Scanner;

public class Maximum {
    public static void main(String[] args) {
        int max = 0; // the current maximum integer
        int count = 0; // and the number of its occurrences
        int currentInt; // the current integer, used for exiting the counting loop
        Scanner scanner = new Scanner(System.in);

        System.out.print("Enter numbers separated by whitespace (0 to terminate): ");

        do {
            currentInt = scanner.nextInt();
            if (currentInt > max) { // replace max, reset count
                max = currentInt;
                count = 1;
            } else if (currentInt == max) { // increment count
                count++;
            }
        } while (currentInt != 0); // end of list, exit counting loop

        // output results
        System.out.println("The largest number is " + max);
        System.out.println("The occurrence count of the largest number is " + count);
        
    }
}
