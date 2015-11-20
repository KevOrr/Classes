/*
Program:    Salary.java
Student:    Kevin Orr
Desc:       Computes a raise based on employee performance and current salary
*/

import java.text.NumberFormat;
import java.util.Scanner;

public class Salary {

    public static void main(String[] args) {
        double currentSalary; // employee's current salary
        String rating; // performance rating
        double raise = 0.0; // amount of the raise
        double newSalary; // new salary for the employee

        Scanner scan = new Scanner(System.in);

        System.out.print("Enter the current salary: ");
        currentSalary = scan.nextDouble();
        System.out.print("Enter the performance rating (Excellent, Good, or Poor): ");
        rating = scan.next().toLowerCase();

        // Compute the raise using if ...
        if (rating.equals("excellent")) {
            raise = .06 * currentSalary;
        } else if (rating.equals("good")) {
            raise = .04 * currentSalary;
        } else if (rating.equals("poor")) {
            raise = .015 * currentSalary;
        }

        newSalary = currentSalary + raise;

        // Print the results
        NumberFormat money = NumberFormat.getCurrencyInstance();
        System.out.println();
        System.out.println("Current Salary: " + money.format(currentSalary));
        System.out.println("Amount of your raise: " + money.format(raise));
        System.out.println("Your new salary: " + money.format(newSalary));
        System.out.println();
    }
}
