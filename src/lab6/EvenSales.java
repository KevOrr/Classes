package lab6;
/*
Program:    EvenSales.java
Student:    Kevin Orr
Desc:       Determines the max, min, mean, and total sales made, as well as
            how many salespeople exceeded a certain sales goal
*/

import java.text.NumberFormat;
import java.util.Scanner;

public class EvenSales {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        NumberFormat currFmt = NumberFormat.getCurrencyInstance();

        // Get a valid number of salespeople
        System.out.print("Number of salespeople: ");
        int salespeople;
        while (true) {
            salespeople = scan.nextInt();
            if (salespeople > 0 && salespeople % 2 == 0) break;
            System.out.print("Number of salespeople must be a positive even integer: ");
        }

        // Initialize sales array, as well as max and min variables
        int[] sales = new int[salespeople];
        int max = 0;
        int min = Integer.MAX_VALUE;
        int maxId = 0;
        int minId = 0;

        // Collect sales, calculate max and min
        for (int i=0; i<sales.length; i++) {
            System.out.print("Enter sales for salesperson " + (2*i + 2) + ": ");
            sales[i] = scan.nextInt();
            if (max < sales[i]) {
                max = sales[i];
                maxId = i;
            }
            if (min > sales[i]) {
                min = sales[i];
                minId = i;
            }
        }

        // Confirm entries, calculate sum
        System.out.println("\nSalesperson   Sales");
        System.out.println("--------------------");
        int sum = 0;
        for (int i=0; i<sales.length; i++) {
                System.out.println("     " + (2*i + 2) + "         " +
                                   currFmt.format(sales[i]));
                sum += sales[i];
          }

        // Print results
        System.out.println("\nTotal sales: " + currFmt.format(sum));
        System.out.println("Average Sales: " + currFmt.format(
                               (double) sum / salespeople));
        System.out.println("Salesperson " + (2*maxId + 2) +
                           " had the highest sale with " + currFmt.format(max));
        System.out.println("Salesperson " + (2*minId + 2) +
                           " had the lowest sale with " + currFmt.format(min));

        // Get goal
        System.out.print("Enter sales goal: ");
        int goal;
        while (true) {
            goal = scan.nextInt();
            if (goal > 0 && goal % 2 == 0) break;
            System.out.print("Sales goal must be a positive even integer: ");
        }

        // Print salespeople who *exceeded* their goal
        System.out.println("Salespeoeople who exceeded their goal:");
        for (int i=0; i < salespeople; i++) {
            if (sales[i] > goal) {
                System.out.println(2*i + 2);
            }
        }
    }
}