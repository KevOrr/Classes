package lab3;
/*
Program:    Deli.java
Student:    Kevin Orr
Desc:       Computes the price of a deli item based on weight and unit price
*/

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Scanner;

public class Deli {

    // ---------------------------------------------------
    // main reads in the price per pound of a deli item
    // and number of ounces of a deli item then computes
    // the total price and prints a "label" for the item
    // --------------------------------------------------
    public static void main(String[] args) {

        final double OUNCES_PER_POUND = 16.0;
        double pricePerPound; // price per pound
        double weightOunces; // weight in ounces
        double weight; // weight in pounds
        double totalPrice; // total price for the item

        Scanner scan = new Scanner(System.in);

        // Declare money as a NumberFormat object and use the
        // getCurrencyInstance method to assign it a value
        NumberFormat money = NumberFormat.getCurrencyInstance();

        // Declare fmt as a DecimalFormat object and instantiate
        // it to format numbers with at least one digit to the left of the
        // decimal and the fractional part rounded to two digits.
        DecimalFormat fmt = new DecimalFormat("0.00");

        // prompt the user and read in each input
        System.out.println("Welcome to the CS Deli!!\n");
        System.out.print("Enter the price per pound of your item: ");
        pricePerPound = scan.nextDouble();
        System.out.print("Enter the weight (ounces): ");
        weightOunces = scan.nextDouble();

        // Convert ounces to pounds and compute the total price
        weight = weightOunces / OUNCES_PER_POUND;
        totalPrice = pricePerPound * weight;

        // Print the label using the formatting objects
        // fmt for the weight in pounds and money for the prices
        System.out.print(
            "***** CS Deli *****\n\n"
           +"Unit Price: " + money.format(pricePerPound) + " per pound\n"
           +"Weight: " + fmt.format(weight) + " pounds\n\n"
           +"TOTAL: " + money.format(totalPrice) + "\n"
        );
    }
}
