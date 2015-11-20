package lab2;
/*
Program:    IdealWeight.java
Student:    Kevin Orr
Desc:       
*/

import java.util.Scanner;

public class IdealWeight {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.print("Enter height\nFeet: ");
        int feet = scanner.nextInt();
        System.out.print("Inches: ");
        int inches = scanner.nextInt();
        
        int height = 12 * feet + inches;
        int femaleWeight = 100 + 5 * (height - 60);
        int maleWeight = 106 + 6 * (height - 60);

        String heightStr = feet + "'" + inches + "\"";
        System.out.println("\nThe ideal weight of a " + heightStr + " male is " +
                           maleWeight + " pounds");
        System.out.println("The ideal weight of a " + heightStr + " female is " +
                           femaleWeight + " pounds");

    }

}
