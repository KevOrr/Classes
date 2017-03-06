package lab5;
/*
Program:    Dates.java
Student:    Kevin Orr
Desc:       Validates a second millenium date
*/

import java.util.Scanner;

public class Dates {
    public static void main(String[] args) {
        int month, day, year; //date read in from user
        int daysInMonth; //number of days in month read in
        boolean monthValid, yearValid, dayValid; //true if input from user is valid
        boolean leapYear; //true if user's year is a leap year
        Scanner scanner = new Scanner(System.in);

        //Get integer month, day, and year from user
        System.out.print("Month: ");
        month = scanner.nextInt();
        System.out.print("Day: ");
        day = scanner.nextInt();
        System.out.print("Year: ");
        year = scanner.nextInt();

        //Check to see if month is valid
        monthValid = month >= 1 && month <= 12;

        //Check to see if year is valid
        yearValid = year >= 1000 && year <= 1999;

        //Determine whether it's a leap year
        leapYear = ((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0);

        // ↓Would this have been breaking the rules?↓
        /*switch (month) {
        case 1:
        case 3:
        case 5:
        case 7:
        case 8:
        case 10:
        case 12:
            dayValid = day >= 1 && day <= 31;
            break;
        case 4:
        case 6:
        case 9:
        case 11:
            dayValid = day >= 1 && day <= 30;
            break;
        case 2:
            dayValid = day >= 1 && day <= (leapYear ? 29 : 28);
            break;
        }*/
        
        if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 ||
            month == 10 || month == 12) {
            daysInMonth = 31;
        } else if (month == 4 || month == 6 || month == 9 || month == 11) {
            daysInMonth = 30;
        } else if (month == 2) {
            daysInMonth = leapYear ? 29 : 28;
        } else {
            daysInMonth = 0;
        }
        
        //Use number of days in month to check to see if day is valid
        dayValid = day >= 1 && day <= daysInMonth;

        //Determine whether date is valid and print appropriate message
        if (monthValid && dayValid && yearValid) {
            System.out.println("Date is valid");
            System.out.println(leapYear ? "It is a leap year" : "It is not a leap year");
        } else {
            System.out.println("Date is not valid");
        }

    }
}