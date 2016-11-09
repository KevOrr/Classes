package lab5;
/*
Program:    Count.java
Student:    Kevin Orr
Desc:       Returns the count of spaces and a's, e's, s's, and t's
*/

import java.util.Scanner;

public class Count {
    public static void main (String[] args) {
        String phrase;    // a string of characters
        int countBlank;   // the number of blanks (spaces) in the phrase 
        int countA, countE, countS, countT; // letter counts
        int length;       // the length of the phrase

        Scanner scanner = new Scanner(System.in);

        // Print a program header
        System.out.println();
        System.out.println("Character Counter");
        System.out.println();

        while (true) {
            // Read in a string and find its length
            System.out.print("Enter a sentence or phrase (or \"quit\" to quit): ");
            phrase = scanner.nextLine();
            if (phrase.equals("quit")) {
                break;
            }
            length = phrase.length();
    
            // Initialize counts
            countBlank = 0;
            countA = 0;
            countE = 0;
            countS = 0;
            countT = 0;
    
            // a for loop to go through the string character by character
            // and count the characters
            for (int i=0; i < length; i++) {
                switch (Character.toLowerCase(phrase.charAt(i))) {
                case ' ':
                    countBlank++;
                    break;
                case 'a':
                    countA++;
                    break;
                case 'e':
                    countE++;
                    break;
                case 's':
                    countS++;
                    break;
                case 't':
                    countT++;
                    break;
                }
            }
    
            // Print the results
            System.out.println();
            System.out.println("Number of blank spaces: " + countBlank);
            System.out.println("Number of a's: " + countA);
            System.out.println("Number of e's: " + countE);
            System.out.println("Number of s's: " + countS);
            System.out.println("Number of t's: " + countT);
            System.out.println();
        }
    }
}