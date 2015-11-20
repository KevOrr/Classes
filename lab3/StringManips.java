/*
Program:    StringManips.java
Student:    Kevin Orr
Desc:       Performs various operations on user input strings
*/

import java.util.Scanner;

public class StringManips {

    public static void main (String[] args) {
        String phrase;
        int phraseLength; // number of characters in `phrase`
        int middleIndex; // index of the middle character in `phrase` (round up)
        String firstHalf, secondHalf; // `phrase` bisected
        String switchedPhrase; // a new phrase with original halves switched
        String middle3; // middle 3 characters
        String city, state; // city and state

        // read in a phrase
        Scanner scan = new Scanner(System.in);
        System.out.print("Please enter a phrase: ");
        phrase = scan.nextLine();

        // read in city and state
        System.out.print("City: ");
        city = scan.nextLine().toLowerCase();
        System.out.print("State: ");
        state = scan.nextLine().toUpperCase();

        // compute the length and middle index of the phrase
        phraseLength = phrase.length();
        middleIndex = phraseLength / 2;

        // get the substring for each half of the phrase
        firstHalf = phrase.substring(0, middleIndex);
        secondHalf = phrase.substring(middleIndex);

        // concatenate the firstHalf at the end of the secondHalf
        switchedPhrase = (secondHalf + firstHalf).replace(' ', '*');

        // get middle 3 characters
        middle3 = phrase.substring(middleIndex - 1, middleIndex + 2);
 
        // print information about the phrase
        System.out.println();
        System.out.println("Original phrase: " + phrase);
        System.out.println("Length of the phrase: " + phraseLength +
                           " characters");
        System.out.println("Index of the middle: " + middleIndex);
        System.out.println("Character at the middle index: " +
                           phrase.charAt(middleIndex));
        System.out.println("Switched phrase: " + switchedPhrase);
        System.out.println("Middle 3 characters: " + middle3);
        System.out.println();

        // city and state
        System.out.println(state + city + state);
    }
}
