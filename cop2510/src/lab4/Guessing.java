package lab4;
/*
Program:    Guessing.java
Student:    Kevin Orr
Desc:       Plays a guessing game with the user
*/

import java.util.*;

public class Guessing {
    public static void main(String[] args) {
        final int MAX = 10;
        int answer, guess;
        Scanner scanner = new Scanner(System.in);
        Random generator = new Random();

        // Think of a number and prompt for guess
        answer = generator.nextInt(MAX) + 1;
        System.out.print("I'm thinking of a number between 1 and "
                          + MAX + ". Guess what it is: ");

        // Continue prompting until user correctly guesses
        while (scanner.nextInt() != answer) {
            System.out.print("Your guess is not correct\nGuess again: ");
        }

        // User guessed correctly
        System.out.println("You got it! Good guessing!");
    }
}
