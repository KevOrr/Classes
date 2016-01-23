package lab4;
/*
Program:    Rock.java
Student:    Kevin Orr
Desc:       Plays a game of RPS with a user
*/

import java.util.Scanner;
import java.util.Random;

public class Rock {
    public static void main(String[] args) {
        String personPlay, computerPlay = ""; // each is one of "R", "P", "S"
        int computerInt; // 0, 1, 2
        Scanner scanner = new Scanner(System.in);
        Random generator = new Random();

        // Generate computer's play (0, 1, 2)
        computerInt = generator.nextInt(3);

        // Translate to string
        if (computerInt == 0) {
            computerPlay = "R";
        } else if (computerInt == 1) {
            computerPlay = "P";
        } else if (computerInt == 2) {
            computerPlay = "S";
        } else {
            System.out.println("Good job, you broke the Matrix");
        }

        // Get player's play
        System.out.print("Enter your play (R, P, or S): ");
        personPlay = scanner.nextLine().toUpperCase();

        // Display computer's choice
        System.out.println("Computer play is " + computerPlay);

        // Calculate winner
        if (personPlay.equals(computerPlay)) {
            System.out.println("It's a tie!");
        } else if (personPlay.equals("R")) {
            if (computerPlay.equals("S")) {
                System.out.println("Rock crushes scissors. You win!!");
            } else {
                System.out.println("Paper covers rock. Somehow you lose!!");
            }
        } else if (personPlay.equals("S")) {
            if (computerPlay.equals("P")) {
                System.out.println("Scissors cuts paper. You win!!");
            } else {
                System.out.println("Rock crushes scissors. You lose!!");
            }
        } else if (personPlay.equals("P")) {
            if (computerPlay.equals("R")) {
                System.out.println("Paper covers rock. Somehow you win!!");
            } else {
                System.out.println("Scissors cut paper. you lose!!");
            }
        }
    }
}
