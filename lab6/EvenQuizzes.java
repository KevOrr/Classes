/*
Program:    EvenQuizzes.java
Student:    Kevin Orr
Desc:       Grades quizzes based on a given key
*/

import java.text.NumberFormat;
import java.util.Scanner;

public class EvenQuizzes {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        NumberFormat fmt = NumberFormat.getPercentInstance();

        System.out.print("Number of questions: ");
        int length = scanner.nextInt();
        int[] key = new int[length];

        System.out.println("Enter grading key (" + length + " answers)");
        for (int i=0; i < length; i++) {
            key[i] = scanner.nextInt();
        }
        
        do {
            System.out.println("Enter " + length + " answers");

            int correct = 0;
            for (int i=0; i < length; i++) {
                correct += scanner.nextInt() == key[i] && i % 2 == 1 ? 1 : 0;
            }

            System.out.println("Correct: " + correct);
            System.out.println("Percentage: " + fmt.format((double) correct / (length / 2)));
            System.out.print("Grade another quiz? (y/n): ");
        } while (scanner.next().equalsIgnoreCase("y"));
    }
}
