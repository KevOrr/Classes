/*
Program:    FindAlmostNearestPoints.java
Student:    Kevin Orr
Desc:       Finds the second shortest edge of a mesh using the 2-norm between each node
            (aka Euclidean norm/magnitude)
*/

import java.util.Scanner;

public class FindAlmostNearestPoints {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Get points
        System.out.print("Number of points: ");
        int length = scanner.nextInt();
        System.out.println("Enter points:");
        double[][] points = new double[length][2];
        for (int i=0; i < length; i++) {
            points[i][0] = scanner.nextDouble();
            points[i][1] = scanner.nextDouble();
        }

        // Get second shortest edge
        double shortest = Double.MAX_VALUE;
        double[][] shortestPoints = new double[2][2];
        double secondShortest = Double.MAX_VALUE;
        double[][] secondShortestPoints = new double[2][2];
        for (int i=0; i<length; i++) {
            for (int j=i+1; j<length; j++) {
                double x = points[j][0] - points[i][0];
                double y = points[j][1] - points[i][1];
                double dist = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));

                if (dist < shortest) { // move shortest->second, dist->shortest
                    secondShortestPoints = shortestPoints.clone();
                    secondShortest = shortest;

                    shortest = dist;
                    shortestPoints[0] = points[i];
                    shortestPoints[1] = points[j];
                } else if (dist >= shortest && dist < secondShortest) { // move dist->second
                    secondShortest = dist;
                    secondShortestPoints[0] = points[i];
                    secondShortestPoints[1] = points[j];
                }
            }
        }

        System.out.println("Second shortest distance: " + secondShortest);
        System.out.println("Corresponding points:");
        System.out.format("(%f, %f)\n", secondShortestPoints[0][0], secondShortestPoints[0][1]);
        System.out.format("(%f, %f)\n", secondShortestPoints[1][0], secondShortestPoints[1][1]);
        
    }
}
