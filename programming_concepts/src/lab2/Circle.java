package lab2;
/*
Program:    Circle.java
Student:    Kevin Orr
Desc:       Prints the area of two circles with unique radii
*/

public class Circle {

    public static void main(String[] args) {
        final double PI = 3.14159;

        // Calculate properties of circle #1
        int radius1 = 10;
        double area1 = PI * radius1 * radius1;
        double circumfrence1 = 2 * PI * radius1;
        System.out.println("The area of a circle with radius " + radius1 +
                           " is " + area1);
        System.out.println("Its circumfrence is " + circumfrence1 + "\n");

        // Calculate properties of circle #2
        int radius2 = 20;
        double area2 = PI * radius2 * radius2;
        double circumfrence2 = 2 * PI * radius2;
        System.out.println("The area of a circle with radius " + radius2 +
                           " is " + area2);
        System.out.println("Its circumfrence is " + circumfrence2 + "\n");

        // Calculate changes in properties
        double areaRatio = area2 / area1;
        double circumfrenceRatio = circumfrence2 / circumfrence1;
        System.out.println("The area increased by a factor of " + areaRatio);
        System.out.println("The circumfrence increased by a factor of " +
                           circumfrenceRatio);

    }

}
