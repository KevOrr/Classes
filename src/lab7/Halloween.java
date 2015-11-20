package lab7;
/*
Program:    Halloween.java
Student:    Kevin Orr
Desc:       Simulates a Halloween night
*/

import java.util.Scanner;

public class Halloween {

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        System.out.println("Which candy should we give out first? Candy in pot 1 or candy in pot 2?");
        int candyPot = scan.nextInt();

        System.out.println("How much candy did we buy?");
        int totalCandy = scan.nextInt();

        TreatHouse ourHouse = new TreatHouse(candyPot, totalCandy);

        while (ourHouse.getCandyCount() > 0) {
            ourHouse.getCandyStatus(); //tells how much candy is left & other stuff

            System.out.println("How much candy per treater should we give out?");
            int treatsPerTreater = scan.nextInt();
            ourHouse.setTreatsPerTreater(treatsPerTreater);
                        
            System.out.println("Knock, knock...." + "Trick or treat!");
            ourHouse.knockKnock();
            ourHouse.passOutCandy();
        }

        System.out.println("Time to turn off the lights and go to bed!");
        System.out.println("The last candy came from pot number"+ ourHouse.getLastPot());
        System.out.println("Happy Halloween!");
        scan.close();
    }
}
