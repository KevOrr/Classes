package lab08;

import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class Halloween {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        ArrayList<TreatHouse> houses = new ArrayList<TreatHouse>();

        Random gen = new Random(System.currentTimeMillis());
        int numHouse = gen.nextInt(10) + 1;
        
        // create houses
        for (int i = 0; i < numHouse; i++) {
            System.out.println("House " + i );
            System.out.println("Which candy should we give out first? Candy in pot 1 or candy in pot 2?");
            int candyPot = scan.nextInt();

            System.out.println("How much candy did we buy?");
            int totalCandy = scan.nextInt();

            houses.add( new TreatHouse(candyPot, totalCandy));
        }
        
        boolean allDone = false;
        while(!allDone){
            //Each house will pass out candy for a bit.
            for (TreatHouse house : houses) {
                house.passTime();                
            }
            
            //Check to see if all the house are done. If so set sentinel flag to true and exit while loop
            allDone = false;
            for(TreatHouse house: houses){
                //If a single house is not done then all are not done
                if(!house.isDone()){
                    allDone = false;
                    break;
                }
                else
                    allDone = true;
            }
            
            //Call static method to display some statistics about the holiday so far
            
            
        }
        System.out.println("All houses are done");
        scan.close();
    }
}