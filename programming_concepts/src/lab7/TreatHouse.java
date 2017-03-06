package lab7;
/*
Program:    TreatHouse.java
Student:    Kevin Orr
Desc:       Defines a house that gives out candy on halloween
*/

import java.util.Random;

public class TreatHouse {
    int[] candyPots = new int[2]; //amount of candy in pots
    int currentPot; //1 or 2
    int totalCandy;
    int currentTreaters; 
    int treatsPerTreater;

    public TreatHouse(int candyPot, int totalCandy) {
        // Fix totalCandy if need be
        if (totalCandy <= 0) {
            System.out.println("We can't give out candy if we don't have any."
                    + "I think we have some from last year."
                    + "Yep, we have 100 pieces of candy to give out.");
            totalCandy = 100;
        }
        
        // Fill pots
        this.candyPots[0] = totalCandy / 2;
        this.candyPots[1] = totalCandy - this.candyPots[0];
        this.totalCandy = totalCandy;

        // Fix pot number if need be, set current and last pots
        if (candyPot != 1 && candyPot != 2) {
            System.out.println("Invalid input, we will use candy pot 1 first.");
            this.currentPot = 0;
        } else {
            this.currentPot = candyPot - 1;
        }
    }

    public int getCandyCount() {
        return this.candyPots[0] + this.candyPots[1];
    }

    // Pass out candy if enough and switch pots, else do nothing
    public void passOutCandy() {
        if (this.candyPots[this.currentPot] >= this.currentTreaters) {
            this.candyPots[this.currentPot] -= this.currentTreaters * this.treatsPerTreater;
            this.currentPot = 1 - this.currentPot;
        } else {
            System.out.println("No candy for you!");
        }
    }

    //Sets the number of trick or treaters.
    public void knockKnock() {
        Random gen = new Random(System.currentTimeMillis());
        this.currentTreaters = gen.nextInt(13) + 1; //1 to 13 treaters.
    }
    
    //Displays how much candy in each pot, total candy left
    public void getCandyStatus() {
        System.out.printf("Pot 1 Candy: %d\nPot 2 Candy: %d\nTotal Candy: %d\n",
                this.candyPots[0], this.candyPots[1], getCandyCount());
        
    }

    //returns the pot number for which candy was last given.
    public int getLastPot() {
        return (1 - this.currentPot) + 1;
    }

    public void setTreatsPerTreater(int treatsPerTreater) {
        this.treatsPerTreater = treatsPerTreater;
    }
}
