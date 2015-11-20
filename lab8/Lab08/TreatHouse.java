package Lab08;

import java.util.Random;
import java.util.Scanner;

public class TreatHouse {
    private static int participatingHouses = 0;
    private static int totalCandyPassed = 0;
    private static int totalTricksGiven = 0;
    
    private int candyPot1; // amount of candy in pot 1 for a single TreatHouse
    private int candyPot2; // amount of candy in pot 2 for a single TreatHouse
    private int currentPot; // (1 or 2) current Pot for a single TreatHouse
    private int totalCandy; // total candy for a single TreatHouse
    private int currentTreaters; // current Treaters for a single TreatHouse
    private boolean isDone = false; // is this house done for the night?
    private int treatsPerTreater; //How much candy to give out to each treater at a single TreatHouse
    private final int HOUSE_ID;
    
    //Static variables
    // participatingHouses //This contains the total number of houses participating in this year's holiday
    // totalCandyPassedOut //This contains the total number of candy passed out for all the houses.
    // totalTricksGiven    //This contains the total number of tricks given out. Remember that each kid gets tricked.
    // houseID               //This is the number for the house which is unique.
    
    //Method to display the statistic variables
//    getHalloweenStatus(){
//        
//    }
    
    

    public TreatHouse(int candyPot, int totalCandy) {
        // increments count and assigns to HOUSE_ID (instructions suggest start at 1)
        this.HOUSE_ID = ++TreatHouse.participatingHouses; 
        
        // Error Check and split candy
        if (totalCandy > 0) {
            candyPot1 = totalCandy / 2;
            candyPot2 = totalCandy / 2 + totalCandy % 2;
        } else {
            System.out.println("We can't give out candy if we don't have any."
                    + " I think we have some from last year. Yep, we have 100 " + "" + "pieces of candy to give out.");
            totalCandy = 100;
            candyPot1 = totalCandy / 2;
            candyPot2 = totalCandy / 2 + totalCandy % 2;
        }

        // currentPot Check
        if (candyPot == 1) {
            currentPot = 1;
        } else if (candyPot == 2) {
            currentPot = 2;
        } else {
            System.out.println("Invalid choice for pot. Only 1 or 2. Using pot 1");
            currentPot = 1;
        }
    }

    public void passTime() {
        Scanner scan = new Scanner(System.in);

        //before
        this.getCandyStatus();
        
        System.out.println("How much candy per treater should we give out?");
        this.setTreatsPerTreater(scan.nextInt());

        System.out.println("Knock, knock...." + "Trick or treat!");
        this.knockKnock();
        this.passOutCandy();
        
        //After
        this.getCandyStatus();

        //Determines if the house if done for the night.
        if(totalCandy == 0){
            System.out.println("Time to turn off the lights and go to bed!");
            System.out.println("The last candy came from pot number" + this.getLastPot());
            System.out.println("Happy Halloween!");
            isDone = true;
        }
    }

    private void printCurrentPot() {
        System.out.println("The current Pot is: " + currentPot);        
    }



    public int getCandyCount() {
        return candyPot1 + candyPot2;
    }

    public void passOutCandy() {
        // If there are enough treats per treater for the given amount per
        // treater, pass out candy from the current pot and switch to the other one.
        // Else display a message that the treaters have been tricked... (no
        // candy for them.) but do not change the current pot

        if ((currentPot == 1 && candyPot1 == 0) || (currentPot == 2 && candyPot2 == 0)) {
            // Switch pots and print message
            System.out.println("There is no candy in the currentPot Switching pots.");
            switchPots();
        }

        {// There is some candy to give out.
            int currentPotAmmount = getCurrentPotAmount();
            if (treatsPerTreater * currentTreaters <= currentPotAmmount) {
                int treats = treatsPerTreater * currentTreaters;
                TreatHouse.totalCandyPassed += treats;
                //Passing out Candy
                if (currentPot == 1) {
                    candyPot1 -= treats;
                    totalCandy -= treats;
                } else {
                    candyPot2 -= treats;
                    totalCandy -= treats;
                }
                switchPots();
            } else {
                // trick
                System.out.println("You have been tricked!");
                TreatHouse.totalTricksGiven += this.currentTreaters;
            }

        }

    }

    private int getCurrentPotAmount() {
        if (currentPot == 1)
            return candyPot1;
        else
            return candyPot2;
    }

    private void switchPots() {
        if (currentPot == 1) {
            currentPot = 2;
        } else
            currentPot = 1;

    }

    // Sets the number of trick or treaters.
    public void knockKnock() {
        Random gen = new Random(System.currentTimeMillis());
        this.currentTreaters = gen.nextInt(13) + 1; // 1 to 13 treaters.
        System.out.println("There are " + currentTreaters + " treaters.");
    }

    // Displays how much candy in each pot, total candy left

    public void getCandyStatus() {
        this.printCurrentPot();
        System.out.println("Candy in Pot 1: " + candyPot1);
        System.out.println("Candy in Pot 2: " + candyPot2);
    }

    public static void getHalloweenStatus() {
        System.out.println("Participating Houses: " + TreatHouse.participatingHouses);
        System.out.println("Total candy passed out: " + TreatHouse.totalCandyPassed);
        System.out.println("Total tricks given: " + TreatHouse.totalTricksGiven);
    }

    // returns the pot number for which candy was last given.
    public int getLastPot() {
        if (currentPot == 1)
            return 2;
        else {
            return 1;
        }
    }

    public void setTreatsPerTreater(int treatsPerTreater) {
        this.treatsPerTreater = treatsPerTreater;
    }

    public boolean isDone() {
                
        return isDone;
    }
}