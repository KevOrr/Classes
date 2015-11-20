package lab8;


import java.util.Random;

public class Turkey {
    private final static int MAX_AGE = 100; // days
    private final static int PREGNANCY_DURATION = 7; // a turkey is born 7 days
                                                // after being pregnant
    private final static int ADULT_AGE = 20; // Age at which a turkey is considered an
                                        // adult
    private boolean hasYChromo; // true for male, false for female
    private int id; // unique ID
    private int age; // in days, 0 initially
    private int pairedWithID; // 0 if not paired.
    private int daysPregnant; // 0 initially
    private boolean isPregnant; // initially false
    private boolean isAdult; // initially false
    private boolean isAlive;
    static Random gen = new Random(System.currentTimeMillis());

    // static variables add them here
    private static int nextId = 0; // This holds the value of the next free ID
    private static int count = 0;
    private static int adultCount = 0;
    private static int nonAdultCount = 0;
    private static int pairedCount = 0;
    private static int unpairedCount = 0;
    private static int numSlaughtered = 0;

    // Constructor that creates a turkey of a random gender.
    // add code here
    public Turkey() {
        // This is the constructor for a Turkey. When A turkey is
        // born/constructed It will get a random gender, 1 or 2.
        // Use the random number generator above.
        // The other variable we will set
        // to 0 Initially.
        Turkey.count++;
        Turkey.nextId++;
        Turkey.nonAdultCount++;
        Turkey.unpairedCount++;

        this.hasYChromo = Turkey.gen.nextBoolean();
        this.id = Turkey.nextId;
        this.age = 0;
        this.pairedWithID = 0;
        this.daysPregnant = 0;
        this.isPregnant = false;
        this.isAdult = false;
        this.isAlive = true;
        
    }

    // "birth" a new turkey
    public static Turkey birth(Turkey t1) {
        t1.isPregnant = false;
        t1.daysPregnant = 0;
        return new Turkey();
    }

    public boolean isAdult() {
        // is age > adult age?
        return this.isAdult;
    }

    public boolean isPregnant() {
        // is the turkey pregnant?
        return this.isPregnant;
    }

    public boolean istimeToDie() {
        // is age > max age?
        return this.age > Turkey.MAX_AGE;
    }

    public int getID() {
        // returns the value of the ID of the current Turkey
        return this.id;
    }

    public int getPairID() {
        // this method returns the pairID, the ID of this turkey's partner
        return this.pairedWithID;
    }

    public void unPair() {
        // This method removes the value for pairID and sets it to zero
        if (this.pairedWithID!= 0) {
            this.pairedWithID = 0;
            Turkey.pairedCount -= 2;
            Turkey.unpairedCount += 2;
        }
    }

    public void passTime() {
        // add one to each of the day values. This includes the age and
        // daysPregnant.
        this.age++;
        this.daysPregnant++;
        if (this.age > Turkey.MAX_AGE && this.isAlive) {
            //Turkey.count--;
            //Turkey.adultCount--;
            this.isAlive = false;
        } else if (this.age > Turkey.ADULT_AGE && !this.isAdult) {
            this.isAdult = true;
            Turkey.adultCount++;
            Turkey.nonAdultCount--;
        }
    }

    public static void slaughter() {
        // increments a counter that keeps track of how many turkeys have been
        // killed.
        Turkey.numSlaughtered++;
    }

    public boolean isPaired() {
        // return true if the turkey pairID is != 0;
        return this.pairedWithID != 0;
    }

    public static void mate(Turkey t1, Turkey t2) {
        // Always set female to pregnant
        // Determine which of the two Turkey's is female and make pregnant and
        // set days pregnant to zero
        if (!t1.hasYChromo) {
            t1.isPregnant = true;
            t1.daysPregnant = 0;
        } else if (!t1.hasYChromo) {
            t1.isPregnant = true;
            t1.daysPregnant = 0;
        }
    }

    private void setDaysPregnant(int days) {
        this.daysPregnant = days;
    }

    public static void pairTurkeys(Turkey t1, Turkey t2) {
        // This method will set pair ID of each other
        Turkey.pairedCount += 2;
        Turkey.unpairedCount -= 2;
        t1.pairedWithID = t2.id;
        t2.pairedWithID = t1.id;
    }

    private void setPairIDTo(int id) {
        this.pairedWithID = id;
    }

    public boolean getGender() {
        return this.hasYChromo;
    }

    public boolean isPregnancyDue() {
        // This method will return true if the daysPregnant is greater than
        // PREGNANCYDURATION

        return this.daysPregnant > Turkey.PREGNANCY_DURATION;
    }

    public void setIsPregnant(boolean p) {
        this.isPregnant = p;
    }

    // toString method that returns important information about a Turkey
    public String toString() {
        String result = String.format("ID:\t%s\nGender:\t%s\nAge:\t%s\n",
                this.id, this.hasYChromo ? "Male" : "Female", this.age);
        return result;
    }

    // static getters
    
    public static int getCount() {
        return Turkey.count;
    }

    public static int getAdultCount() {
        return Turkey.adultCount;
    }

    public static int getNonAdultCount() {
        return Turkey.nonAdultCount;
    }

    public static int getPairedCount() {
        return Turkey.pairedCount;
    }

    public static int getUnpairedCount() {
        return Turkey.unpairedCount;
    }

    public static int getSlaughteredCount() {
        return Turkey.numSlaughtered;
    }
}
