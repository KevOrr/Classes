package Lab08;

import java.util.ArrayList;
import java.util.Random;

import Lab08.Turkey;

public class TurkeyFarm {

    public static void main(String[] args) {
        ArrayList<Turkey> farm = new ArrayList<Turkey>();
        Random gen = new Random(System.currentTimeMillis());
        
        //Five turkeys may not always have both genders
        int startTurkeys = /* gen.nextInt(100) + */ 5;
        for (int i = 0; i < startTurkeys; i++) {
            farm.add(new Turkey());
        }

        int daysMax = 365;
        int days = 0;
        while (days <= daysMax) {// for each day
            System.out.println("Day:" + days);
            for (Turkey t1 : farm) {

                // check if adult{
                if (t1.isAdult()) {
                    // if paired & not pregnant call mate
                    if (!t1.isPregnant() && t1.isPaired()) {
                        for (Turkey t2 : farm) {
                            // find a turkey's pair and mate
                            // The turkey is only paired with the opposite
                            // gender
                            if (t1.getID() == t2.getPairID()) {
                                Turkey.mate(t1, t2);
                            }
                        }
                    }
                    // if not paired pair up with a free turkey of the opposite
                    // gender
                    if (!t1.isPaired())
                        for (Turkey t2 : farm) {// find a turkey to pair up with
                            if (!t2.isPaired() && t1.getGender() != t2.getGender())
                                Turkey.pairTurkeys(t1, t2);
                        }
                }
            }
            for (int j = farm.size() - 1; j >= 0; j--) {
                Turkey t1 = farm.get(j);
                // if pregnant give birth to new turkeys?
                if (t1.isPregnant() && t1.isPregnancyDue()) {
                    int x = gen.nextInt(3) + 1;
                    for (int i = 0; i <= x; i++)
                        farm.add(Turkey.birth(t1));
                }

            } // end check if adult

            for (int i = farm.size() - 1; i >= 0; i--) {
                Turkey t1 = farm.get(i);
                // is too old? Time to die
                if (t1.istimeToDie()) {
                    // Unpair other turkey if need be
                    for (Turkey t2 : farm) {
                        if (t2.getPairID() == t1.getID()) {
                            //We unpair both to keep the counts of those
                            //paired and unpaired accurate.
                            t1.unPair();
                            t2.unPair();
                        }
                    }
                    Turkey.slaughter();
                    farm.remove(t1);
                }

                // PassTime Method
                t1.passTime();

            } // end for

            // Status of the Farm. These should be static method calls
            // Get & Print total number of Turkeys;
            System.out.println("Total turkey count: " + Turkey.getCount());
            // Get & Print total number of adult Turkeys
            System.out.println("Adult count: " + Turkey.getAdultCount());
            // Get & Print total number of notAdult Turkeys
            System.out.println("Non-Adult Count: " + Turkey.getNonAdultCount());
            // Get & Print total number of Paired Turkeys
            System.out.println("Paired Count: " + Turkey.getPairedCount());
            // Get & Print total number of NotPaired Turkeys
            System.out.println("Unpaired count: " + Turkey.getUnpairedCount());
            System.out.println();
                        
            days++;
        } // end While loop

        System.out.println(farm.size());

        // Display all the IDs of the turkeys left
        // If the toString method is not overridden we memory addresses
        // not the details of our turkeys!
        for (Turkey t1 : farm){
            System.out.println(t1);
        }

    }
}