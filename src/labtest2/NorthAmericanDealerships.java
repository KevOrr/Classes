/* Program:    NorthAmericanDealerships.java
 * Student:    Kevin Orr
 * Desc:       Runs a weeklong simulation of three dealers
 */

package labtest2;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Random;

public class NorthAmericanDealerships {
    public static void main(String[] args) {
        int max = 7;
        int day = 0;

        Random gen = new Random(System.currentTimeMillis());
        double sellRate = .30;

        ArrayList<Dealership> dealers = new ArrayList<Dealership>();

        dealers.add(new Dealership("Brandon Ford"));
        dealers.add(new Dealership("Tampa Honda"));
        dealers.add(new Dealership("Hillsboro Auto Mart, Inc."));

        while(day < max){
            for(int i = 0; i < dealers.size(); i++){
                if(gen.nextDouble() < sellRate){
                    dealers.get(i).sellACar();
                }
            }
            day++;
        }

        // Get average sales from all dealers
        double average = 0.0;
        for (Dealership dealer : dealers) {
            average += dealer.getTotalSales();
        }
        average /= dealers.size();

        NumberFormat f = NumberFormat.getCurrencyInstance();
        System.out.println("Dealership Totals Average " + f.format(average)+"\n");

        //print each dealership
        for(int i = 0; i < dealers.size(); i++){
            System.out.println(dealers.get(i));
            System.out.println(dealers.get(i).toStringSoldCars());
        }
    }
}
