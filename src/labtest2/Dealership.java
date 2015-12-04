/* Program:    NorthAmericanDealerships.java
 * Student:    Kevin Orr
 * Desc:       
 */

package labtest2;

import java.util.ArrayList;
import java.util.Random;
import java.util.AbstractList;

public class Dealership {

    public final static int NUM_MODELS = 5;
    private static int dealershipCount = 0;
    static Random gen = new Random(System.currentTimeMillis());

    private String name;
    private int id = Dealership.dealershipCount;
    private int salesCount = 0;
    private int[] modelCounts = new int[NUM_MODELS];
    private ArrayList<Car> carsSold = new ArrayList<Car>();

    public Dealership(String name) {
        this.name = name;
    }

    // Create a car with random id (0..NUM_MODELS)
    public void sellACar() {
        int type = gen.nextInt(Dealership.NUM_MODELS); //0 to 5
        this.carsSold.add(new Car(type));
        this.modelCounts[type]++;
        this.salesCount++;
    }

    // Gets the total cost of all sales by this Dealer
    public double getTotalSales() {
        double total = 0.0;
        for (Car car : this.carsSold) {
            total += car.getPrice();
        }
        return total;
    }

    // Gets the average selling price of all cars sold by this Dealership
    public double getAveragePrice() {
        return this.getTotalSales() / this.salesCount;
    }

    // Get the model id of the dealership's best seller
    public int getMostCommonModel() {
        int id = -1;
        int max = 0;
        for (int i=0; i < Dealership.NUM_MODELS; i++) {
            if (max < this.modelCounts[i]) {
                max = modelCounts[i];
                id = i;
            }
        }
        return id;
    }

    @Override
    public String toString() {
        return String.format(
                "#%d: %s, cars sold: %d, total sales $%,.2f, average cost $%,.2f, bestseller model: %d",
                this.id, this.name, this.getTotalSales(), this.getAveragePrice(),
                this.getMostCommonModel()
        );
    }

    public String toStringSoldCars() {
        String res = "";
        for (Car car : this.carsSold) {
            res += car.toString() + "\n";
        }
        return res;
    }
}
