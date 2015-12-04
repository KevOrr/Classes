/* Program:    Car.java
 * Student:    Kevin Orr
 * Desc:       
 */

package labtest2;

import java.util.Random;

public class Car {

    private int type;
    private double price = (new Random().nextInt(2500000) + 5000) / 100.0;

    public Car(int type) {
        this.type = type;
    }

    public int getType() {
        return this.type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public double getPrice() {
        return this.price;
    }

    public void setPrice(double price) {
        this.price = price;
    }

    @Override
    public String toString() {
        return String.format("type %d cost: $,.2f", this.type, this.price);
    }

}
