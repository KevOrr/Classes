/*
Program:    Ghost.java
Student:    Kevin Orr
Desc:       Defines a Ghost
*/

public class Ghost {
    //These values are not positive. They MUST be negative!
    private int hunger = 0;
    private int spookiness = 0;

    //Constructor
    public Ghost(){
        System.out.println("The world has given birth to a new Ghost!");
    }
    
    
    //scare: decrease the spookieness level by 3 and set it to 0 if it is greater than 0. . 
    public void scare(){
        this.spookiness = Math.min(this.spookiness + 3, 0);
    }
    
    //getSpookiness: return the spookines level
    public int getSpookiness(){
        return spookiness;
    }
    
    //getHunger: return the hunger level;
    public int getHunger() {
        return this.hunger;
    }

    //getCandy: return the 
    public int getCandy() {
        return this.hunger;
    }
    
    //getMood: return the mood as the difference of the hunger
    //level and the spookiness level
    public int getMood(){
        return hunger + spookiness;//This will be a negative number
    }
    
    //passTime: increase the hunger level and the spookiness
    //level by 1

    public void passTime() {
        hunger--;       //won't go larger than 0.
        spookiness--;   //won't go larger than 0.
    }
    
    //eatCandy: increase the hunger level by 3 and set it to 0 if
    //it is greater than 0.
    public void eatCandy(){
        System.out.println("MMMMMMmmmmmmmmmmm.........");
        this.hunger = Math.min(this.hunger + 3, 0);
    }

    public void speakWithTheSpirits(){
        String moodStr = "I feel ";
        int mood = getMood();
        if (mood < -15) {
            moodStr += "vengeful";
        } else if (mood < -10) {
            moodStr += "whistful";
        } else if (mood < -5) {
            moodStr += "spooky";
        } else {
            moodStr += "friendly"; 
        }
        System.out.println(moodStr);
    }
}
