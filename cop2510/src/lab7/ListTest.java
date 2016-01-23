package lab7;


// ***************************************************************
// ListTest.java
//
// A simple test program that creates an IntList, puts some
// ints in it, and prints the list.
//
// ***************************************************************
import java.util.Scanner;

public class ListTest
{
   public static void main(String[] args)
   {
        Scanner scan = new Scanner(System.in);;
      IntList myList = new IntList(10);
      int count = 0;
       int num;
       while (count < 10)
       {
         System.out.println("Please enter a number, enter 0 to quit:");
         num = scan.nextInt();
         if(num !=0)
         {  myList.add(num);
            count++;
         }
         else break;
      }
      System.out.println(myList);
   }
}