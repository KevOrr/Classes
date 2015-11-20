/*
Program:    IntList.java
Student:    Kevin Orr
Desc:       An (unsorted) integer list class with a method to add an
            integer to the list and a toString method that returns the contents
            of the list with indices.
*/

public class IntList
{
   private int[] list;
   private int numElements = 0;
   //-------------------------------------------------------------
   // Constructor -- creates an integer list of a given size.
   //-------------------------------------------------------------
   public IntList(int size)
   {
      list = new int[size];
   }
   //------------------------------------------------------------
   // Adds an integer to the list. If the list is full,
   // prints a message and does nothing.
   //------------------------------------------------------------
   public void add(int value)
   {
      if (numElements == list.length)
      {
          System.out.println("Can't add, list is full");
          return;
      }
      
      // Else place the new value in the correctly sorted position
      for (int i=0; i < Math.max(numElements+1, 1); i++)
      {
          if (list[i] == 0)
          {
              // Got to the end, simply place the value in the next slot
              list[i] = value;
              break;
          }
          else if (value <= list[i])
          {
              // Start immediately after the end of effective list, pulling values from left 
              for (int j=numElements; j>i; j--)
              {
                  list[j] = list[j-1];
              }

              // Place value in the new opened slot
              list[i] = value;
              break;
          }
      }
      numElements++;
   }
   
   //-------------------------------------------------------------
   // Returns a string containing the elements of the list with their
   // indices.
   //-------------------------------------------------------------
   public String toString()
   {
      String returnString = "";
      for (int i=0; i<numElements; i++)
         returnString += i + ": " + list[i] + "\n";
      return returnString;
   }
}