package grouplab1;

// **********************************************************
//   DNA.java      Michael Palermo  and Kevin Orr
//
// Reads several files in directory and finds the
// %G~C content of text within
//
// **********************************************************

import java.util.Scanner;
import java.text.NumberFormat;
import java.util.Scanner;
import java.io.*;

public class GCContent{
  public static void main(String[] args) throws IOException {
        // Housekeeping Promoters
        System.out.println("Housekeeping Promoters");
        System.out.print("Heat Shock Protein 90 (HSP90)");
        getGCFreq("HSP90.txt");
        System.out.println();

        System.out.println("Glucose-6-phosphate Dehydrogenase (G6PD)");
        getGCFreq("G6PD.txt");
        System.out.println();

        System.out.println("Beta-actin (ACTB)");
        getGCFreq("ACTB.txt");
        System.out.println();

        System.out.println();

        // Tissue Specific Promoters
        System.out.println("Tissue Specific Promoters");
        System.out.println("Bone Morphogenetic Protein 5 (BMP5)");
        getGCFreq("BMP5.txt");
        System.out.println();

        System.out.println("Hemoglobin Beta (HBB)");
        getGCFreq("HBB.txt");
        System.out.println();

        System.out.println("GABA Receptor A1 (GABRA1)");
        getGCFreq("GABRA1.txt");
    }

  public static void getGCFreq (String file) throws IOException
  {
   //variables
   String sequence = "";
   int  currentLength = 0;
   int length, G = 0, C = 0, T = 0, A = 0;
   float gcContent;

   NumberFormat fmt = NumberFormat.getPercentInstance();
   fmt.setMinimumFractionDigits(3);


   Scanner scan = new Scanner (new File(file));
   //prompt user
   while (scan.hasNext()) {
     sequence += scan.nextLine();
   }
   //calculate string length
   length = sequence.length();

   //calculate amounts of G, C, A, & T
   while (length > currentLength){
      if (sequence.charAt(currentLength) == 'G'){
         G = G + 1;
      }
      if (sequence.charAt(currentLength) == 'C'){
         C = C + 1;
      }
      if (sequence.charAt(currentLength) == 'A'){
         A = A + 1;
      }
      if (sequence.charAt(currentLength) == 'T'){
         T = T + 1;
      }
      if (sequence.charAt(currentLength) != 'G' && sequence.charAt(currentLength) != 'C'
      && sequence.charAt(currentLength) != 'A' && sequence.charAt(currentLength) != 'T' ){
         System.out.println("WARNING: character detected"
          + " that was not G, C, A, or T.");
      }
      currentLength = currentLength + 1;

   }
   gcContent = (float) (G + C) / length;

   //print results
   System.out.println("A: " + A);
   System.out.println("T: " + T);
   System.out.println("C: " + C);
   System.out.println("G: " + G);
   System.out.println("Total:" + length);
   System.out.println("The %G~C content is " + fmt.format(gcContent));
  }
}
