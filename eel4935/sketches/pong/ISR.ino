// Source: http://makecourse.weebly.com/week10segment2.html

//Interrupt Service Routine "ISR". This function is called everytime the time interrupt is active.
//This routine scans through the pattern[][] array  and whenever it finds a 1 it flashes the appropriate LED.

void refreshScreen() {

      if (board[c][r] && flag == 0)//we have two ISR cycles per LED position. In the first one (flag=0) we turn the LED on (if there is a 1 in the pattern[][] array
      {                                 //in the second (flag=1) we turn it off
        digitalWrite(anodes[r], HIGH);//turn pixel on
        digitalWrite(cathodes[c], LOW);

      }
      else
      {
        digitalWrite(anodes[r], LOW);//turn pixel off
        digitalWrite(cathodes[c], HIGH);
       }
       counter++;//this counter drives the scanning through the 8x8 array and the on/off cycles. In total we have 8x8x2=128 states of the display, i.e. we use the first
                 //7 bits of the counter. The first one selects on/off, the next three count the rows, and 4-7 count the columns. The on/off bit is flipped during
                 //every cycle, the row bits are counted up during every 2nd cycle, while the column bits are counted up everytime the row bits lapse back to zero (i.e
                 //after we completed a row, the next column is selected.
                 //the 0bxxxxxxxx & counter operations select the relevant bits, and the xxxxxxxx >> y operations shift the bits all the way to the right that they can
                 //be used as decimal row or column numbers
       flag = 0b00000001 & counter;//the lsb of the counter selects between on and off cycles. Every on cycle (flag=0) needs to be followed by a off cycle (flag=1) to flash 
                                   //the LED before counting to the next LED position
       r = (0b00001110 & counter)>>1;//this shifts the four least significant bits of the counter one to the right (i.e. divides by 2)
                                     //this results in counting the row r up by one only every second time the counter is counted up
                                     //this enables to first turn the LED on and then off, before moving on to the next position in the array
       c = (0b01110000 & counter)>>4;//here we use bits 5,6,7 as row counter. '>>4' divides by 8 to scale the number down to 0-7 range
                                     //which is suitable as column counter
                                     //the columns are only counted up by one whenever one row 0->7 cycle is completed

}
