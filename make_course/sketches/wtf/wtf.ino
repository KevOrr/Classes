#include <TimerOne.h>

const byte anodes[8] = {2, 11, A3, 5, 9, A2, 8, A0};
const byte cathodes[8] = {10, 7, 6, 3, A1, 4, 12, 13};
const byte directions[3][2] = {{1, 1}, {1, 2}, {2 ,1}};
byte horizSpeed = 1;
byte vertSpeed = 1;
byte x = 0, y = 0;

volatile byte counter = 0;

void setup() {
  for (byte i=0; i<8; i++) {
    pinMode(anodes[i], OUTPUT);
    digitalWrite(anodes[i], LOW);
    
    pinMode(cathodes[i], OUTPUT);
    digitalWrite(cathodes[i], LOW);
  }

  Timer1.initialize(100);
  Timer1.attachInterrupt(blinkLed);

  Serial.begin(38400);
}

void loop() {
  if (x == 0 || x == 7 || y == 0 || y == 7)
    bounce();

  x += horizSpeed;
  if (x > 7) x = 7;
  else if (x < 0) x = 0;
  
  y += vertSpeed;
  if (y > 7) y = 7;
  else if (y < 0) y = 0;

  Serial.print(x); Serial.print(y); Serial.println(" ");
  /*Serial.print(horizSpeed); Serial.println(vertSpeed);*/

  delay(100);
}

void bounce() {
  byte i = random(3); // length of directions array
  horizSpeed = directions[i][0];
  vertSpeed = directions[i][1];
  if (x == 7)
    horizSpeed *= -1;
  if (y == 7)
    vertSpeed *= -1;
}

void blinkLed() {
  byte mode = counter & 1;
  digitalWrite(anodes[x], mode);
  digitalWrite(cathodes[y], !mode);
  counter++;
}
