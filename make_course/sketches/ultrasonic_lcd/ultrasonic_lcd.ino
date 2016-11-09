#include <Wire.h>
#include <LiquidCrystal_I2C.h>

#define TRIG A0
#define ECHO A1
#define DEBUG true

double distances[5] = {0};
LiquidCrystal_I2C myDisplay(0x27, 16, 2);

void setup() {
  pinMode(13, OUTPUT); digitalWrite(13, LOW);

  pinMode(TRIG, OUTPUT);
  pinMode(ECHO, INPUT);

  if (DEBUG) Serial.begin(230400);

  myDisplay.init();
  myDisplay.backlight();
}

void loop() {
  //double distance = getRollingAverage();
  double centis = getDistance();
  if (DEBUG) Serial.println(centis);

  myDisplay.setCursor(0,0);
  myDisplay.print("        "); // clear
  myDisplay.setCursor(getPadding(centis), 0);
  myDisplay.print(centis);
  myDisplay.setCursor(7,0);
  myDisplay.print(" cm      ");

  double inches = centis / 2.54;
  myDisplay.setCursor(0,1);
  myDisplay.print("        "); // clear
  myDisplay.setCursor(getPadding(inches), 1);
  myDisplay.print(inches);
  myDisplay.setCursor(7,1);
  myDisplay.print(" in      ");

  delay(250);
}

double getDistance() {

  digitalWrite(TRIG, HIGH);
  delayMicroseconds(10);
  digitalWrite(TRIG, LOW);

  int echo_time = pulseIn(ECHO, HIGH);

  return echo_time/58.0;
}

int getPadding(double x) {
  if      (x>100) return 0;
  else if (x>10)  return 1;
  else            return 2;
}

/*double getInsertDistance() {
  for (int i = 0; i < 4; i++)
    distances[i] = distances[i+1];
  return distances[5] = getDistance();
}*/

