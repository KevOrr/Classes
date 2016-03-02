#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <AccelStepper.h>

#define DEBUG true

#define A 6
#define B 7
#define C 8
#define D 9
#define POT_INPUT A0

#define REVOLUTION_STEPS 4096
#define MAXSPEED 800.0 // ~11.7 rpm
#define POT_STEPPER_SPEED_RATIO MAXSPEED / 1024.0

LiquidCrystal_I2C myDisplay(0x27, 16, 2);
AccelStepper stepper = AccelStepper(AccelStepper::HALF4WIRE, A, C, B, D);

void setup() {
  pinMode(POT_INPUT, INPUT);

  stepper.setMaxSpeed(0);
  stepper.setAcceleration(500);

  myDisplay.init();
  myDisplay.backlight();

  if (DEBUG) Serial.begin(115200);
}

void loop() {
  int analog = analogRead(POT_INPUT);
  if (DEBUG) {Serial.print("analog: "); Serial.print(analog);}

  float currSpeed = analog * POT_STEPPER_SPEED_RATIO;
  if (DEBUG) {Serial.print("\tcurrSpeed: "); Serial.print(currSpeed);}

  float rpm = currSpeed * 60 / REVOLUTION_STEPS; // sps -> spm -> rpm
  if (DEBUG) {Serial.print("\trpm: "); Serial.print(rpm);}

  writeLcd(rpm);

  if (DEBUG) Serial.println();
  delay(200);
}

void writeLcd(float rpm) {
  myDisplay.setCursor(4,0);
  myDisplay.print(" "); // clear
  myDisplay.setCursor(0,0);
  myDisplay.print(rpm);
  myDisplay.setCursor(6,0);
  myDisplay.print("rpm");
}

int getPadding(double x) {
  if      (x>100) return 0;
  else if (x>10)  return 1;
  else            return 2;
}

