// Note that this sketch works best when you change TWI_FREQ in twi.h
// from 100000L to 400000L. This puts the I2C bus in fastmode (400 kHz).
// On my machine, twi.h was in /usr/local/arduino-1.6.5-r5/hardware/arduino/avr/libraries/Wire/utility

#include <stdlib.h>
#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <AccelStepper.h>

#define A 6
#define B 7
#define C 8
#define D 9
#define POT_INPUT A0

#define REVOLUTION_STEPS 4096 // 4 coils * 8 phases * 64 reduction * 2 half-stepping
#define MAXSPEED 900.0 // ~13 rpm
#define POT_STEPPER_SPEED_RATIO MAXSPEED / 1024.0 // 10-bit ADC

LiquidCrystal_I2C myDisplay(0x27, 16, 2); // I2C 1602 LCD
AccelStepper stepper = AccelStepper(AccelStepper::HALF4WIRE, A, C, B, D); // 28BYJ-48

long int t;
float currSpeed;
float rpm;

void setup() {
  pinMode(POT_INPUT, INPUT);

  stepper.setMaxSpeed(MAXSPEED);
  stepper.setAcceleration(1000);
  stepper.moveTo(1000000000); // Essentially infinity, takes about 13 days to get there at 900 pps
  stepper.run();

  myDisplay.init();
  myDisplay.backlight();
  myDisplay.setCursor(6,0);
  myDisplay.print("rpm");

  t = millis();
}

void loop() {
  stepper.run();
  if (millis() - t >= 500) {
    t = millis();
    checkPot();
    writeLcd();
  }
}

void checkPot() {
  int analog = analogRead(POT_INPUT);
  currSpeed = analog * POT_STEPPER_SPEED_RATIO;
  rpm = currSpeed * 60 / REVOLUTION_STEPS; // pps -> ppm -> rpm

  stepper.setMaxSpeed(currSpeed);
  stepper.run();
}

void writeLcd() {
  // Convert rpm -> str
  char *str = new char[7];
  dtostrf(rpm, 5, 2, str); // width 5, precision 2
  stepper.run();

  // print rpm one char at a time, each time call stepper.run()
  myDisplay.setCursor(0,0);
  stepper.run();
  for (int i=0; i<7; i++) {
    if (*(str+i) == '\0') break;
    myDisplay.print(*(str+i));
    stepper.run();
  }

  // cleanup
  delete str;
}

