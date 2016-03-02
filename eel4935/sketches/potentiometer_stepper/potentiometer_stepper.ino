#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <AccelStepper.h>

#define DEBUG false

#define A 6
#define B 7
#define C 8
#define D 9
#define POT_INPUT A0

#define REVOLUTION_STEPS 4096
#define MAXSPEED 600.0 // ~8.8 rpm
#define POT_STEPPER_SPEED_RATIO MAXSPEED / 1024.0 // 10-bit ADC

LiquidCrystal_I2C myDisplay(0x27, 16, 2);
AccelStepper stepper = AccelStepper(AccelStepper::HALF4WIRE, A, C, B, D);

int iter;
float currSpeed;
float rpm;

void setup() {
  pinMode(POT_INPUT, INPUT);

  stepper.setMaxSpeed(1500);
  stepper.setAcceleration(1000);
  stepper.moveTo(1000000000);
  stepper.run();

  myDisplay.init();
  myDisplay.backlight();
  myDisplay.setCursor(4,0);
  myDisplay.print(" rpm");

  iter = 0;
}

void loop() {
  stepper.run();
  if (++iter == 25000) {
    iter = 0;
    checkPot();
    stepper.run();
    flashLcd();
  }
}

void checkPot() {
  int analog = analogRead(POT_INPUT);
  currSpeed = analog * POT_STEPPER_SPEED_RATIO;
  rpm = currSpeed * 60 / REVOLUTION_STEPS; // sps -> spm -> rpm

  stepper.setMaxSpeed(currSpeed);
  stepper.run();
}

void flashLcd() {
  myDisplay.setCursor(0,0);
  stepper.run();
  myDisplay.print(int(rpm));
  stepper.run();
  myDisplay.print(".");
  stepper.run();
  myDisplay.print(int((rpm - int(rpm)) * 100));
  stepper.run();
}

