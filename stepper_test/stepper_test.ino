#include <AccelStepper.h>

// I'm using a ULN2003 to drive a 24BYJ-48

#define A 6
#define B 7
#define C 8
#define D 9

#define REV_STEPS 4096 // 4 coils * 8 phases * 64 reduction * 2 halfstepping
#define GEAR_RATIO 2
#define MAXSPEED 500
#define ACCEL 1000

AccelStepper stepper = AccelStepper(AccelStepper::HALF4WIRE, A, C, B, D);

void setup() {
  stepper.setMaxSpeed(MAXSPEED);
  stepper.setAcceleration(ACCEL);
}

void loop() {
  stepper.moveTo(REV_STEPS * GEAR_RATIO);
  stepper.runToPosition(); // blocks
  stepper.moveTo(0);
  stepper.runToPosition();
}

