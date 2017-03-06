#include <AccelStepper.h>

// I'm using a ULN2003 to drive a 24BYJ-48

#define A A5
#define B A4
#define C A3
#define D A2
#define INDICATOR 2

#define REV_STEPS 4096 // 4 coils * 8 phases * 64 reduction * 2 halfstepping
#define GEAR_RATIO 2
#define MAXSPEED 700
#define ACCEL 1000

AccelStepper stepper = AccelStepper(AccelStepper::HALF4WIRE, A, C, B, D);
char buf[256] = {0};
uint16_t i = 0;
long int target = 0;

void moveStepper();

void setup() {
  Serial.begin(38400); // My HC-05 Bluetooth Module runs at 38400 baud
  stepper.setMaxSpeed(MAXSPEED);
  stepper.setAcceleration(ACCEL);
  stepper.move(3600); // Just an initial "I'm on and working!" test
  stepper.run(); // Begin movement
}

void loop() {
  while (Serial.available()) {
    digitalWrite(INDICATOR, HIGH); // On whenever receiving data

    char c = Serial.read();

    if (c == '\r') continue; // discard carriage returns
    buf[i++] = c; // store anything else

    if ((c == '\n') || (i == sizeof(buf) - 1)) {
      digitalWrite(INDICATOR, LOW); // EOL, not receiving anymore
      buf[i] = '\0'; // Terminate with NUL
      i = 0; // Begin at start of buf next time

      // convert degrees -> steps, then move
      target = GEAR_RATIO * REV_STEPS * atof(buf) / 360;
      moveStepper();
    }
  }

  stepper.run();
}

void moveStepper() {
  stepper.moveTo(target);
  stepper.run();
}

