#include <AccelStepper.h>
#include <TimerOne.h>

#define DEBUG 1 // set to 0 if you dont want the ATMega to send debug info

#define GEAR_RATIO      2 // 2 planet column revolutions for 1 stepper revolution
#define STEPS_PER_STEPPER_REV 4096 // 4096 half-steps per stepper revolution
#define STEPS_PER_REV   STEPS_PER_STEPPER_REV * GEAR_RATIO
#define MAXSPEED        600     // max stepper speed in half-steps/sec
#define MAXACCEL        1000    // max stepper acceleration in half-steps/sec^2
#define N               3       // number of planets

AccelStepper *steppers[3]; // pointer array to each plannet
AccelStepper mercury = AccelStepper(AccelStepper::HALF4WIRE, A2, A4, A3, A5); // 28BYJ-48
AccelStepper venus = AccelStepper(AccelStepper::HALF4WIRE, 9, 11, 10, 12); // 28BYJ-48
AccelStepper earth = AccelStepper(AccelStepper::HALF4WIRE, 5, 7, 6, 8); // 28BYJ-48

char buf[101] = {0}; // Temporary input buffer
uint8_t i = 0; // input buffer char index

void setup() {
  Serial.begin(38400); // this is what the HC-05 UART baud rate is set to

  // Store pointers to the steppers to easily pass to functions, iterate over, etc
  steppers[0] = &mercury;
  steppers[1] = &venus;
  steppers[2] = &earth;

  // Set initial maxSpeed and acceleration on each of the steppers
  for (int i=0; i<N; i++) {
    steppers[i]->setMaxSpeed(MAXSPEED);
    steppers[i]->setAcceleration(MAXACCEL);
  }

  // stepper.run must be called very often, but at least once per step cycle
  Timer1.initialize(100); // once every 100 microseconds
  Timer1.attachInterrupt(runSteppers);

  // Print column headers
  if (DEBUG) Serial.println(F("Stepper    Pos   Dist     Target"));
}

void loop() {

  while (Serial.available()) { // There's input, let's read it
    char c = Serial.read(); // Get one character

    if (c == '\r')
      continue; // discard carriage returns
    else if ((c != '\n') && (i < sizeof(buf) - 1)) // avoid buffer overflow
      buf[i++] = c; // store anything else
    else if (c == '\n') { // end of command
      buf[i] = '\0'; // Terminate with NUL
      i = 0; // Begin at start of buf next time

      // tokenize string, get targets for each stepper
      char *tok = strtok(buf, " ");
      int targets[N];
      for (int i=0; tok && i<3; i++) {
        targets[i] = STEPS_PER_REV * atof(tok) / 360;
        tok = strtok(NULL, " ");
      }

      moveSteppers(targets);
    } else { // buffer must have overflown
      if (DEBUG) Serial.println("Buffer overflow!");
      i = 0; // ignore the command
    }
  }
}

// http://mathb.in/56432
void moveSteppers(int targets[]) {
  int step_disp[N] = {0}; // steps for each stepper to travel
  int maxDistance = 0; // longest movement for all of the steppers

  for (int i=0; i<N; i++) {
    int pos = steppers[i]->currentPosition();
    pos %= STEPS_PER_REV; // reset position to mod(currPos, STEPS_PER_REV)

    // Calculate distance to each target
    targets[i] %= STEPS_PER_REV; // make sure each target is in 0..STEPS_PER_REV
    int dist = targets[i] - pos;
    if (abs(STEPS_PER_REV - dist) < abs(dist))
      dist = dist - STEPS_PER_REV; // if going the other way is closer, do it
    step_disp[i] = dist;

    // Find maximum distance
    if (abs(dist) > maxDistance)
      maxDistance = abs(dist);
  }

  for (int i=0; i<N; i++) {
    AccelStepper *stepper = steppers[i]; // get current stepper

    int pos = steppers[i]->currentPosition();
    pos %= STEPS_PER_REV;

    // v_i = v_max * d_i / d_max
    float velocity = MAXSPEED * (float) abs(step_disp[i]) / (float) maxDistance;
    stepper-> setMaxSpeed(velocity);

    // a_i = a_max * v_i / v_max
    stepper-> setAcceleration(MAXACCEL * velocity / (float) MAXSPEED);

    stepper-> setCurrentPosition(pos);
    stepper-> move(step_disp[i]);

    // Send debug info
    if (DEBUG) {
      char str[101];
      sprintf(str, "Stepper %1d: @%-4d %+-5d -> %-4d\r\n", i, pos, step_disp[i], targets[i]);
      Serial.print(str);
    }
  }
}

// Check each stepper if a step is needed. If so, move by one step
void runSteppers() {
  for (int i=0; i<N; i++)
    steppers[i] -> run();
}
