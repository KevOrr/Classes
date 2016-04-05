//#include <StandardCplusplus.h>
//#include <vector>
#include <AccelStepper.h>

#define DEBUG 1

#define STEPS_PER_REV 4096
#define MAXSPEED      1000    // pulse/sec
#define MAXACCEL      1000    // pulse/sec^2
#define N             3       // number of planets

AccelStepper *steppers[3];
AccelStepper mercury = AccelStepper(AccelStepper::HALF4WIRE, 1, 3, 2, 4); // 28BYJ-48
AccelStepper venus = AccelStepper(AccelStepper::HALF4WIRE, 1, 3, 2, 4); // 28BYJ-48
AccelStepper earth = AccelStepper(AccelStepper::HALF4WIRE, 1, 3, 2, 4); // 28BYJ-48

void setup() {
  steppers[0] = &mercury;
  steppers[1] = &venus;
  steppers[2] = &earth;
}

void loop() { }

void checkCommands() { }

void moveSteppers(AccelStepper *steppers[], int targets[]) {
  int step_disp[N] = {0}; // steps for each stepper to travel
  int maxDistance = 0; // longest movement for all of the steppers

  for (int i=0; i<N; i++) {
    int dist = mod(steppers[i]->currentPosition() - targets[i], STEPS_PER_REV);
    if (STEPS_PER_REV - dist < dist)
      dist *= -1;
    step_disp[i] = dist;

    if (abs(dist) > maxDistance)
      maxDistance = abs(dist);
  }

  for (int i=0; i<N; i++) {
    AccelStepper *stepper = steppers[i];
    stepper-> setMaxSpeed(MAXSPEED * abs(step_disp[i]) / maxDistance);
    stepper-> setAcceleration(MAXACCEL * abs(step_disp[i]) * maxDistance); // TODO Not 100% correct, figure out accel calculations
    stepper-> setCurrentPosition(mod(stepper-> currentPosition(), STEPS_PER_REV));
    stepper-> moveTo(targets[i]);
    stepper-> run();
  }
}

// http://stackoverflow.com/a/12089637/1529586
inline int mod(int a, int b) {
  const int result = a % b;
  return result >= 0 ? result : result + b;
}

