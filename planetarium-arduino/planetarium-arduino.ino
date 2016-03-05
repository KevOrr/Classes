#include <AccelStepper.h>

#define STEPS_PER_REV 4096
#define MAXSPEED      1000    // pulse/sec
#define MAXACCEL      1000    // pulse/sec^2
#define N             3       // number of planets

void setup() { }

void loop() { }

void checkCommands() { }

void moveSteppers(AccelStepper *steppers[], int targets[]) {
  int dists[N] = {0};
  int maxDistance = 0;
  for (int i=0; i<N; i++) {
    int dist = abs( (steppers[i]->currentPosition() % STEPS_PER_REV) - (targets[i] % STEPS_PER_REV) );
    dists[i] = dist;

    if (dist > STEPS_PER_REV/2)
      dist = STEPS_PER_REV - dist;
    if (dist > maxDistance)
      maxDistance = dist;
  }

  for (int i=0; i<N; i++) {
    AccelStepper *stepper = steppers[i];
    stepper-> setMaxSpeed(MAXSPEED * dists[i] * maxDistance);
    stepper-> setAcceleration(MAXACCEL * dists[i] * maxDistance); // Not 100% correct, maybe 60% instead
    stepper-> setCurrentPosition(stepper-> currentPosition() % STEPS_PER_REV);
    stepper-> moveTo(targets[i]);
    stepper-> run();
  }
}

