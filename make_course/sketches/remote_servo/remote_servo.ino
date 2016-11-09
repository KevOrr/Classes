#include <Servo.h>
#include <IRremote.h>

#define IR_PIN 11
#define SERVO_PIN 12

#define LEFT 16743045
#define RIGHT 16724175
#define CENTER 16718055
#define STEP_RIGHT 16769055
#define STEP_LEFT 16754775

Servo servo;
IRrecv irrecv(IR_PIN);
decode_results results;

void setup() {
  Serial.begin(9600);
  irrecv.enableIRIn();

  servo.attach(SERVO_PIN);
}

void loop() {
  if (irrecv.decode(&results)) {
    Serial.println(results.value, DEC);
    switch (results.value) {
      case LEFT:   servo.write(0);   break;
      case RIGHT:  servo.write(180); break;
      case CENTER: servo.write(90);  break;
      case STEP_RIGHT: servo.write(servo.read() + 1); break;
      case STEP_LEFT:  servo.write(servo.read() - 1); break;
    }
    irrecv.resume();
  }
}

