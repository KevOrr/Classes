#include <TimerOne.h>

const byte anodes[8] = {2, 11, A3, 5, 9, A2, 8, A0};
const byte cathodes[8] = {10, 7, 6, 3, A1, 4, 12, 13};

const int8_t directions[][2] = {{1, 0}, {0, 1}, {1, 1}, {1, 2}, {2 ,1}};

float xSpeed = 0.7, ySpeed = 0.7;
int8_t xDir = 1, yDir = 1;
float x = 3.0f, y = 3.0f;
int8_t ledX = 3, ledY = 3;

volatile byte parity = 0;

void setup() {
  for (byte i=0; i<8; i++) {
    pinMode(anodes[i], OUTPUT);
    digitalWrite(anodes[i], LOW);
    
    pinMode(cathodes[i], OUTPUT);
    digitalWrite(cathodes[i], HIGH);
  }

  Timer1.initialize(100);
  Timer1.attachInterrupt(blinkLed);
}

void loop() {
  if (x == 0 || x >= 7.99 || y == 0 || y >= 7.99)
    bounce();

  noInterrupts();

  digitalWrite(anodes[ledX], LOW);
  digitalWrite(cathodes[ledY], HIGH);

  x += xSpeed;
  if (x >= 7.99) x = 7.99;
  else if (x < 0) x = 0;
  ledX = (int) x;
  
  y += ySpeed;
  if (y >= 8) y = 7.99;
  else if (y < 0) y = 0;
  ledY = (int) y;

  interrupts();

  delay(50);
}

void bounce() {
  if (x >= 7.99 || x == 0)
    xDir *= -1;
  else if (y >= 7.99 || y == 0)
    yDir *= -1;

  float azimuth = ((float)rand() / (float)(RAND_MAX)) * PI / 2;
  xSpeed = xDir * cos(azimuth);
  ySpeed = yDir * sin(azimuth);
}

void blinkLed() {
  if (parity = !parity) {
    digitalWrite(anodes[ledX], parity);
    digitalWrite(cathodes[ledY], !parity);
  }
}
