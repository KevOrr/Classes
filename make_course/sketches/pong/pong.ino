#include <TimerOne.h>

#define POT A5
#define AI 1

const byte anodes[8] = {2, 11, A3, 5, 9, A2, 8, A0};
const byte cathodes[8] = {10, 7, 6, 3, A1, 4, 12, 13};

const int8_t directions[][2] = {{1, 0}, {0, 1}, {1, 1}, {1, 2}, {2 ,1}};

float xSpeed, ySpeed;
int8_t xDir, yDir;
float x, y;
uint8_t board[8][8];

float paddleCenter = 3.5f;
uint8_t paddleLed = (uint8_t) paddleCenter;

volatile uint8_t c, r, flag, counter;

void reset() {
  randomSeed(analogRead(A4));

  xSpeed = 0.7, ySpeed = 0.7;
  xDir = 1, yDir = 1;
  x = 3.0f, y = 3.0f;
  c, r, flag, counter = 0;

  for (int i=0; i<8; i++)
    for (int j=0; j<8; j++)
      board[i][j] = 1;

  delay(250);

  for (int i=0; i<8; i++)
    for (int j=0; j<8; j++)
      board[i][j] = 0;

  delay(250);

  board[(int) x][(int) y] = 1;

  for (int i=paddleCenter-1; i<=paddleCenter+1; i++)
    board[(int) paddleCenter][7] = 1;
}

void setup() {
  for (byte i=0; i<8; i++) {
    pinMode(anodes[i], OUTPUT);
    digitalWrite(anodes[i], LOW);

    pinMode(cathodes[i], OUTPUT);
    digitalWrite(cathodes[i], HIGH);
  }

  Timer1.initialize(100);
  Timer1.attachInterrupt(refreshScreen);

  reset();
}

void loop() {
  if (y >= 7.99) {
    reset();
    return;
  }

  if (x == 0 || x >= 7.99 || y == 0)
    bounce();
  if (y >= 5.99 && x >= paddleCenter - 1.5 && x <= paddleCenter + 1.5)
    paddleBounce();

  noInterrupts();

  board[(int) x][(int) y]--;

  x += xSpeed;
  if (x >= 7.99) x = 7.99;
  else if (x < 0) x = 0;

  y += ySpeed;
  if (y >= 8) y = 7.99;
  else if (y < 0) y = 0;

  board[(int) x][(int) y]++;

  interrupts();

  board[(int) paddleCenter - 1][7] = 0;
  board[(int) paddleCenter][7] = 0;
  board[(int) paddleCenter + 1][7] = 0;

  if (AI)
    paddleCenter = min(max((int) x, 1), 6);
  else
    paddleCenter = map(analogRead(POT), 0, 1023, 1.5, 8 - 1.5);

  board[(int) paddleCenter - 1][7]++;
  board[(int) paddleCenter][7]++;
  board[(int) paddleCenter + 1][7]++;

  delay(AI ? 15 : 150);
}

void bounce() {
  if (x >= 7.99 || x == 0)
    xDir *= -1;
  else if (y >= 7.99 || y == 0)
    yDir *= -1;

  float azimuth = PI / 6 + ((float)rand() / (float)(RAND_MAX)) * PI / 6;
  xSpeed = xDir * cos(azimuth);
  ySpeed = yDir * sin(azimuth);
}

void paddleBounce() {
  float azimuth = PI / 6 + ((float)rand() / (float)(RAND_MAX)) * PI / 6;
  ySpeed = -yDir * sin(azimuth);
}
