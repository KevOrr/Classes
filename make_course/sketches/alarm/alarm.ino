#define TRIGGER1 12
#define TRIGGER2 11
#define RESET 10
#define LED_BAD 4
#define LED_GOOD 3

int alarm = false;

void setup() {
  digitalWrite(13, LOW);

  digitalWrite(TRIGGER1, HIGH);
  digitalWrite(TRIGGER2, HIGH);
  digitalWrite(RESET, HIGH);
}

void loop() {
  alarm = (alarm || !digitalRead(TRIGGER1) && !digitalRead(TRIGGER2)) && digitalRead(RESET);
  pinMode(LED_GOOD, !alarm);
  pinMode(LED_BAD, alarm);
}

