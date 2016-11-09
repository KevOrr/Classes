#define led 12
#define button 11

void setup() {
  pinMode(13, OUTPUT);
  digitalWrite(13, LOW);

  pinMode(led, OUTPUT);
  pinMode(button, INPUT);
  digitalWrite(button, HIGH);
}

void loop() {
  digitalWrite(led, 1 - digitalRead(button));
}

