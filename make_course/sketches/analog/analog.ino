#define PWM_OUT 3
#define ANALOG_IN A0
#define PERIOD 5000000

void setup() {
  digitalWrite(13, LOW);
  pinMode(PWM_OUT, OUTPUT);

  Serial.begin(115200);
}

void loop() {
  for (int i=0; i<256; i++) {
    analogWrite(PWM_OUT, i);
    Serial.println(analogRead(ANALOG_IN) / (1024.0 / 5.0));
    delayMicroseconds(PERIOD / 256);
  }
}

