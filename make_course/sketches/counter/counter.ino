byte counter;

void setup() {
  Serial.begin(9600);
  for(int i=2; i <= 9; i++){
    pinMode(i, OUTPUT);
    digitalWrite(i, LOW);
  }

}

void loop() {
  if (counter < 1) {
    counter = 1;
  }
  writeValue(counter);
  Serial.print(counter);
  Serial.print(" = 0b");
  Serial.println(counter, BIN);

  counter <<= 1;
  delay(70);
  
}

void writeValue(byte val) {
  for (int i=0; i < 8; i++) {
    if (bitRead(val, i) == 1) {
      digitalWrite(9-i, HIGH);
    } else {
      digitalWrite(9-i, LOW);
    }
  }
}

