#include "HCSR04.h"
#include "Arduino.h"

HCSR04ProxSensor::HCSR04ProxSensor(int echoPin, int trigPin) {
  _trigPin = trigPin; // Pin connected to HCSR04 TRIG
  _echoPin = echoPin; // Pin connected to HCSR04 ECHO

  pinMode(trigPin, OUTPUT); // TRIG sends a pulse
  pinMode(echoPin, INPUT);  // ECHO listens for a reply

  readSensor(); readSensor();
}

float HCSR04ProxSensor::readSensor() {
  _lastValue = _currentValue; // _currentValue will be old after this function executes, move to _lastValue

  // Send 10 us pulse on ECHO
  digitalWrite(_trigPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(_trigPin, LOW);

  
  return _currentValue = pulseIn(_echoPin, HIGH) / ECHO_TIME_TO_CM;

}

//std::function<float()> readSensorCentimeters = readSensor;
float HCSR04ProxSensor::readSensorCentimeters() {
  return readSensor();
}

float HCSR04ProxSensor::readSensorInches() {
  return readSensor() / CENT_IN_INCH;
}

float HCSR04ProxSensor::getLastValue() {
  return _lastValue;
}

