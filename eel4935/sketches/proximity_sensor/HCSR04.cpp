#include "HCSR04.h"
#include "Arduino.h"

//#define CENT_TO_INCH 1.0 / 2.54
//#define ECHO_TIME_TO_CM 1.0 / 58.0

HCSR04ProxSensor::HCSR04ProxSensor(int trigPin, int echoPin) {
  _trigPin = trigPin; // Pin connected to HCSR04 TRIG
  _echoPin = echoPin; // Pin connected to HCSR04 ECHO

  pinMode(trigPin, OUTPUT); // TRIG sends a pulse
  pinMode(echoPin, INPUT);  // ECHO listens for a reply
}

float HCSR04ProxSensor::readSensor() {
  _lastValue = _currentValue; // _currentValue will be old after this function executes, move to _lastValue

  // Send 10 us pulse on ECHO
  digitalWrite(_trigPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(_trigPin, LOW);

  
  return _currentValue = pulseIn(_echoPin, HIGH) / 58.0;

}

//std::function<float()> readSensorCentimeters = readSensor;
float HCSR04ProxSensor::readSensorCentimeters() {
  return readSensor();
}

float HCSR04ProxSensor::readSensorInches() {
  return readSensor() / 25.4;
}

float HCSR04ProxSensor::getLastValue() {
  return _lastValue;
}

