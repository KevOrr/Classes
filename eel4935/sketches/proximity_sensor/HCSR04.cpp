#include "HCSR04.h"
#include "Arduino.h"

#define CENT_TO_INCH 1.0 / 2.54

HCSR04ProxSensor::HCSR04ProxSensor(int trigPin, int echoPin) {
  _trigPin = trigPin;
  _echoPin = echoPin;

  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
}

float HCSR04ProxSensor::readSensor() {
  _lastValue = _currentValue;

  digitalWrite(_trigPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(_trigPin, LOW);

  return _currentValue = pulseIn(_echoPin, HIGH)/58.0;

}

//std::function<float()> readSensorCentimeters = readSensor;
float HCSR04ProxSensor::readSensorCentimeters() {
  return readSensor();
}

float HCSR04ProxSensor::readSensorInches() {
  return readSensor() * CENT_TO_INCH;
}

float HCSR04ProxSensor::getLastValue() {
  return _lastValue;
}

