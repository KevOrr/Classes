#include "HCSR04.h"

HCSR04ProxSensor::HCSR04ProxSensor(int trigPin, int echoPin) {
  _trigPin = trigPin;
  _echoPin = echoPin;
}

