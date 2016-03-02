#ifndef __HCSR04_Prox_Sensor_h__
#define __HCSR04_Prox_Sensor_h__

class HCSR04ProxSensor {
  public:
    HCSR04ProxSensor(int trigPin, int echoPin);
    float readSensor();
    float readSensorInches();
    float readSensorCentimeters();

    float getLastValue();


  private:
    int _trigPin;
    int _echoPin;

    float _currentValue;
    float _lastValue;
};

#endif

