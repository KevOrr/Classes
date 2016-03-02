#ifndef __HCSR04_Prox_Sensor_h__
#define __HCSR04_Prox_Sensor_h__

#define CENT_IN_INCH 2.54
#define ECHO_TIME_TO_CM 58.0

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

