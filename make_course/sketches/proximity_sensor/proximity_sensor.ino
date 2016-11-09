/******************************************
  PURPOSE:  Proximity Sensor Assignment 3
  Created by      Rudy Schlaf
  DATE:   2/2014
*******************************************/

#include "HCSR04.h"//include your proximity sensor library

#define echoPin 8  // This is the echo pin
#define triggerPin 9 // This is the trigger pin

HCSR04ProxSensor distanceSensor(echoPin,triggerPin);//here we call the constructor to instantiate a sensor named "distanceSensor"

/***************************setup function****************************************************/

void setup() {
  Serial.begin(9600);//start serial communication
}

/***************************main loop*********************************************************/

void loop() {
  Serial.print("The distance is :  ");
  
  float distance = distanceSensor.readSensor();//here we call the 'readSensor' method to determine the distance
  Serial.print(distance);// send the measurement to the serial monitor
  Serial.println(" cm");
  
  if (distanceSensor.getLastValue() - distance > 1) {Serial.println("object is approaching");}//here we call the 'getLastValue' method to determine the direction of motion
  if (distanceSensor.getLastValue() - distance < -1) {Serial.println("object is retracting");}
  

  delay(500);
}

