#include "dynarr.h"
#include <iostream>
#include <string>

int doubleIt(dynarr<int> A)
{
  int newsize = 2*A.getCapacity();
  A.reserve(newsize);
  return newsize;
}

int main()
{
  dynarr<int> D(15);
  std::cout << "The capacity of D is " << D.getCapacity() << std::endl;
  dynarr<int> E(D);
  std::cout << "The capacity of E is " << E.getCapacity() << std::endl;
  D.reserve(25);
  std::cout << "The capacity of D is now " << D.getCapacity() << std::endl;
  E.reserve(2*E.getCapacity());
  std::cout << "The capacity of E is now " << E.getCapacity() << std::endl;
  D[0] = 11;
  std::cout << "D[0] = " << D[0] << std::endl;
  try {
    std::cout << D[-1] << std::endl;
  }
  catch(InvalidIndex &ex) {
    std::cout << ex.getMessage() << std::endl;
  }
  try {
    std::cout << D[30] << std::endl;
  }
  catch(InvalidIndex &ex) {
    std::cout << ex.getMessage() << std::endl;
  }
  return 0;
}
