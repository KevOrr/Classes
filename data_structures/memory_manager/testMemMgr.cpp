#include <iostream>
#include "MemoryManager.h"

int main() {
   MemoryManager heaper(100);
   std::cout << "heap initialized\n";
   heaper.showBlockList();

   

   return 0;
}
