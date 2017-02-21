#include <iostream>
#include "MemoryManager.h"
using namespace std;

int main()
{
   MemoryManager heaper(50);
   cout << "\nheap initialized\n";
   cout << "\n-------------BlockList start------------------\n";
   heaper.showBlockList();
   cout << "-------------BlockList end------------------\n\n";
   // Next, carry out a number of calls to malloc and free and
   // show the blocklist after each operation:

   

   return 0;
}
