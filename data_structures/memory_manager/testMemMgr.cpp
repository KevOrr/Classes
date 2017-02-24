#include <iostream>
#include <vector>
#include <random>
#include "MemoryManager.h"

int main() {
   MemoryManager heaper(100);
   std::cout << "Heap initialized" << std::endl;
   heaper.showBlockList();

   std::vector<unsigned char *> chunks;

   std::default_random_engine gen;
   std::uniform_int_distribution<unsigned int> distr(0, 20);
   for (int i=0; i<10; ++i) {
       unsigned int size = distr(gen);
       std::cout << "Requesting memory size " << size << "..." << std::endl;
       unsigned char *p = heaper.malloc(size);

       if (p)
           chunks.push_back(p);
       else
           std::cout << " FAILED" << std::endl;

       heaper.showBlockList();
   }

   while (auto still_allocated = chunks.size()) {
       auto choice = std::uniform_int_distribution<std::vector<unsigned char *>::size_type>(0, still_allocated-1)(gen);
       std::cout << "Freeing " << chunks[choice];
       heaper.free(chunks[choice]);
       chunks.erase(chunks.begin() + choice);
       heaper.showBlockList();
   }

   return 0;
}
