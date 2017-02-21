#include <cassert>
#include <iostream>
#include "dlUtils.h"
#include "MemoryManager.h"

MemoryManager::MemoryManager(unsigned int memtotal): memsize(memtotal)
{
   baseptr = new unsigned char[memsize];
   blockdata dummyBlock(0,false,0);
   blockdata originalBlock(memsize,true,baseptr);
   header = new dlNode<blockdata>(dummyBlock,nullptr,nullptr);
   trailer = new dlNode<blockdata>(dummyBlock,nullptr,nullptr);
   header->next = new dlNode<blockdata>(originalBlock,header,trailer);
   trailer->prev = header->next;
}

MemoryManager::~MemoryManager()
{
  delete [] baseptr;
  clearList(header);
}

void MemoryManager::showBlockList() 
{
  printDlList(header,trailer,"->");
}

void MemoryManager::splitBlock(dlNode<blockdata> *p, unsigned int chunksize)
{
  // Complete the code for this method
  
}

unsigned char * MemoryManager::malloc(unsigned int request)
{
  // Complete the code for this method

}

void MemoryManager::mergeForward(dlNode<blockdata> *p)
{
  // Complete the code for this method

}

void MemoryManager::mergeBackward(dlNode<blockdata> *p)
{ 
  // Complete the code for this method

}

void MemoryManager::free(unsigned char *ptr2block)
{
  // Complete the code for this method

}


