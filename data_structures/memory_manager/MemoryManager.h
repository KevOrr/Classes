#ifndef __MM__
#define __MM__

#include <iostream>
#include <sstream>
#include "blockdata.h"
#include "dlUtils.h"

using namespace std;

class MemoryManager
{
  public:
   MemoryManager(unsigned int memsize);
   ~MemoryManager();
   unsigned char * malloc(unsigned int request);
   void free(unsigned char * ptr2block);
   void showBlockList();

  private:
   unsigned int memsize;
   unsigned char *baseptr;
   dlNode<blockdata>* header;
   dlNode<blockdata>* trailer;

   void mergeForward(dlNode<blockdata> *p);
   void mergeBackward(dlNode<blockdata> *p);
   void splitBlock(dlNode<blockdata> *p,unsigned int chunksize);
};
  
	
#endif
