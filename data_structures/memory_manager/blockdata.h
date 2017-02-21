#ifndef _BLOCKDATA_
#define _BLOCKDATA_

#include <iostream>

using namespace std;

class blockdata {
  friend ostream& operator << (ostream&, const blockdata &);

 public:
  blockdata(unsigned int s, bool f, unsigned char *p);
  unsigned char blocksize;
  bool free;
  unsigned char *blockptr;	    
};


#endif
