#include "dlUtils.h"
#include "blockdata.h"
#include <iostream>
using namespace std;

blockdata::blockdata(unsigned int s, bool f, unsigned char *p)
{
  blocksize = s;
  free = f;
  blockptr = p;
}

ostream &operator << (ostream &out, const blockdata &B)
{
  out << "[" << B.blocksize << ",";
  if (B.free)
    out << "free";
  else
    out << "allocated";
  out << "]";
  return out;
}

