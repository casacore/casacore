//# tBucketMapped.cc: Test program for the BucketMapped class
//# Copyright (C) 2009
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/IO/BucketMapped.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>
#include <string.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the BucketMapped class
// </summary>

void a (Bool);
void b (Bool);

int main (int argc, const char*[])
{
  try {
    a (argc<2);
    b (argc<2);
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}

// Build a file.
void a (Bool)
{
  // Create the file.
  BucketFile file ("tBucketMapped_tmp.data", 0, True);
  file.open();
  BucketMapped cache (&file, 512, 32768, 5);
  Int i;
  union {
    char buf[32768];
    Int  bufi[32768/4];
  };
  for (i=0; i<32768; i++) {
    buf[i] = 0;
  }
  cache.extend (100);
  for (i=0; i<100; i++) {
    bufi[0] = i+1;
    bufi[32760/4] = i+10;
    memcpy (cache.getrwBucket(i), buf, 32768);
  }
  for (i=0; i<100; i++) {
    const char* buf = cache.getBucket(i);
    if (*(const Int*)buf != i+1  ||  *(const Int*)(buf+32760) != i+10) {
      cout << "xError in bucket " << i << endl;
      cout << *(const Int*)buf <<' '<<  *(const Int*)(buf+32760) <<endl;
    }
  }
  cache.flush();
  cout << "wrote " << cache.nBucket() << " buckets of 32768 bytes" << endl;
}

void b (Bool)
{
  // Open the file.
  BucketFile file("tBucketMapped_tmp.data", False, 0, True);
  file.open();
  Int i;
  BucketMapped cache (&file, 512, 32768, 105);
  for (i=0; i<100; i++) {
    const char* buf = cache.getBucket(i);
    if (*(const Int*)buf != i+1  ||  *(const Int*)(buf+32760) != i+10) {
      cout << "Error in bucket " << i << endl;
      cout << *(const Int*)buf <<' '<<  *(const Int*)(buf+32760) <<endl;
    }
  }
  for (i=100; i<105; i++) {
    const char* buf = cache.getBucket(i);
    for (int j=0; j<32768; ++j) {
      if (buf[j] != 0) {
        cout << "Error in bucket " << i << endl;
        break;
      }
    }
  }
  cout << "checked " << cache.nBucket() << " buckets" << endl;
}
