//# tMutex.cc: Test program for class Mutex
//# Copyright (C) 2011
//# Associated Universities, Inc. Washington DC, USA.
//# 
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//# 
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//# 
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/OS/Mutex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/iostream.h>

using namespace casacore;

// <summary>
// Test program for class Mutex.
// </summary>

void testErrorCheck()
{
  cout << "Test ERRORCHECK ..." << endl;
  Mutex mutex(Mutex::ErrorCheck);
  Bool caught = True;
  // Unlock without a lock should fail.
  try {
    mutex.unlock();
    caught = False;
  } catch (AipsError& x) {
    cout << x.what() << endl;
  }
  AlwaysAssertExit (caught);
  // First lock should succeed.
  mutex.lock();
  // A trylock should fail.
  AlwaysAssertExit (! mutex.trylock());
  // Next lock should fail.
  try {
    mutex.lock();
    caught = False;
  } catch (AipsError& x) {
    cout << x.what() << endl;
  }
  AlwaysAssertExit (caught);
  // Unlock.
  mutex.unlock();
  // A trylock should succeed.
  AlwaysAssertExit (mutex.trylock());
  mutex.unlock();
  // Next unlock should fail.
  try {
    mutex.unlock();
    caught = False;
  } catch (AipsError& x) {
    cout << x.what() << endl;
  }
  AlwaysAssertExit (caught);
  // Lock should be fine here.
  mutex.lock();
  mutex.unlock();
}

void testRecursive()
{
  cout << "Test RECURSIVE ..." << endl;
  Mutex mutex(Mutex::Recursive);
  Bool caught = True;
  // Unlock without a lock should fail.
  try {
    mutex.unlock();
    caught = False;
  } catch (AipsError& x) {
    cout << x.what() << endl;
  }
  AlwaysAssertExit (caught);
  // First lock should succeed.
  mutex.lock();
  // A trylock should succeed.
  AlwaysAssertExit (mutex.trylock());
  // Next lock should succeed.
  mutex.lock();
  // Unlock.
  mutex.unlock();
  // A trylock should succeed.
  AlwaysAssertExit (mutex.trylock());
  mutex.unlock();
  mutex.unlock();
  mutex.unlock();
  // Unlock without a lock should fail.
  try {
    mutex.unlock();
    caught = False;
  } catch (AipsError& x) {
    cout << x.what() << endl;
  }
  AlwaysAssertExit (caught);
  // Lock should be fine here.
  mutex.lock();
  mutex.unlock();
}

void testNormal()
{
  cout << "Test NORMAL ..." << endl;
  Mutex mutex(Mutex::Normal);

  // For normal mutexes, unlocking an unlocked mutex is specified as
  // producing undefined behavior.  In this case, it appears to leave
  // foul up the internal nUsers field of the mutex which causes the
  // destructor for fail (EBUSY) which throws an exception.
  // jjacobs (2013-01-17)
    //  // Usually an an unlock does not fail.
    //  try {
    //    mutex.unlock();
    //  } catch (AipsError& x) {
    //    cout << x.what() << endl;
    //  }


  // First lock should succeed.
  mutex.lock();
  // Doing another lock results in a deadlock, so we don't do that.
  // A trylock should fail.
  AlwaysAssertExit (! mutex.trylock());
  // Unlock.
  mutex.unlock();
  // A trylock should succeed.
  AlwaysAssertExit (mutex.trylock());
  mutex.unlock();
  // Lock should be fine here.
  mutex.lock();
  mutex.unlock();
}

void testMutexedInitFunc (void* arg)
{
  int* count = static_cast<int*>(arg);
  (*count)++;
}

// Test serially.
void testMutexedInitSerial()
{
  int count=0;
  MutexedInit safeInit (testMutexedInitFunc, &count);
  for (int i=0; i<16; ++i) {
    safeInit.exec();
  }
  AlwaysAssertExit (count==1);
}

// Test parallel.
void testMutexedInitParallel()
{
  int count=0;
  MutexedInit safeInit (testMutexedInitFunc, &count);
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (int i=0; i<16; ++i) {
    safeInit.exec();
  }
  AlwaysAssertExit (count==1);
}


int main()
{
  try {
    testMutexedInitSerial();
#ifdef USE_THREADS
    testErrorCheck();
    testRecursive();
    testNormal();
    testMutexedInitParallel();
#endif
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;                           // exit with success status
}
