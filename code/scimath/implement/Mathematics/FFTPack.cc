//# extern_fft.cc: C++ wrapper functions for FORTRAN FFT code
//# Copyright (C) 1993,1994,1995,1997
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

#include <aips/Mathematics/extern_fft.h>
#include <aips/Exceptions/Error.h>

// declarations for FORTRAN functions that actually do ffts
//  
// single precision
//
extern "C" void scopy_(int *, float *, int *, float *, int *);
extern "C" void sscal_(int *, float *, float *, int *);
extern "C" void cfftf_(int *, float *, float *);
extern "C" void cfftb_(int *, float *, float *);
extern "C" void cffti_(int *, float *);
extern "C" void rfftf_(int *, float *, float *);
extern "C" void rfftb_(int *, float *, float *);
extern "C" void rffti_(int *, float *);

//
// double precision
//
extern "C" void dcopy_(int *, double *, int *, double *, int *);
extern "C" void dscal_(int *, double *, double *, int *);
extern "C" void dcfftf_(int *, double *, double *);
extern "C" void dcfftb_(int *, double *, double *);
extern "C" void dcffti_(int *, double *);
extern "C" void drfftf_(int *, double *, double *);
extern "C" void drfftb_(int *, double *, double *);
extern "C" void drffti_(int *, double *);

extern "C" void mfft_(float *re, float *im, int *ntot, int *n, int
		      *nspan, int *isn, float *worka, float *workb,
		      float *workc, float *workd, int *worke, int
		      *workf);

extern "C" void mdfft_(double *re, double *im, int *ntot, int *n, int
		       *nspan, int *isn, double *worka, double *workb,
		       double *workc, double *workd, int *worke, int
		       *workf);
		       
//
 // c++ wrapper functions which call FORTRAN routines to do actual FFTs
 //
void 
scopy(int *n, float *sx, int *incx, float *sy, int *incy)
{
  /* call fortran function */
  scopy_(n, sx, incx, sy, incy);
}

void 
scopy(int *n, double *sx, int *incx, double *sy, int *incy)
{
  /* call fortran function */
  dcopy_(n, sx, incx, sy, incy);
}

void 
sscal(int *n, float *scale, float *sx, int *incx)
{
  /* call fortran function */
  sscal_(n, scale, sx, incx);
}

void 
sscal(int *n, double *scale, double *sx, int *incx)
{
  /* call fortran function */
  dscal_(n, scale, sx, incx);
}

void 
cfftf(int *n, float *rdata, float *work)
{
  /* call fortran function */
  cfftf_(n, rdata, work);
}

void 
cfftf(int *n, double *rdata, double *work)
{
  /* call fortran function */
  dcfftf_(n, rdata, work);
}

void 
cfftb(int *n, float *rdata, float *work)
{
  /* call fortran function */
  cfftb_(n, rdata, work);
}

void 
cfftb(int *n, double *rdata, double *work)
{
  /* call fortran function */
  dcfftb_(n, rdata, work);
}


void 
rfftf(int *n, float *rdata, float *work)
{
  /* call fortran function */
  rfftf_(n, rdata, work);
}

void 
rfftf(int *n, double *rdata, double *work)
{
  /* call fortran function */
  drfftf_(n, rdata, work);
}

void 
rfftb(int *n, float *rdata, float *work)
{
  /* call fortran function */
  rfftb_(n, rdata, work);
}

void 
rfftb(int *n, double *rdata, double *work)
{
  /* call fortran function */
  drfftb_(n, rdata, work);
}

void 
cffti(int *n, float *work)
{
  /* call fortran function */
  cffti_(n, work);
}

void 
cffti(int *n, double *work)
{
  /* call fortran function */
  dcffti_(n, work);
}

void 
rffti(int *n, float *work)
{
  /* call fortran function */
  rffti_(n, work);
}

void 
rffti(int *n, double *work)
{
  /* call fortran function */
  drffti_(n, work);
}


// A local helper class used by mfft and mdfft.
// See those functions for examples of usage.

class TempStore {
  // Provides reusable temporary storage which can be 
  // dynamically resized.
  // The storage always grows, it never shrinks. 
public:
  TempStore();
  TempStore(int s);
  // copy, NOT reference semantics
  TempStore(const TempStore&);

  ~TempStore();

  // copy, NOT reference semantics
  const TempStore& operator=(const TempStore&);

  // Obvious semantics. Does not do bounds checking!
  char &operator[](int i) { return store[i]; }
  
  // return Address of store. If resize > size, 
  // resizes store, and returns new address.

  char *getAddress(int resize=0);

  // returns the size of store
  int getSize();
protected:
  char *store;
  int size;
};

TempStore::TempStore()
{
  store = 0;
  size = 0;
}

TempStore::TempStore(int s)
{
  if (s > 0) {
    store = new char[s];
    if (!store) {
      throw(AipsError("TempStore::TempStore(int):: new returned 0"));
    }
    size = s;
  } else {
    store = 0;
    size = 0;
  }
}

TempStore::TempStore(const TempStore &other)
{
  *this=other;
}


const TempStore &TempStore::operator=(const TempStore &other)
{
  // ok to delete zero pointer, see ARM
  delete [] store;
  if (other.size > 0) {
    store = new char[other.size];
    if (!store) {
      throw(AipsError("TempStore::TempStore(int):: new returned 0"));
    }
    size = other.size;
  } else {
    store = 0;
    size = 0;
  }
  // copy storage
  for (int i = 0; i < other.size; ++i) {
    store[i] = other.store[i];
  }
  return *this;
}

TempStore::~TempStore()
{
  delete [] store;
}

char *TempStore::getAddress(int resize)
{
  if (resize > size) {
    delete [] store;
    store = new char[resize];
    if (!store) {
      throw(AipsError("TempStore::getAddress(int):: new returned 0"));
    } 
    size = resize;
  }
  return store;
}

int TempStore::getSize()
{
  return size;
}

/*

   // Testing code for TempStore included here. To use, you
   // must include iostream.h.


   void testTempStore() {
   static TempStore a;
   static TempStore b;

   TempStore c(10);

   
   cout << "Testing constructors and getSize(...)" << endl;
   cout << " a's size, should be zero: " << a.getSize() << endl;
   cout << " b's size, should be zero: " << b.getSize() << endl;

   cout << "Testing getAddress(...)" << endl;
   cout << " a's address, should be zero: " << (void*) a.getAddress(0) << endl;
   cout << " a's size, should be zero: " << a.getSize() << endl;

   cout << " b's address,  should be nonzero: " << (void*) b.getAddress(5) << endl;
   cout << " b's size, should be 5: " << b.getSize() << endl;

   cout << " b's address, should be same as last: " << (void*) b.getAddress(4) << endl;
   cout << " b's size, should still be 5: " << b.getSize() << endl;

   cout << " c's address, should be nonzero: " << (void*) c.getAddress() << endl;
   cout << " c's size, should be 10: " << c.getSize() << endl;

   cout << " c's address, should be nonzero, and different: " << (void*) c.getAddress(1024) << endl;
   cout << " c's size, should be 1024: " << c.getSize() << endl;

   cout << " c's address, should be same as last: " << (void*) c.getAddress() << endl;
   cout << " c's size, should still be 1024: " << c.getSize() << endl;

   cout << endl;

   cout << "Testing copy constructor" << endl;

   TempStore d(c);

   cout << " d's address, should be different than last c: " << (void *) d.getAddress() << endl;
   cout << " d's size, should be same as c's: " << d.getSize() << endl;

   cout << endl;

   cout << "Testing operator= and operator[]" << endl;

   c = a;
   cout << " c's address, should now be zero: " << (void *) c.getAddress() << endl;
   cout << " c's size, should now be zero: " << (void *) c.getSize() << endl;

   char *bstorage=b.getAddress();
   for (int i = 0; i < b.getSize(); ++i) {
   b[i] = i + 'a';
   }
   b[i] = '\0';

   cout << " address of b's storage: " << (void*) b.getAddress() << endl;
   cout << " b's storage, should look like abcd... " << bstorage << endl;
   cout << " b's size, should still be 5: " << b.getSize() << endl;

   TempStore e;

   cout << " e's storage, should be zero: " << (void*) e.getAddress() << endl;
   cout << " e's size, should be zero: " << e.getSize() << endl;
   
   e = b;

   cout << " address of b's storage, should be different than e's:" << (void*) e.getAddress() << endl;
   cout << " e's storage, should look like e's: " << e.getAddress() << endl;
   cout << " e's size, should be same as b's:" << e.getSize() << endl;

   }

   */
// mfft and mdfft invoke FORTRAN fft routines. They use the TempStore
// class to allocate temporary storage. TempStore allows the storage
// is reused, or resized dynamically. mfft and mdfft are frequently 
// called repeatedly with the same value of n; hence this is a useful 
// optimization, since it avoids the overhead of calls to new/delete.
// Perhaps we could just use alloca().

// Note that this usage disallows concurrency. If we had more than
// a single thread of control, we'd be in trouble.

static TempStore WorkArrays[6];

void 
mfft(float *a, float *b, int *ntot, int *n, int *nspan, int *isn) 
{
  mfft_(a, b, ntot, n, nspan, isn, 
	(float *) WorkArrays[0].getAddress(sizeof(float) * *n),
	(float *) WorkArrays[1].getAddress(sizeof(float) * *n),
        (float *) WorkArrays[2].getAddress(sizeof(float) * *n),
        (float *) WorkArrays[3].getAddress(sizeof(float) * *n),
        (int *) WorkArrays[4].getAddress(sizeof(int) * *n),
        (int *) WorkArrays[5].getAddress(sizeof(int) * 16));
  
  if (*isn == 0) {
  // can't happen 
    throw(AipsError("Static memory limit in mfft_ exceeded."));
  }
}

void 
mfft(double *a, double *b, int *ntot, int *n, int *nspan, int *isn) 
{
  mdfft_(a, b, ntot, n, nspan, isn, 
	 (double *) WorkArrays[0].getAddress(sizeof(double) * *n),
	 (double *) WorkArrays[1].getAddress(sizeof(double) * *n),
	 (double *) WorkArrays[2].getAddress(sizeof(double) * *n),
	 (double *) WorkArrays[3].getAddress(sizeof(double) * *n),
	 (int *) WorkArrays[4].getAddress(sizeof(int) * *n),
	 (int *) WorkArrays[5].getAddress(sizeof(int) * 16));
  if (*isn == 0) {
    // singleton ran out of memory
    throw(AipsError("Static memory limit in mdfft_ exceeded."));
  }
}

