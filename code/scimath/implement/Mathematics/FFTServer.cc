//# FFTServer.cc: A class with methods for Fast Fourier Transforms
//# Copyright (C) 1994,1995,1996
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

#include <aips/Mathematics/FFTServer.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/VectorIter.h>
#include <memory.h>
#include <aips/Lattices/Slice.h> 
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <iostream.h>

//------------------------------------------------------

// Default constructor.
template<class T, class S>
FFTServer<T,S>::FFTServer(): temp(1),work(1)

{
  work = T(0.0);
  temp = T(0.0);
  scale = T(0.0);
  dimension = 1;
  IPosition Nyquist_parms(dimension);
  Nyquist_parms(0) = 1;
  Nyquist.resize(Nyquist_parms);
  Nyquist = T(0.0);
}

// Copy constructor. 
template<class T, class S>
FFTServer<T,S>::FFTServer(const FFTServer<T,S> &other) 
: FourierTool<T,S>(other)
{
  temp  = other.temp;
  work  = other.work;
  scale = other.scale;
  dimension = other.dimension;
  Nyquist = other.Nyquist;
}

// Construct with the real Array rdata.
template<class T, class S>
FFTServer<T,S>::FFTServer(const Array<T> & rdata): temp(1), work(1)
{
  Set(rdata);
}

// Construct with the complex Array cdata.
template<class T, class S>
FFTServer<T,S>::FFTServer(const Array<S> &cdata, int has_ny): temp(1), work(1)
{
  Set(cdata, has_ny);
}

// Construct with the IPosition object shape.
// Shape is the size of the array with which
// to do transforms.
template<class T, class S>
FFTServer<T,S>::FFTServer(const IPosition &Shape): temp(1), work(1)
{
  Set(Shape);
}

// Destructor. Currently does nothing.
template<class T, class S>
FFTServer<T,S>::~FFTServer()
{
  // nothing
}

// Reassociate *this with a real array.
template<class T, class S>
void
FFTServer<T,S>::Set(const Array<T> & rdata)
{

  dimension = rdata.ndim();

  // copy semantics
  IPosition Shape(rdata.shape());

  int size, i;

  // Set size to maximum size
  // of dimensions
  
  for (i = 1, size = Shape(0); i < dimension; ++i) {
    if (Shape(i) > size) {
      size = Shape(i);
    }
  }

  // Set size of work array to 4*size + 15
  IPosition Length(1);
  Length = 4 * size + 15;
  work.resize(Length); work = T(0.0);

  // initialize work array to requirements
  // of {d,f}fftpak.f functions
  if (dimension == 1) {
    Bool Delete_w;
    T *workptr = work.getStorage(Delete_w);
    // first initialize
    rffti(&Shape(0), workptr);
    work.putStorage(workptr, Delete_w);
  }

  if (dimension > 1) {
    Length = 2 * size;
    temp.resize(Length); temp = T(0.0);
  }

  // Resize Nyquist array
  Shape(0) = 2;
  Nyquist.resize(Shape);
  Nyquist = T(0.0);
}

// Reassociate *this with a complex Array.
template<class T, class S>
void
FFTServer<T,S>::Set(const Array<S> &cdata, int has_ny)
{
  dimension = cdata.ndim();

  IPosition Shape(cdata.shape());

  int nrows, size, i;

  if (has_ny == 0)
    nrows = Shape(0) * 2;
  else
    nrows = (Shape(0) - 1) * 2;

  // set size to max{nrows, size(dimensions
  // 1 through dimension)}

  for (i = 1, size = nrows; i < dimension; ++i) {
    if (Shape(i) > size) {
      size = Shape(i);
    }
  }

  // size and initialize work array 
  // according to requirements
  // of {d,f}fftpak.f functions
  IPosition Length(1);
  Length = 4 * size + 15;
  work.resize(Length); work = T(0.0);
  if (dimension == 1) {
    Bool Delete_w;
    T *workptr = work.getStorage(Delete_w);
    // first initialize
    nrows /= 2;
    cffti(&nrows, workptr);
    work.putStorage(workptr, Delete_w);
  }

  if (dimension > 1) {
    Length = 2 * size;
    temp.resize(Length); temp = T(0.0);
  }

  // resize Nyquist array
  Shape(0) = 2;
  Nyquist.resize(Shape);
  Nyquist = T(0.0);
}

// Reassociate *this with an IPosition object.  Shape is the shape of the
// array for which FFTS are to be done.
template<class T, class S>
void
FFTServer<T,S>::Set(const IPosition &Shape)
{
  dimension = Shape.nelements();
  int size, i; 
  IPosition shape(Shape);

  // set size to size of maximum dimension
  for (i = 1, size = shape(0); i < dimension; ++i) {
    if (shape(i) > size) {
      size = shape(i);
    }
  }

  // initialize work array to requirements
  // of {d,f}fftpak.f functions
  IPosition Length(1);
  Length = 4 * size + 15;
  work.resize(Length); work = T(0.0);
  if (dimension == 1) { // initialize our work array 
    Bool Delete_w; 
    T *workptr = work.getStorage(Delete_w);
    i = shape(0);
    rffti(&i, workptr); 
    work.putStorage(workptr, Delete_w); 
  } 
  if (dimension > 1) { 
    Length = 2 * size; 
    temp.resize(Length); temp = T(0.0); 
  } 

  // resize Nyquist array
  shape(0) = 2; 
  Nyquist.resize(shape); 
  Nyquist = T(0.0); 
} 

// operator= Copy semantics.
template<class T, class S>
FFTServer<T,S> &
FFTServer<T,S>::operator=(const FFTServer<T,S>& other)
{
  if(this == &other) return *this;

  FourierTool<T,S>::operator=(other);  //copy base class members
  temp.resize(other.temp.shape());
  work.resize(other.work.shape());
  temp = other.temp;
  work = other.work;
  scale = other.scale;
  dimension = other.dimension;

  return *this;
}

// Real to real packed n dimensional fft.  If dir > 0, forward transform;
// if dir < 0, backward transformation with normalization. If dir == 0,
// backward transformation without normalization.
template<class T, class S>
void 
FFTServer<T,S>::fft(Array<T> &rdata, int dir)
{
  
  if (dir <= 0) {
    unpack(rdata);
  }

  // only scale for dir < 0, not dir == 0
  rrfft(rdata, dir, dir < 0 ? 1 : 0);

  if (dir > 0 ) {
    pack(rdata);
  }
}

// Real to real n dimensional fft on rdata. Size of rdata should be
// (n1+2) x n2 x n3 x ... x nm where m is the number of dimensions. The
// Nyquist frequency components, if n1 is even, are stored in the last
// two columns. If n1 is odd, then the n1-1 (last) column is the
// imaginary component of the last element, and real component of the
// last element is duplicated in the n1-2 and n1-3 columns.  If dir > 0,
// forward transform; if dir == 0, backward transform without
// normalization; if dir < 0, backward transform with normalization.
template<class T, class S>
void 
FFTServer<T,S>::nyfft(Array <T> &rdata, int dir)
{
  int config = rdata.ndim();
  if (config != dimension ) {
    throw(FFTError("FFTServer::nyfft() - error: wrong ffttool for dimension"
		   " of array"));
  }
  IPosition Shape(dimension);
  Shape = rdata.shape();
  int nrows = Shape(0);

  if (dir <= 0) { // reverse transform
    if (dimension > 1) {
      exchangeUV(rdata);
// not strictly necessary unless someone expects to
// wants to extract Nyquist
      exchangeUV(Nyquist);
    }
  } else {
    flipImage(rdata,1);
  }

  // only scale for dir < 0

  rndnyfft(rdata, dir, dir < 0 ? 1 : 0);

  if (dir > 0 ) {
    if (dimension > 1) {
      // parity 0 on this side
      exchangeUV(rdata, 0);
// not strictly necessary, but decent if
// someone wants to extract Nyquist.
      exchangeUV(Nyquist, 0);
    }
  } else {
    // parity 0 on this side
    flipImage(rdata, 1, 0);
  }
}

// Complex to complex n dimensional fft on cdata. If dir > 0, forward
// transform. If dir == 0, backward transform without scaling. If
// dir<0, backward transform with scaling.  If center==0, do not shift
// phase center to origin. If center nonzero, shift image to phase center.
template<class T, class S>
void 
FFTServer<T,S>::cxfft(Array<S> &cdata, int dir, int center)
{
// This function makes optimistic optimizations based on the rank
// of cdata. If cdata is of even rank, then shifting by half in either
// the time or frequency domains is equivalent to multiplying an element
// by -1 if the sum of its coordinates relative to a zero origin is odd.
// If cdata is of odd rank, then complex multiplications in shift are
// used (O(N)). This may produce noticeable roundoff error.  The efficacy
// of this solution is unproven; however, copying is presently
// O(dimension of cdata * N) in flipimage. Multiplying every second
// element by -1 is O(N) and avoids half the copies. The optional scaling
// after a reverse transform is also wrapped in.
  int canflipsigns;
  int i;
  int ndims=cdata.ndim();
  IPosition shape(cdata.shape());
  Vector<T> offset(ndims);

  if (center) {
    for (i = 0; i < ndims; ++i) {
      offset(i) = T(shape(i) / 2);
    }
    canflipsigns=1;
    for (i = 0; i < ndims; ++i) {
      if (shape(i) % 2) {
	// some dimension(i) is of odd size
	canflipsigns=0;
	break;
      }
    }
    if (canflipsigns) {
      flipsignsfact(cdata, S(1.0));
    } else {
      shift(cdata, S(1.0), dir > 0 ? 0 : 1, offset);
    }
  }

  // sets member scale for us, but don't
  // scale cdata
  cndfft(cdata, dir, 0);

  if (center) {
    if (canflipsigns) {
      flipsignsfact(cdata, dir < 0 ? S(scale) : S(1.0));
    } else {
      if (dir < 0) { // normalize after backward transform
	shift(cdata, S(scale), 1, offset);
      } else if (dir == 0) {
	shift(cdata, S(1.0), 1, offset);
      } else {
	shift(cdata, S(1.0), 0, offset);
      }
    }
  }
}

// Real to complex forwrd transform on rdata. Returns complex array.
// If first dimension of rdata is odd, then the returned array will be
// of size ceiling (N/2) complex elements in the first dimension.
template<class T, class S>
Array<S> 
FFTServer<T,S>::rcfft(const Array<T> &rdata)
{
// <note role=warning> Very inefficient! Current implementation
// results in repeated copying of the array. Some of the steps should
// be rolled into one. </note>
  Array<T> rdataCopy(rdata.shape());
  rdataCopy = rdata;

  // forward transform on rdataCopy
  rrfft(rdataCopy, 1);
  pack(rdataCopy);

  if (rdata.shape()(0) % 2) {
    expandby1(rdataCopy);
  }
  Array<S> cdata = RealToComplex(rdataCopy);
  return cdata;
}

// Real to complex forward transform on rdata. Complex result
// returned, with an extra complex element at the end for Nyquist. If
// the first dimension of rdata is odd, then the array will only be
// expanded by one. The result will have a first dimension of
// ceil((N+1)/2) complex elements.
template<class T, class S>
Array<S> 
FFTServer<T,S>::rcnyfft(const Array<T> &rdata)
{
// <note role=warning> Very inefficient! Current implementation
// results in repeated copying of the array. Some of the steps should
// be rolled into one. </note>
  Array<T> rdataCopy(rdata.shape());
  rdataCopy = rdata;

  // forward transform on rdataCopy
  rrfft(rdataCopy, 1);


  if (rdata.shape()(0) % 2) {
    expandby1(rdataCopy);
  } else {
    expand(rdataCopy);
  }
  Array<S> cdata = RealToComplex(rdataCopy);
  return cdata;
}


// Complex to real backward transform on cdata. If do_scale is zero,
// normalization not done, otherwise it is. If shrinkodd is nonzero,
// then the base method shrinkby1 will be invoked on the converted
// real array before transformation.
template<class T, class S>
Array<T>
FFTServer<T, S>::crfft(Array<S> &cdata, int do_scale, int shrinkodd)
{
// <note role=warning> Very inefficient! Current implementation
// results in repeated copying of the array. Some of the steps should
// be rolled into one. </note>
  Array<T> rdata = ComplexToReal(cdata);

  if (shrinkodd) {
    shrinkby1(rdata);
  }

  unpack(rdata);
  // reverse transform on rdata
  rrfft(rdata, -1, do_scale);

  return rdata;
}

// Complex to real backward transform. The last two element in each
// row of cdata is expected to contain the Nyquist component. If
// shrinkodd is 1, then it is expected to contain the last element in
// each row, and the array is shrunk by 1 before the transform.
// Otherwise it is shrunk by 2. If do_scale is zero, normalization is
// not done, otherwise it is.
template<class T, class S>
Array<T>
FFTServer<T, S>::crnyfft(Array<S> &cdata, int do_scale, int shrinkodd)
{
// <note role=warning> Very inefficient! Current implementation
// results in repeated copying of the array. Some of the steps should
// be rolled into one. </note>
  Array<T> rdata = ComplexToReal(cdata);

  if (shrinkodd) {
    shrinkby1(rdata);
  } else {
    shrink(rdata);        // copy Nyquist data to internal store
  }

  // reverse transform on rdata

  rrfft(rdata, -1, do_scale);

  return rdata;
}


// Multiplies all elements in cdata by factor*(-1)^(i+j+k+...+n) where
// i,j,k,...,n are the dimensions of cdata. When cdata has even
// dimensions, this is equivalent to shifting in either domain by half
// in every direction.
template<class T, class S>
void
FFTServer<T,S>::flipsignsfact(Array<S> &cdata, S factor) 
{
// Optimized for Vectors, Matrixes, and Cubes.
  int ndims=cdata.ndim();
  IPosition shape=cdata.shape();
  IPosition origin=cdata.origin();
  IPosition end=cdata.end();
  IPosition cursor(ndims);

  for (int x=0; x < ndims; ++x) {
    // all dimensions must be even
    if (shape(x) % 2) {
      throw(FFTError("FFTServer::flipsignsfact(...): dimension of odd size"));
    }
  }
  S negfactor(-factor);
  if (ndims==1) {
    // Vector
    Vector<S> v;
    v.reference(cdata);
    int n = end(0);
    int i = origin(0);
    while (i <= n) {
      v(i++) *= factor;
      v(i++) *= negfactor;
    }
  } else if (ndims==2) {
    // Matrix
    Matrix<S> m;
    m.reference(cdata);
    int n1 = end(0);
    int n2 = end(1);
    for (int i1 = origin(0); i1 <= n1; i1++) {
      for (int i2 = origin(1); i2 <= n2; i2++) {
	if ((i1 + i2) % 2) { // odd exponent
	  m(i1, i2) *= negfactor;
	} else {
	  m(i1, i2) *= factor;
	}
      }
    }
  } else if (ndims==3) {
    // cube
    Cube<S> c;
    c.reference(cdata);
    int n1 = end(0);
    int n2 = end(1);
    int n3 = end(2);
    int partialsum;
    for (int i1 = origin(0); i1 <= n1; i1++) {
      for (int i2 = origin(1); i2 <= n2; i2++) {
	partialsum = i1 + i2;
	for (int i3 = origin(2); i3 <= n3; i3++) {
	  if ((partialsum + i3) % 2) { // odd exponent
	    c(i1, i2, i3) *= negfactor;
	  } else {
	    c(i1, i2, i3) *= factor;
	  }
	}
      } 
    }
  } else if (ndims > 3) {
    int i;
    int sum;
    ArrayPositionIterator iterator(shape, origin, 0);
    while (!iterator.pastEnd()) {
      cursor = iterator.pos();
      for (i = sum = 0; i < ndims; ++i) {
	sum += cursor(i) - origin(i);
      }
      if (sum % 2) { // odd exponent
	cdata(cursor) *= negfactor;
      } else {
	cdata(cursor) *= factor;
      }
      iterator.next();
    }
  } else {
    throw(FFTError("FFTServer::flipsignsfact(...): "
                   "incorrect number of dimensions"));
  }
}



// Multiplies elements in cdata by factor and complex exponentials
// equivalent to shifting by shift in the domain specified by timeshift.
// For example, suppose we want to shift cdata by (5,1,....) in the
// frequency domain. We could perform a forward transform, then
// shift. Equivalently, we call this function before with timeshift set
// to zero, and shift set to (5,1....), then we perform the transform.
// If timeshift is 1, the sign of the exponent will be -1; otherwise 1.
// Remember: shifting in the time domain is equivalent to multiplying
// before the transform with time set to 1
// shifting in the frequency domain is equivalent to multiplying in the
// frequency domain before the transform, with time set to 0
// shift is the amount to shift.
template<class T, class S>
void
FFTServer<T,S>::shift(Array<S> &cdata, S factor, 
                      int timeshift, const Vector<T> &shift) 
{
    // Optimized for Vectors, Matrixes, and Cubes.
  int ndims=cdata.ndim();
  if (shift.nelements() != ndims) {
    throw(FFTError("FFTServer::shift(...): parameter "
                   "Vector<T> shift must have "
		   "as many elements as the "
                   "parameter Array<S> cdata has "
		   "dimensions"));
  }
  IPosition shape=cdata.shape();
  IPosition origin=cdata.origin();
  IPosition end=cdata.end();
  IPosition cursor(ndims);

  for (int i = 0; i < ndims; ++i) {
    if (shape(i) == 0) {
    throw(FFTError("FFTServer::shift(..): some dimension"
		   "of cdata has zero size."));
    }
  }
  S expmultiplier = S(0.0,M_PI * 2.0);

  if (timeshift) { // multiplying in UV domain; exponent -1
    expmultiplier *= S(-1.0);
  }  // otherwise, multiplying in time domain; exponent +1
  if (ndims==1) {
    // Vector
    Vector<S> v;
    v.reference(cdata);
    int n = end(0);
    S incr = expmultiplier * (shift(0) / shape(0));
    S exponent = S(0.0);			
    // idea is to use repeated addition rather than multiplication
    for (int i = origin(0) ; i <= n; ++i, exponent += incr) {
      // Probably should cache these exp values; would probably
      // result in significant performance gains
      v(i) *= factor * exp(exponent);
    }
  } else if (ndims==2) {
    // Matrix
    Matrix<S> m;
    m.reference(cdata);
    int n1 = end(0);
    int n2 = end(1);
    S incr0 = expmultiplier * (shift(0) / shape(0));
    S incr1 = expmultiplier * (shift(1) / shape(1));
    S exponent = S(0.0);
    S outerexponent = S(0.0);
// use repeated addition to avoid multiplication exponent looks like +/-
// j * 2 * pi * ((i/N)*n + (j/M)*m) where j = sqrt(-1), i is shift in
// first dim, j is shift in sec.  dim, n and m are the varying indexes,
// and N and M are the sizes of the respective dimensions.  We can avoid
// the multiplications with repeated additions: i, j, N, M are constant;
// m varies fastest.
    for (int i1 = origin(0); i1 <= n1; i1++, outerexponent += incr0) {
      exponent = outerexponent;
      for (int i2 = origin(1); i2 <= n2; i2++, exponent += incr1) {
	m(i1, i2) *= factor * exp(exponent);
      }
    }
  } else if (ndims==3) {
    // Cube
    Cube<S> c;
    c.reference(cdata);
    int n1 = end(0);
    int n2 = end(1);
    int n3 = end(2);
    S incr0 = expmultiplier * (shift(0) / shape(0));
    S incr1 = expmultiplier * (shift(1) / shape(1));
    S incr2 = expmultiplier * (shift(2) / shape(2));
    S exponent = S(0.0);
    S middleexponent = S(0.0);
    S outerexponent = S(0.0);
    for (int i1 = origin(0); i1 <= n1; i1++, outerexponent += incr0) {
      middleexponent = outerexponent;
      for (int i2 = origin(1); i2 <= n2; i2++, middleexponent += incr1) {
	exponent = middleexponent;
	for (int i3 = origin(2); i3 <= n3; i3++, exponent += incr2) {
	  c(i1, i2, i3) *= factor * exp(exponent);
	}
      }
    } 
  }  else if (ndims > 3) {
    int i;
    int sum;
    Vector<T> products(ndims);
    for (i = 0; i < ndims; ++i) {
      products(i) = (shift(i) / shape(i));
    }
    T partialsum;
    ArrayPositionIterator iterator(shape, origin, 0);
    while (!iterator.pastEnd()) {
      cursor = iterator.pos();
      for (partialsum = T(1.0), i = 0; i < ndims; ++i) {
	// inefficient, since partial sum doesn't change that often
	partialsum += products(i) * (cursor(i) - origin(i));
      }
      cdata(cursor) *= factor * exp(expmultiplier * partialsum);
      iterator.next();
    }
  } else {
    throw(FFTError("FFTServer::flipsignsfact(...): incorrect number of dimensions"));
  }
}


// This function moves weights on to a uv grid in preparation for doing
// FFT to obtain an antenna pattern packing is also done.
template<class T, class S>
void 
FFTServer<T,S>::uvassign(Array<T> &UV, Array<T> &Weight)
{
  IPosition UV_Shape(dimension);
  IPosition wt_shape(dimension);
  UV_Shape = UV.shape();
  wt_shape = Weight.shape();

  // the number of rows in UV must be 2 * nwidth

  if (UV_Shape(0) != 2 * wt_shape(0) - 2 ) {
    throw(FFTError("FFTServer::uvassign() - error: UV grid row dimension"
		   " is less than 2 x that of weights"));
  }
  VectorIterator<T> uv_iter(UV);
  VectorIterator<T> wt_iter(Weight);
  VectorIterator<T> ny_iter(Nyquist);
  Int start_wt, end_wt;
  wt_iter.vector().origin(start_wt);
  wt_iter.vector().end(end_wt);
  
  while (! wt_iter.pastEnd()) {
    Vector<T> &uv_temp = uv_iter.vector();
    Vector<T> &wt_temp = wt_iter.vector();
    Vector<T> &ny_temp = ny_iter.vector();
    for(Int i=start_wt; i < end_wt; i++) 
      uv_temp(2 * i) = wt_temp(i);
    ny_temp(0) = wt_temp(end_wt);
    ny_temp(1) = T(0.0);
    uv_iter.next();
    wt_iter.next();
    ny_iter.next();
  }
  pack(UV);
}

// This function moves weights on to a uv grid in preparation for
// doing FFT to obtain an antenna pattern packing is also done.
template<class T, class S>
void 
FFTServer<T,S>::uvassign(Array<S> &UV, Array<T> &Weight)
{
  IPosition UV_Shape(dimension);
  IPosition wt_shape(dimension);
  UV_Shape = UV.shape();
  wt_shape = Weight.shape();
  // the number of rows in UV must be 2 * nwidth
  if (UV_Shape(0) !=  wt_shape(0) - 1 ) {
    throw(FFTError("FFTServer::uvassign() - error: UV grid row dimension"
		   " is less than 2 x that of weights"));
  }
  VectorIterator<S> uv_iter(UV);
  VectorIterator<T> wt_iter(Weight);
  VectorIterator<T> ny_iter(Nyquist);
  Int start_wt, end_wt;
  wt_iter.vector().origin(start_wt);
  wt_iter.vector().end(end_wt);
  
  while (! wt_iter.pastEnd()) {
    Vector<S> &uv_temp = uv_iter.vector();
    Vector<T> &wt_temp = wt_iter.vector();
    Vector<T> &ny_temp = ny_iter.vector();
    for(Int i=start_wt; i < end_wt; i++) 
      uv_temp(i) = S(wt_temp(i));
    ny_temp(0) = wt_temp(end_wt);
    ny_temp(1) = T(0.0);
    uv_iter.next();
    wt_iter.next();
    ny_iter.next();
  }
  pack(UV);
}

// This function moves weights on to a full complex uv grid in
// preparation for doing FFT to obtain an antenna pattern packing is also
// done.
template<class T, class S>
void 
FFTServer<T,S>::cxUVassign(Array<S> &UV, Array<T> &Weight)
{
  IPosition UV_Shape(dimension);
  IPosition wt_shape(dimension);
  UV_Shape = UV.shape();
  wt_shape = Weight.shape();

  // the number of rows in UV must be 2 * nwidth

  if (UV_Shape(0) != wt_shape(0) ) {
    throw(FFTError("FFTServer::cxUVassign() - error: UV grid row dimension"
		   " does not equal that of weights"));
  }
  VectorIterator<S> uv_iter(UV);
  VectorIterator<T> wt_iter(Weight);
  Int start_wt, end_wt;
  wt_iter.vector().origin(start_wt);
  wt_iter.vector().end(end_wt);
  
  while (! wt_iter.pastEnd()) {
    Vector<S> &uv_temp = uv_iter.vector();
    Vector<T> &wt_temp = wt_iter.vector();
    for(Int i=start_wt; i <= end_wt; i++) 
      uv_temp(i) = S(wt_temp(i));
    uv_iter.next();
    wt_iter.next();
  }
}

// This function sums up the weights for FFT normalization.
template<class T, class S>
T FFTServer<T,S>::wtsum(Array<T> &Weight)
{
  VectorIterator<T> wt_iter(Weight);
  Int start_wt, end_wt;
  wt_iter.vector().origin(start_wt);
  wt_iter.vector().end(end_wt);
  
  T ret_value = T(0.0);
  while (! wt_iter.pastEnd()) {
    Vector<T> &wt_temp = wt_iter.vector();
    for(Int i=start_wt+1; i <= end_wt-1; i++) 
      ret_value += 2.0 * wt_temp(i);
    ret_value += wt_temp(start_wt) + wt_temp(end_wt);
    wt_iter.next();
  }
  return ret_value;
}

// This function sums up the weights from a full complex grid for FFT
// normalization.
template<class T, class S>
T FFTServer<T,S>::cxWtsum(Array<T> &Weight)
{
  VectorIterator<T> wt_iter(Weight);
  Int start_wt, end_wt;
  wt_iter.vector().origin(start_wt);
  wt_iter.vector().end(end_wt);
  
  T ret_value = T(0.0);
  while (! wt_iter.pastEnd()) {
    Vector<T> &wt_temp = wt_iter.vector();
    for(Int i=start_wt; i <= end_wt; i++) 
      ret_value += wt_temp(i);
    wt_iter.next();
  }
  return ret_value;
}

// This function sums up image weights for FFT normalization
template<class T, class S>
T FFTServer<T,S>::imWtsum(Array<T> &Weight)

{
  VectorIterator<T> wt_iter(Weight);
  Int start_wt, end_wt;
  wt_iter.vector().origin(start_wt);
  wt_iter.vector().end(end_wt);
  
  T ret_value = T(0.0);
  while (! wt_iter.pastEnd()) {
    Vector<T> &wt_temp = wt_iter.vector();
    for(Int i=start_wt; i <= end_wt; i++) 
      ret_value += wt_temp(i);
    wt_iter.next();
  }
  return ret_value;
}

// Divides the n-dimensional object into 2^n parts and flips each part
// with the part that diametrically opposes it; i.e. for Vector, flips
// halves; for a Matrix, flips quadrants; for a Cube, flips octants;
// and so on.
// if parity == 1, then the center pixel in an odd dimension will end up
// at the zero pixel. Otherwise, the center pixel + 1 will end up at the
// zero pixel.
// If image_type != DEF_IMAGE_TYPE, then the image is assumed to have
// two extra columns of Nyquist data, and only the non-Nyquist part
// will be flipped.
// <todo> The current algorithm has performance O(N*d) where N is the
// number of elements in image, and d is the number of dimensions.
// Most of the work is copying, much of which could be avoided, as
// follows. If image has dimensions n1,n2,n3,...,nk, then image is
// isomorphic to the external direct product P of Z[n1],Z[n2],...,Z[nk].
// Let c be an abitrary shift. Then <c> is a normal subgroup of P; in
// fact, the cosets of <c> form the factor group of P by <c>. Now...
// when we do a shift, then we take the element at (0,0,..,0) and put
// it in (c)... we take (c) and put it in (c+c)... The last one in <c>
// goes into (0,0,...,0). Now, take the next coset and do the same
// thing. Once you've done all the cosets (there will be |P|/|<c>| of
// them, by Lagrange's Theorem), you will have shifted the entire
// image, not by N/2, but by an abitrary amount, in constant time,
// assuming that your method of finding the coset leaders can be done
// in constant time. Problem: given <c>, how do you find the next
// coset leader in constant time and space?
// </todo>
template<class T, class S>
void
FFTServer<T,S>::flipImage(Array<T> &image, int image_type, int parity)


{
  ArrayIterator<T> iterator(image, 1);
  Int iterations = image.nelements() / iterator.array().nelements();
  IPosition Start =  iterator.array().origin();
  IPosition End   =  iterator.array().end();
  IPosition Middle = iterator.array().shape();
  int odd = Middle(0) % 2;

  Middle(0) /= 2;
  Middle -= 1;
  Middle += Start;
  
  if(image_type != fftparms::DEF_IMAGE_TYPE){
    End(0) -= 2;
    Middle(0) -= 1;
  }

  IPosition Middle_Plus_One(1);
  Middle_Plus_One = 0;
  Middle_Plus_One(0) = Middle(0) + 1;


// When the first dimension is of even size, MiddleOdd == Middle, and
// Middle_Plus_OneOdd == Middle_Plus_One. Otherwise, MiddleOdd(0) ==
// Middle(0) + 1 and Middle_Plus_OneOdd(0) == Middle_Plus_One(0) + 1 and
// all other dimensions are equal.


  IPosition Middle_Plus_OneOdd( Middle_Plus_One );
  IPosition MiddleOdd( Middle );
  if (odd) {
    MiddleOdd(0) += 1;
    Middle_Plus_OneOdd(0) +=1;
  }

  //exchange halves of iterator.array();
  for(Int i=0; i<iterations; i++) {
    Array<T> temp;
    if (parity) { // convention: center pixel is phase center
      temp = iterator.array()(Middle_Plus_One, End);
      iterator.array()(Middle_Plus_OneOdd,End) = 
	iterator.array()(Start, Middle);
      iterator.array()(Start, MiddleOdd) = temp;
    } else { // convention: center pixel + 1 is phase center
      temp = iterator.array()(Start, MiddleOdd);
      iterator.array()(Start, Middle) = 
	iterator.array()(Middle_Plus_OneOdd, End);
      iterator.array()(Middle_Plus_One, End) = temp;
    }
    if(!iterator.pastEnd())
      iterator.next();
  }
  exchangeUV(image, parity);
}

// Divides UV in to 2^(n-1) pieces, and exchanges each piece with the
// component that diametrically opposes it. Like flipImage, but does
// not flip in u dimension. If parity==1, then the center pixel in an
// odd dimension will be moved to the origin. Otherwise, the center
// pixel + 1 will end up at the zero pixel.
template<class T, class S>
void
FFTServer<T,S>::exchangeUV(Array<T> &UV, int parity)
{

  for(Int current_dim = 2; current_dim <= UV.ndim(); current_dim++){
    //iterate dimension "current_dim".
    ArrayIterator<T> iterator(UV, current_dim);
    Array<T> &array = iterator.array();
    IPosition Start =  array.origin();
    IPosition End   =  array.end();
    IPosition Middle = array.shape();
    int odd = Middle(current_dim - 1) % 2;

    Middle(current_dim - 1) /= 2;
    Middle -= 1;
    Middle += Start;
    IPosition Middle_Plus_One(Middle.nelements());
    Middle_Plus_One = 0;
    Middle_Plus_One(current_dim - 1) = Middle(current_dim - 1) + 1;

    IPosition Middle_Plus_OneOdd( Middle_Plus_One );
    IPosition MiddleOdd( Middle );
    if (odd) {
      MiddleOdd(current_dim - 1) += 1;
      Middle_Plus_OneOdd(current_dim - 1) +=1;
    }
    Int iterations = UV.nelements() / array.nelements();
    for(Int i=0; i<iterations; i++){
      //exchange halves of iterator.array();
      if (parity) {
	Array<T> temp = array(Middle_Plus_One, End).copy();
	array(Middle_Plus_OneOdd, End) = array(Start, Middle);
	array(Start, MiddleOdd) = temp;
      } else {
	Array<T> temp = array(Start, MiddleOdd).copy();
	array(Start, Middle) = array(Middle_Plus_OneOdd, End);
	array(Middle_Plus_One, End) = temp;
      }
      if(!iterator.pastEnd())
	iterator.next();
    }
  }
}

// Given an n-dimensional image: This function divides the image into 2^n
// parts and flips each part with the part that diametrically opposes it.
// ie for a Matrix, flip quadrants, for a cube flip octants.  if parity
// == 1, then the center pixel in an odd dimension will end up at the
// zero pixel. Otherwise, the center pixel + 1 will end up at the zero
// pixel.

template<class T, class S>
void
FFTServer<T,S>::flipImage(Array<S> &image, int image_type, int parity)
{
  ArrayIterator<S> iterator(image, 1);
  Array<S> &array = iterator.array();
  Int iterations = image.nelements() / iterator.array().nelements();
  IPosition Start =  array.origin();
  IPosition End   =  array.end();
  IPosition Middle = array.shape();
  int odd = Middle(0) % 2;

  Middle(0) /= 2;
  Middle -= 1;
  Middle += Start;
  if(image_type != fftparms::DEF_IMAGE_TYPE){
    End(0) -= 1;
    Middle(0) = (array.shape()(0) - 1)/ 2;
  }
  IPosition Middle_Plus_One(1);
  Middle_Plus_One = 0;
  Middle_Plus_One(0) = Middle(0) + 1;

// When the first dimension is of even size, MiddleOdd == Middle, and
// Middle_Plus_OneOdd == Middle_Plus_One. Otherwise, MiddleOdd(0) ==
// Middle(0) + 1 and Middle_Plus_OneOdd(0) == Middle_Plus_One(0) + 1 and
// all other dimensions are equal.

  IPosition Middle_Plus_OneOdd( Middle_Plus_One );
  IPosition MiddleOdd( Middle );
  if (odd) {
    MiddleOdd(0) += 1;
    Middle_Plus_OneOdd(0) +=1;
  }

  //exchange halves of iterator.array();
  for(Int i=0; i<iterations; i++){
    if (parity) {
      Array<S> temp = array(Middle_Plus_One, End).copy();
      array(Middle_Plus_OneOdd, End) = array(Start, Middle);
      array(Start, MiddleOdd) = temp;
    } else {
      Array<S> temp = array(Start, MiddleOdd).copy();
      array(Start, Middle) = array(Middle_Plus_OneOdd, End);
      array(Middle_Plus_One, End) = temp;
    }
    if(!iterator.pastEnd())
      iterator.next();
  }
  exchangeUV(image, parity);
}

// This function exchanges column uv locations for FFT so that data fed
// to FFT are in frequency order 0 ... N/2 -1, Nyquist ... -1 if
// parity==1, then the center pixel in an odd dimension will end up at
// the zero pixel. Otherwise, the center pixel + 1 will end up at the
// zero pixel.
template<class T, class S>
void
FFTServer<T,S>::exchangeUV(Array<S> &UV, int parity)
{

  for(Int current_dim = 2; current_dim <= UV.ndim(); current_dim++){
    //iterate dimension "current_dim".
    ArrayIterator<S> iterator(UV, current_dim);
    Array<S> &array = iterator.array();
    IPosition Start =  array.origin();
    IPosition End   =  array.end();
    IPosition Middle = array.shape();
    int odd = Middle(current_dim - 1) % 2;

    Middle(current_dim - 1) /= 2;
    Middle -= 1;
    Middle += Start;
    IPosition Middle_Plus_One(Middle.nelements());
    Middle_Plus_One = 0;
    Middle_Plus_One(current_dim - 1) = Middle(current_dim - 1) + 1;

    IPosition Middle_Plus_OneOdd( Middle_Plus_One );
    IPosition MiddleOdd( Middle );
    if (odd) {
      MiddleOdd(current_dim - 1) += 1;
      Middle_Plus_OneOdd(current_dim - 1) +=1;
    }

    Int iterations = UV.nelements() / array.nelements();
    for(Int i=0; i<iterations; i++){
      //exchange halves of iterator.array();
      if (parity) {
	Array<S> temp = array(Middle_Plus_One, End).copy();
	array(Middle_Plus_OneOdd, End) = array(Start, Middle);
	array(Start, MiddleOdd) = temp;
      } else {
	Array<S> temp = array(Start, MiddleOdd).copy();
	array(Start, Middle) = array(Middle_Plus_OneOdd, End);
	array(Middle_Plus_One, End) = temp;
      }
      if(!iterator.pastEnd())
	iterator.next();
    }
  }
}

template<class T, class S>
T FFTServer<T,S>::scaleFactor(void)
{
  return scale;
}

// N dimensional fft on the real unpacked array rdata. Does not
// perform image flipping. If dir > 0, forward transform. If dir<=0,
// backward transform. If do_scale zero, do not normalize after
// backward transform, otherwise do.
template<class T, class S>
void
FFTServer<T,S>::rndfft(Array<T> &rdata, int dir, int do_scale)
{

  if (rdata.ndim() != dimension ) {
    throw(FFTError("FFTServer::rndfft(...) - error: wrong ffttool for dimension "
		   " of array"));
  }
  // get work and rdata storage
  Bool rdelete, wdelete, ndelete;
  T *wptr = work.getStorage(wdelete);
  T *nptr = Nyquist.getStorage(ndelete);
  T *rptr = rdata.getStorage(rdelete);
  int nelements = rdata.nelements();
  int nyelements = Nyquist.nelements();

  T *roff, *noff, *toff;
  T *rend = rptr + nelements;
  T *nend = nptr + nyelements;

  IPosition shape(rdata.shape());
  int ndims = shape.nelements();

  // nrows is the number along the u
  // axis, not the number of rows...
  int nrows = shape(0);
  if (nrows == 0) {
    throw(FFTError("FFTServer::rndfft(...) first dimension of "
		   "size zero in rdata"));
  }
  // used by transform loops
  int i, j;
  long prod;
  int upper, current;
  long roffset, noffset, toffset;
  
  int even = !(shape(0) % 2);    

  int endoneven = even ? nrows : nrows - 1;
  int skip, x;

  // what the FORTRAN mfft takes. Sign is 
  // sign of complex exponent: -1 is forward.
  int mfftdir;

// Set prod to 2 * true product of dimensions.  In a packed array, the
// imaginary part of the zero component is missing, and in the even case,
// the last one is too. It's a a factor of two larger, since the elements
// are in re,im, re,im pairs--- each pair is one element in that
// dimension.


  prod = nrows + 1 + even;

  for (i = 1 ; i < ndims; ++i) {
    // get product of all dimensions, and maximum 
    // dimension length
    if (shape(i) == 0) {
      throw(FFTError("FFTServer::rndfft(...) dimension of"
		     "size zero in rdata"));
    }
    prod *= shape(i);
  }

  // initialize work storage
  rffti( &nrows, wptr );

  if (dir > 0) { 
    // forward transform
    upper = prod / (nrows + 1 + even);
    // upper is now number of rows
    for ( noff = nptr, roff = rptr; roff < rend; roff += nrows, noff += 2 ) {
      rfftf( &nrows, roff, wptr );

      // toff points to last element in this row
      toff = roff + nrows - 1;
      if (even) {
	// Stick last element into real Nyquist,
	// zero into imaginary
	*noff = *toff;
	*(noff + 1) = T(0.0);
      } else {
	// Stick next to last into real Nyquist for
	// convenience (see below)
	// and last into imaginary
	*noff = *(toff - 1);
	*(noff + 1) = *toff;
      }
      // shuffle rest along
      for ( ; toff > roff; --toff ) {
	*toff = *(toff - 1);
      }
      // zero into second element
      *(roff + 1) = T(0.0);
    }
  } else { // backward transform
    scale = 1.0 / nelements;

    // backward transform, copy for sanity's 
    // sake. Shouldn't be necessary, but will anyway
    if (!even) { 
      // copy last element to real Nyquist
      for ( roff = rptr + nrows - 1, noff = nptr; roff < rend; roff += nrows, noff +=2 ) {
	*noff = *roff;
      }
    }
  }

// other dimensions, same for both forward and backward transforms. We
// use the Nyquist array in assuming that it holds 1. for first dimension
// even: real and imaginary Nyquist 2. for first dimension odd: real and
// imaginary components of (N+1/2)

// Algorithm: for each dimension, perform an FFT on every vector in that
// dimension.  A vector in a dimension i is a set of elements generated
// by varying over index i while holding all other indexes fixed.

// For example, a 3 x 4 array will have 4 rows and 3 columns. A column
// [vector in the second dimension] can be generated by choosing the
// elements (2,j), where 0<=j<4.

// The FORTRAN routines mfft and mdfft allow us to specify an indexing
// increment between succesive elements. Thus we perform an in-place FFT
// on an abitrary vector in the array by passing a pointer to its first
// element and an appropriate indexing skip.


  for ( skip = nrows, i = 1; i < ndims; ++i) {
    // for all other dimensions
    current = shape(i);
    skip *= current;
    upper = prod / (2 * current);
    roffset = noffset = 0;

// upper is the number of vectors in this dimension skip is the number of
// elements in rdata between the start of this vector and the start of
// the next vector in this dimension

// only difference in transforms: coefficient to mfft. Sign is sign of
// complex exponential magnitude is increment between successive elements
// in the vector.

    if (dir > 0) { //forward
      mfftdir = -skip / current;
    } else { //backward
      mfftdir = skip / current;
    }

// nyskip is the increment between successive elements in this vector, in
// Nyquist.
// skip along nyquist array is different than mfftdir in this way:
    int nyskip = (mfftdir / nrows) * 2;

    for (j = 0; j < upper; ++j) {
      // for all these vectors
      x = roffset % nrows;
      if (x == endoneven) {
// only happens when nrows is odd blast the Nyquist, we're at the end
// conenience: I use the nyquist for both, when the real is duplicated in
// the last column of rdata
// <note role=danger> Assumes that layout of a complex object is re, im,
// re, im, in FORTRAN order </note>
	mfft( nptr + noffset, nptr + noffset + 1, &current, &current, &current, &nyskip );
	noffset += (skip / nrows) * 2;
	// check and wraparound
	if (noffset >= nyelements) {
	  // point to next pair
	  noffset -= nyelements - 2;
	}
	// check and wraparound
	roffset += skip;
	if (roffset >= nelements) {
// wraparound, then plus 1, because nrows is odd, and we're skipping the
// real component at the end of the row--- the real in the last element
// is duplicated in the the real of the nyquist.  It's easier to use the
// nyquist for this call to mfft, for both the real and imaginary
// components.  Why? Suppose we used a pointer to the last column of
// rdata for the real part, and a pointer into the imaginary column of ny
// for the imaginary part. Then the magnitude of nyskip would only work
// for one of them, since the skip between successive elements depends on
// the first dimension, which is in general different between ny and
// rdata.
	  roffset -= nelements - 1;
	} 
      } else {
	// all the rest
	  // <note role=danger> Assumes that layout of a complex
	  // object is re, im, re, im in FORTRAN order </note>
	mfft( rptr + roffset, rptr + roffset + 1, 
	     &current, &current, &current, &mfftdir );
	roffset += skip;
	if (roffset >= nelements) {
	  // wrap around to next pair
	  roffset -= nelements - 2;
	}
	if (x == 0 && even) {
	  // blast the Nyquist as well in the even case
	  // <note role=danger> Assumes that layout of a complex
	  // object is re, im, re, im in FORTRAN order</note>
	  mfft( nptr + noffset, nptr + noffset + 1, &current,
	       &current, &current, &nyskip );
	  noffset += (skip / nrows) * 2;
	  if (noffset >= nyelements) {
	    // wrap around to next pair
	    noffset -= nyelements - 2;
	  }	    
	  // count one extra for our Nyquist friend
	  ++j;
	}
      }
    }
  } 
  if (dir > 0) { // forward, last thing to do
    if (!even) {
      // copy real Nyquist to last element
      for ( roff = rptr + nrows - 1, noff = nptr; 
	   roff < rend; roff += nrows, noff +=2 ) {
	*roff = *noff;
      }
    }

  } else { // backward, now do rows
    upper = prod / (nrows + 1 + even);
    // upper is now number of rows
    for ( noff = nptr, roff = rptr, i = 0; i < upper;
	 ++i, roff += nrows, noff += 2 ) {
      // shuffle rest along; roff[1] should be zero
      for ( toff = roff + 1; toff < roff + nrows - 1; ++toff ) {
	*toff = *(toff + 1);
      }

      // now toff == roff + nrows - 1, i.e. last element
      if (even) {
	// Stick Nyquist real into last element in row.
	// Nyquist imaginary should be zero.
	*toff = *noff;
      } else {
	// Stick real Nyquist in next to last 
	// Stick imaginary Nyquist into last
	*(toff - 1) = *noff;
	*toff = *(noff + 1);
      }
      rfftb( &nrows, roff, wptr );
      // now scale them. After transform to reduce roundoff
      // error
      if (do_scale) {
	// scale Nyquist
	*noff *= scale;
	*(noff + 1) *= scale;
	// toff now points to roff + nrows - 1
	for ( ; toff >= roff ; --toff ) {
	  *toff *= scale;
	}
      }
    }      
  }
  // put the storage back.
  rdata.putStorage(rptr, rdelete);
  work.putStorage(wptr, wdelete);
  Nyquist.putStorage(nptr, ndelete);
}	


// Real to real transform on rdata. Rdata is expected to be of dimension
// (n1+2) x n2 x n3 x ... x nm Although this is one more column than is
// needed for the the u odd case, it is unambiguous. Given n1 = 8, we
// know that the true dimension is 6. If dir >0, forward transform. If
// dir <=0, backward transform. If do_scaleno is nonzero, normalize after
// a backward transform, otherwise do not normalize.
template<class T, class S>
void
FFTServer<T,S>::rndnyfft(Array<T> &rdata, int dir, int do_scale)

{
  if (rdata.ndim() != dimension ) {
    throw(FFTError("FFTServer::rndnyfft(...) - error: wrong ffttool for dimension "
		   " of array"));
  }
  // get work and rdata storage
  Bool rdelete, wdelete, ndelete;
  T *wptr = work.getStorage(wdelete);
  T *rptr = rdata.getStorage(rdelete);
  T *nptr = Nyquist.getStorage(ndelete);
  int nelements = rdata.nelements();

  T *roff, *toff, *noff;
  T *rend = rptr + nelements;
  T *nend = nptr + Nyquist.nelements();

  IPosition shape(rdata.shape());
  int ndims = shape.nelements();
  int nrows = shape(0);

  // used by transform loops
  int i, j;
  long prod;
  int upper, current;
  long roffset, noffset, toffset;

  int even = shape(0) % 2 ? 0 : 1;    

  int endoneven = even ? nrows : nrows - 1;
  int skip, x;

// what the FORTRAN mfft takes. Sign is sign of complex exponent: -1 is
// forward.
  int mfftdir;

// prod is 2 * true product of dimensions in odd case, we have an extra
// column

// truenumelements is the true number of elements in the object --
// subtract two for the first dimension

  prod = nrows - !even;
  int truenumelements = nrows - 2;
  for (i = 1 ; i < ndims; ++i) {
    // get product of all dimensions
    truenumelements *= shape(i);
    prod *= shape(i);
  }

  // initialize work storage

  int tmp = nrows - 2;
  rffti( &tmp, wptr );

  if (dir > 0) { 
    // forward transform
    for ( roff = rptr; roff < rend; roff += nrows ) {
      rfftf( &tmp, roff, wptr );

      toff = roff + nrows - 2;

      // shuffle elements along
      for ( ; toff > roff; --toff ) {
	*toff = *(toff - 1);
      }

      if (even) {
	// Stick zero into imaginary part of ``Nyquist''
	*(roff + nrows - 1) = T(0.0);
      } 

      // zero into second element
      *(roff + 1) = T(0.0);
    }
  } else { // backward transform
    scale = 1.0 / truenumelements;
    for ( roff = rptr; roff < rend; roff += nrows ) {
      if (!even) {
	// copy imaginary that sits in 
	// very last element to imaginary of
	// last element
	*(roff + nrows - 2) = *(roff + nrows - 1);
      } 
    }
  }

// Algorithm: for each dimension, perform an FFT on every vector in that
// dimension.  A vector in a dimension i is a set of elements generated
// by varying over index i while holding all other indexes fixed.

// For example, a 3 x 4 array will have 4 rows and 3 columns. A column
// [vector in the second dimension] can be generated by choosing the
// elements (2,j), where 0<=j<4.

// The FORTRAN routines mfft and mdfft allow us to specify an indexing
// increment between succesive elements. Thus we perform an in-place FFT
// on an abitrary vector in the array by passing a pointer to its first
// element and an appropriate indexing skip.
  for ( skip = nrows, i = 1; i < ndims; ++i) {
    // for all other dimensions
    current = shape(i);
    skip *= current;
    upper = prod / (2 * current);
    roffset = noffset = 0;

// upper is the number of vectors in this dimension skip is the number of
// elements in rdata between the start of this vector and the start of
// the next vector in this dimension

// only difference in transforms: coefficient to mfft. Sign is sign of
// complex exponential magnitude is the increment between successive
// elements in this vector

    if (dir > 0) { //forward
      mfftdir = -skip / current;
    } else { //backward
      mfftdir = skip / current;
    }
    for (j = 0; j < upper; ++j) {
      // for all these vectors
      x = roffset % nrows;
      if (x == endoneven) {
	// only when nrows is odd
	roffset += skip;
	if (roffset >= nelements) {
	  // skip around, then plus 1
	  // because we're ignoring the 
	  // last column in rdata
	  roffset -= nelements - 1;
	} else {
	  /* ok, nothing */
	}
      }
// all the rest <note role=danger> Assumes that layout of a complex
// object is re, im, re, im in FORTRAN order</note>

      mfft( rptr + roffset, rptr + roffset + 1, &current, &current, &current, &mfftdir );
      roffset += skip;
      if (roffset >= nelements) {
	// wrap around and point to the
	// next pair.
	roffset -= nelements - 2;
      }
    }
  } 
  if (dir > 0) { // forward, last thing to do
    // copy last two columns into Nyquist, for
    // convenience's sake
    for ( roff = rptr, noff = nptr; roff < rend; roff += nrows, noff +=2 ) {
      if (!even) {
	// if it's not even, move re and im for last element
	// into the last two positions in the row. This duplicates
	// the real part. 
	*(roff + nrows - 1) = *(roff + nrows - 2);
	*(roff + nrows - 2) = *(roff + nrows - 3);
      }
      *noff = *(roff + nrows - 2);
      *(noff + 1) = *(roff + nrows - 1);
    }
  } else { // backward, now do rows
    int tmp = nrows - 2;
    rffti( &tmp, wptr );
    for ( roff = rptr; roff < rend; roff += nrows ) {
      // shuffle rest along; roff[1] should be zero
      for ( toff = roff + 1; toff < roff + nrows - 1; ++toff ) {
	*toff = *(toff + 1);
      }
      // now toff == roff + nrows - 1, i.e. last element
      
      rfftb( &tmp, roff, wptr );
      // now scale them. After transform to reduce roundoff
      // error
      if (do_scale) {
	toff = roff + nrows - 1;
	for ( ; toff >= roff ; --toff ) {
	  *toff *= scale;
	}
      }
    }      
  }
  // put the data back
  rdata.putStorage(rptr, rdelete);
  work.putStorage(wptr, wdelete);
  Nyquist.putStorage(nptr, ndelete);
}	



// Real to real n dimensional fft. Image flipping is performed. rdata
// is assumed to be in unpacked format, and is returned in unpacked
// format. If dir >=0, forward transform. If dir <=0, backward
// transform. If do_scale is zero, then no normalization is done after
// a reverse transform; otherwise rdata is normalized after a reverse
// transform. 
template<class T, class S>
void
FFTServer<T, S>::rrfft(Array<T> &rdata, int dir, int do_scale)
{
  int config = rdata.ndim();
  if (config != dimension ) {
    throw(FFTError("FFTServer::fft() - error: wrong ffttool for dimension "
		   " of array"));
  }
  if (dir <= 0) { // reverse transform
    if (dimension > 1) {
      exchangeUV(rdata);
      exchangeUV(Nyquist);
    }
  } else {
    flipImage(rdata);
  }

  rndfft(rdata, dir, do_scale);

  if (dir > 0 ) {
    if (dimension > 1) {
      // phase center is mid point
      exchangeUV(rdata, 0);
      exchangeUV(Nyquist, 0);
    }
  } else {
    // phase center is mid point
    flipImage(rdata, 0, 0);
  }
}


// Complex to complex multi-dimensional fft, with separate real and
// imaginary components. Calls FORTRAN code directly. Rdata is the real
// component, and cdata is the imaginary component. Arrays are assumed to
// be in unpacked format, and are returned in unpacked format. No
// reference to the Nyquist array is made. If dir > 0, the transform is
// forward. If dir<=0, the transform is backward. If do_scale is zero,
// then do not normalize after a backward transform, otherwise do. Does
// not do image flipping.
template<class T, class S>
void
FFTServer<T, S>::rcndfft(Array<T> &rdata, Array<T> &cdata, int dir, int do_scale)
{
  // rdata and cdata must conform
  if (!rdata.conform(cdata)) {
    throw(FFTError("FFTServer<T,S>::rcndfft(...):rdata and cdata no not conform"));
  }
  int nelems = rdata.nelements();

  scale = nelems ? 1.0 / nelems : 1.0;
  IPosition Shape(rdata.shape());
  
  int i;
  Bool rDelete, cDelete;
  T *rptr = rdata.getStorage(rDelete);
  T *cptr = cdata.getStorage(cDelete);

  int dims = Shape.nelements();
  int prod = 1;
// Sense of dir: for FFTServer, if d >= 0, it's a forward transform. This
// means the sign of the exponential is negative, and that's what mfft
// takes.  The magnitude (1 here) is the indexing skip. See the FORTRAN.

  int isn = dir > 0 ? -1 : 1;
  int tmp;
  for (i = 0; i < dims; ++i) {
// for each dimension
    prod *= tmp = Shape(i);
	  // <note role=danger> Assumes that layout of a complex
	  // object is re, im, re, im in FORTRAN order</note>
    mfft(rptr, cptr, &nelems, &tmp, &prod, &isn);
    if (isn==0) { // storage exceeded in mfft
      // should never happen
      throw(FFTError("FFTServer<T,S>::newndfft(...):"
		     "Static storage limits exceeded in mfft"));
    }
  }
  if (dir <= 0 && do_scale) { // backward, do the scale
    for (int off=0; off < nelems; ++off) {
      *(rptr + off) *= scale;
      *(cptr + off) *= scale;
    }
  }
  rdata.putStorage(cptr, cDelete);
  cdata.putStorage(rptr, rDelete);

}

// Complex to complex n dimensional FFT. Calls FORTRAN code directly.
// Does no image flipping. Expects cdata to be unpacked, and returns
// it unpacked. If dir > 0, it's a forward transform. If dir <=0, it's
// a backward transform. If do_scale is zero, then cdata is not
// normalized after a backward transform, otherwise it is.
template<class T, class S>
void
FFTServer<T, S>::cndfft(Array<S> &cdata, int dir, int do_scale)
{
  
  IPosition Shape(cdata.shape());

  Bool Delete;
  S *rptr = cdata.getStorage(Delete);
  
  int dims = Shape.nelements();
  int prod = 1;
// Sense of dir: for FFTServer, if > 0, it's a forward transform. This
// means the sign of the exponential is negative, and that's what mfft
// takes.  the magnitude is the indexing skip. See the FORTRAN.
  int isn = dir > 0 ? -2 : 2;
  int nelems = cdata.nelements();
  scale = nelems ? 1.0 / nelems : 1.0;
  int tmp;
  for (int i = 0; i < dims; ++i) {
// for each dimension
    prod *= tmp = Shape(i);
// <note role=danger> Assumes that layout of a complex object is re, im,
// re, im in FORTRAN order</note>
    mfft((T*) rptr, ((T*) rptr)+1, &nelems, &tmp, &prod, &isn);
    if (isn==0) { // storage exceeded in mfft
      // should never happen
      throw(FFTError("FFTServer<T,S>::newcndfft(...)::Static storage limits exceeded in mfft"));
    }
  }
  if (dir <= 0 && do_scale) { // backward
    for (int off=0; off < nelems; ++off) {
      *(rptr + off) *= scale;
    }
  }
  cdata.putStorage(rptr, Delete);
}

