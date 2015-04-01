//# DFTServer.h: This class contains methods for doing n-D slow Fourier transforms
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2003
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

#ifndef SCIMATH_DFTSERVER_H
#define SCIMATH_DFTSERVER_H


#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIter.h> 
#include <casacore/casa/Arrays/ArrayIO.h> 
#include <casacore/casa/BasicSL/Constants.h> 
#include <casacore/casa/math.h> 

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Matrix;

// <summary>
// Error class for <linkto class=DFTServer>DFTServer</linkto> class
// </summary>

// <synopsis>
// Error class for <linkto class=DFTServer>DFTServer</linkto> class.
// </synopsis>

class DFTError: public AipsError
{
public:
   DFTError(): AipsError("DFTError") {}
   DFTError(const Char *m) : AipsError(m) {}
   DFTError(const String &m) : AipsError(m) {}

   virtual ~DFTError() throw() {}
};

// <summary>
// Class containing methods for doing n-D slow Fourier transforms
// </summary>

// <synopsis>
// The DFTServer class contains methods for doing n-dimensional 
// Slow Fourier Transforms. (In practice, the maximum dimension is 3).
//
// </synopsis>

template<class T> 
class DFTServer 
{
public:
// default constructor
   DFTServer();

// copy constructor
   DFTServer(const DFTServer<T> &);

// Other constructors
// <group>
   DFTServer(Array<T> &, Array<T> &);
   DFTServer(int, int, int);
   DFTServer(IPosition &, IPosition &);
// </group>

// destructor
   ~DFTServer();

// assignment
   DFTServer<T>  &operator=(const DFTServer<T> &);

// n-d real <src> <-> </src> complex dft
   void rcdft(Array<T> &, Array<T> &);

// n-d complex <src> <-> </src> real dft
   void crdft(Array<T> &, Array<T> &);

// n-d complex <src> <-> </src> complex dft
   void cxdft(Array<T> &, Array<T> &, int);

// display only the real component of the data 
   void showReal(Array<T> &);

// display both the real and the imaginary components of the data 
   void showComplex(Array<T> &);

private:
   // dimension of the both input and output data
   int dimension;

   // number of time data points
   int numTime;

   // number of frequency data points
   int numFreq;

   // set to 1 (true) if a crfft is done
   int crFlag;


// does a complex to complex DFT
   void c2c(Matrix<T> &,  Matrix<T> &, int);

// turn a general array into a matrix
   Matrix<T> getMatrix(Array<T> &);

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/DFTServer.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif //DFT_SERVER
