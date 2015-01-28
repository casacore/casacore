//# LatticeTwoPtCorr.h: compute two-point correlation functions from a lattice
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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
//#
//# $Id$

#ifndef LATTICES_LATTICETWOPTCORR_H
#define LATTICES_LATTICETWOPTCORR_H

//# Includes
#include <casacore/casa/aips.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

template <class T> class MaskedLattice;
template <class T> class Lattice;
class IPosition;
class LogIO;
class String;

// <summary>
// Compute two point auto-correlation functions from a lattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="MaskedLattice">MaskedLattice</linkto>
// </prerequisite>

// <synopsis>
// This class allows you to compute two point correlation functions
// from lattices over planes of the specified two axes.
// At present, only autocorrelation is implemented and only
// the structure function is available.
//
// The structure function is
// <src>S(x,y) = < [lat(i,j) - lat(i+x,j+y)]**2 ></src>
// where x and y are absolute integer shifts (or lags).
// </synopsis>
// <example>
// <srcblock>
// </srcblock>
// </example>


// <todo asof="yyyy/mm/dd">
// <li> Add additional algorithms other than the structure function
// <li> Allow cross correlation algorithms as well as autocorrelation
// </todo>


template <class T> class LatticeTwoPtCorr
{
public:

enum Method {

// Undefined
   UNDEFINED,

// Structure Function
   STRUCTUREFUNCTION,

// nMethods
   NMETHODS
};


// Default constructor
   LatticeTwoPtCorr()
   {}

// Destructor
   ~LatticeTwoPtCorr()
   {}

// Compute specified autocorrelation function for the planes of the given TWO axes.
// If the output lattice has a mask, it will first be set to False (bad)
// and then any output pixel with some contributing values will be set to
// True (good).
// <group>
   void autoCorrelation (MaskedLattice<T>& out, const MaskedLattice<T>& in,
                         const IPosition& axes, Method method,
                         Bool showProgress=True) const;
// </group>

// Helper function to provide output lattice shape give the input shape
// and the axes to find the structure function over.
   static IPosition setUpShape (const IPosition& inShape, const IPosition& axes);

// Helper functions to convert method types to and from strings
// <group>
   static Method fromString (const String& method);
   static String toString (Method method);
// </group>

private:
// Function Pointer typedef
   typedef T (LatticeTwoPtCorr<T>::*FuncPtr)(T d1, T d2) const;

// Do the iteration work 
   void autoCorrelation (MaskedLattice<T>& out, const MaskedLattice<T>& in,
                         const IPosition& axes, 
                         FuncPtr,                
                         Bool showProgress) const;

// Check Output lattice shape
   void check (LogIO& os, const MaskedLattice<T>& latOut,
               const MaskedLattice<T>& latIn,
               const IPosition& axes) const;

// Compute structure function
  T structureFunction (T d1, T d2) const {return (d1-d2)*(d1-d2);}
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeTwoPtCorr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
