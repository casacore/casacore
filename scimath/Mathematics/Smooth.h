//# Smooth.h: smooth vectors and arrays 
//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef SCIMATH_SMOOTH_H
#define SCIMATH_SMOOTH_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Smooth a Vector or the rows of a 2D Array taking into account
// flags which are supplied in a Vector/Array of the same shape.
// Modify the flags as necessary to mark channels for which the
// smoothing could not be done because needed channels were flagged.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Vector">Vector</linkto>
//   <li> <linkto class="Array">Array</linkto>
// </prerequisite>

// <etymology>
// self-explanatory
// </etymology>

// <synopsis>
// Perform smoothing on Vectors or Arrays
// </synopsis>

// <example>
// <srcBlock>
//    Smooth<Complex>::hanning(outv, // the output
//     			       outFlags, // the output mask
//			       yin, // the input
//			       yinFlags, // the input mask
//			       False,  // for flagging: good is not true
//                             True); // use the default scheme for producing output flags 
// </srcBlock>
// </example>

// <motivation>
// This is used, e.g., for visibilities in spectral channel vectors.
// </motivation>

// <todo asof="2010/07/27">
//   <li> 
// </todo>
 

  template <class T>
  class Smooth {

  Smooth(){}; 
  
  public:
  
  // Hanning smoothing
  static void hanning(Vector<T>& out, Vector<Bool>& outmask, 
		      Vector<T>& in, Vector<Bool>& mask, 
		      Bool TrueIsGood, Bool relaxed=True);
  
  // as above but calling hanningSmooth for each row of the 2D array
  static void hanning(Array<T>& out, Array<Bool>& outmask, 
		      Array<T>& in, Array<Bool>& mask, 
		      Bool TrueIsGood, Bool relaxed=True);
  };
  
  
} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/Smooth.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif

