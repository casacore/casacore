//# MedianSlider.h: Optimized sliding-median computator
//# Copyright (C) 2000,2001
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

#ifndef SCIMATH_MEDIANSLIDER_H
#define SCIMATH_MEDIANSLIDER_H

//#! Includes go here

#include <casa/Arrays/Vector.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations

// <summary>
// Class to compute sliding median
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// MedianSlider is a class for efficient computing of sliding medians.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Flagging Agents make extended use of sliding medians.
// </motivation>
//
// <todo asof="yyyy/mm/dd">
//   <li> think about a 2D sliding median
// </todo>

class MedianSlider
{
public:
    
  MedianSlider  ();
  MedianSlider  ( int halfwin );
  MedianSlider  ( const MedianSlider &other );
  ~MedianSlider ();
  MedianSlider & operator = ( const MedianSlider &other );
  
  void cleanup ();

// Adds a datum to the slider. Once the window is full, newer values will 
// push out older values. Returns the new median value.
// If flag is set to true, adds a "flagged" datum, one which takes
// up space in the window but is skipped during median computations.
  Float add      ( Float d,Bool flag=False );
// Adds a flagged datum
  Float add      ()                          { return add(0,True); }
// Adds N flagged datums
  Float next     ( uInt n=1 );
// Adds several datums at once (with corresponding flags)
  Float add      ( const Vector<Float> &d,const Vector<Bool> &flag );
// Adds several non-flagged datums at once
  Float add      ( const Vector<Float> &d );
  
// Returns the number of values currently in the window. This is less
// than the window width initially.
//  Int  size    ();    

// Returns the number of non-flagged values in window
  Int  nval    ();
  
// Returns the current median value  
  Float median ();

// Returns a previous value (from n steps ago) from the sliding window
  Float prevVal  ( uInt n,Bool &flag );

// Returns value from midpoint (center) of window, possibly with flag
  Float midpoint ( Bool &flag );
  Float midpoint ()               
          { Bool dum; return midpoint(dum); }

// Returns the difference between the current median and the value
// at window center. Optionally, also returns flag of median center
  Float diff ( Bool &flag )       { return midpoint(flag) - median(); };
  Float diff ()                   
           { Bool dum; return diff(dum); }

// returns total memory usage (in bytes) for a given halfwin size 
  static size_t objsize ( int halfwin )
  { return sizeof(MedianSlider)+(sizeof(Float)+sizeof(uInt)+sizeof(Bool))*(halfwin*2+1); }
  
// For testing purposes only: verifies current value of median.
// Throws an exception if it fails.
  Bool  assure ();

private:

  uInt   halfwin,fullwin;
  Float *buf;
  uInt  *index;
  Bool  *valid;
  uInt   ibuf,nind;
  
};


inline Int MedianSlider::nval ()
{
  return nind;
}

inline Float MedianSlider::median () 
{
  if( !nind )
    return 0;
  return nind%2 ? buf[ index[nind/2] ] 
      : ( buf[ index[nind/2-1] ] + buf[ index[nind/2] ] )/2;
//  return nind%2 ? buf[ index[nind/2] ] 
//      : buf[ index[nind/2-1] ];
}

inline Float MedianSlider::midpoint ( Bool &flag ) 
{
  return prevVal(halfwin+1,flag);
}



} //# NAMESPACE CASA - END

#endif
