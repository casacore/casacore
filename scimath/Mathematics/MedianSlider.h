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

#ifndef SCIMATH_MEDIANSLIDER_H
#define SCIMATH_MEDIANSLIDER_H

//#! Includes go here

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
  float add      ( float d,bool flag=false );
// Adds a flagged datum
  float add      ()                          { return add(0,true); }
// Adds N flagged datums
  float next     ( uint32_t n=1 );
// Adds several datums at once (with corresponding flags)
  float add      ( const Vector<float> &d,const Vector<bool> &flag );
// Adds several non-flagged datums at once
  float add      ( const Vector<float> &d );
  
// Returns the number of values currently in the window. This is less
// than the window width initially.
//  int32_t  size    ();    

// Returns the number of non-flagged values in window
  int32_t  nval    ();
  
// Returns the current median value  
  float median ();

// Returns a previous value (from n steps ago) from the sliding window
  float prevVal  ( uint32_t n,bool &flag );

// Returns value from midpoint (center) of window, possibly with flag
  float midpoint ( bool &flag );
  float midpoint ()               
          { bool dum; return midpoint(dum); }

// Returns the difference between the current median and the value
// at window center. Optionally, also returns flag of median center
  float diff ( bool &flag )       { return midpoint(flag) - median(); }
  float diff ()                   
           { bool dum; return diff(dum); }

// returns total memory usage (in bytes) for a given halfwin size 
  static size_t objsize ( int halfwin )
  { return sizeof(MedianSlider)+(sizeof(float)+sizeof(uint32_t)+sizeof(bool))*(halfwin*2+1); }
  
// For testing purposes only: verifies current value of median.
// Throws an exception if it fails.
  bool  assure ();

private:

  uint32_t   halfwin,fullwin;
  float *buf;
  uint32_t  *index;
  bool  *valid;
  uint32_t   ibuf,nind;
  
};


inline int32_t MedianSlider::nval ()
{
  return nind;
}

inline float MedianSlider::median () 
{
  if( !nind )
    return 0;
  return nind%2 ? buf[ index[nind/2] ] 
      : ( buf[ index[nind/2-1] ] + buf[ index[nind/2] ] )/2;
//  return nind%2 ? buf[ index[nind/2] ] 
//      : buf[ index[nind/2-1] ];
}

inline float MedianSlider::midpoint ( bool &flag ) 
{
  return prevVal(halfwin+1,flag);
}



} //# NAMESPACE CASACORE - END

#endif
