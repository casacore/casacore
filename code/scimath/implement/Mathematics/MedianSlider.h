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

#if !defined(AIPS_MEDIANSLIDER_H)
#define AIPS_MEDIANSLIDER_H

//#! Includes go here

#include <aips/Arrays/Vector.h>

//# Forward Declarations

// <summary>
// Optimized sliding median computator
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
//#! What does the class do?  How?  For whom?  This should include code
//#! fragments as appropriate to support text.  Code fragments shall be
//#! delimited by <srcblock> </srcblock> tags.  The synopsis section will
//#! usually be dozens of lines long.
// </synopsis>
//
// <example>
//#! One or two concise (~10-20 lines) examples, with a modest amount of
//#! text to support code fragments.  Use <srcblock> and </srcblock> to
//#! delimit example code.
// </example>
//
// <motivation>
//#! Insight into a class is often provided by a description of
//#! the circumstances that led to its conception and design.  Describe
//#! them here.
// </motivation>
//
// <templating arg=T>
//#! A list of member functions that must be provided by classes that
//#! appear as actual template arguments.  For example:  imagine that you
//#! are writing a templated sort class, which does a quicksort on a
//#! list of arbitrary objects.  Anybody who uses your templated class
//#! must make sure that the actual argument class (say, Int or
//#! String or Matrix) has comparison operators defined.
//#! This tag must be repeated for each template formal argument in the
//#! template class definition -- that's why this tag has the "arg" attribute.
//#! (Most templated classes, however, are templated on only a single
//#! argument.)
//    <li>
//    <li>
// </templating>
//
// <thrown>
//#! A list of exceptions thrown if errors are discovered in the function.
//#! This tag will appear in the body of the header file, preceding the
//#! declaration of each function which throws an exception.
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//#! A List of bugs, limitations, extensions or planned refinements.
//#! The programmer should fill in a date in the "asof" field, which
//#! will usually be the date at which the class is submitted for review.
//#! If, during the review, new "todo" items come up, then the "asof"
//#! date should be changed to the end of the review period.
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class MedianSlider
{
public:
    
  MedianSlider  ();
  MedianSlider  ( int halfwin );

  ~MedianSlider ();

// Adds a datum to the slider. Once the window is full, newer values will 
// push out older values. Returns the new median value.
// If flag is set to true, adds a "flagged" datum, one which takes
// up space in the window but is not concidered for median computations.
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
  Int  size    ();    
// Returns the number of non-flagged values
  Int  nval    ();
  
// Returns the current median value  
  Float median ();

// Returns a previous value (from n steps ago) from the sliding window
  Float prevVal  ( uInt n,Bool &flag );
// Returns value from midpoint (center) of window
  Float midpoint ( Bool &flag );
  Float midpoint ()               { Bool dum; return midpoint(dum); }
// Returns the difference between the current median and the value
// at window center. Optionally, also returns flag of median center
  Float diff ( Bool &flag )       { return midpoint(flag) - median(); };
  Float diff ()                   { Bool dum; return diff(dum); }

// returns memory usage for a given halfwin size 
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


#endif
