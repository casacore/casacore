//# ImageUtilities.h: Some utility functions handy for accessing images
//# Copyright (C) 1996,1997
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
#if !defined(AIPS_IMAGEUTILITIES_H)
#define AIPS_IMAGEUTILITIES_H

#if defined(_AIX)
#pragma implementation ("ImageUtilities.cc")
#endif

#include <aips/aips.h>
#include <aips/Mathematics/Complex.h>
#include <trial/Lattices/Lattice.h>
template <class T> class Vector;
class CoordinateSystem;
class Table;
//
// <summary>
// A set of simple functions to help access and iteration through images
// </summary>
//   
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> IPosition
//   <li> Arrays
//   <li> Lattice
// </prerequisite>
//
// <synopsis>
//
// Some helpful static functions that are common to some of my image
// analysis application  classes.
// </synopsis>
//
// <motivation>
// I needed some bits and pieces.  My goal isto move this rag-tag bunch
// out of here into other classes as time goes on.  So far
// I have eliminated 50% of the original !
// </motivation>
//
// <todo asof="1996/11/27">
//  <li>  
// </todo>
 

class ImageUtilities {
public:

// Convert comma or space delimitered substrings into an
// array of strings.  
   static Vector<String> getStrings (const String& string);

// Find the next comma or space delimiterd substring in a string.   If <src>init=True</src> 
// start from the start of the string, else start from the end of the previous found 
// substring.  A return value of False indicates no more substrings.
   static Bool getNextSubString (String& subString,
                                 const String& inString,
                                 const Bool& init);

// This function converts pixel coordinates to world coordinates. You
// specify a vector of pixel coordinates (<src>pixels</src>) for only one 
// axis, the <src>pixelAxis</src>.    For the other pixel axes in the
// <src>CoordinateSystem</src>, if a pixel axis "i" is  found in the 
// <src>CursorAxes</src> vector, its pixel coordinate is set to 
// the average of the selected region from the image for that axis
// (<src>(blc(i)+trc(i))/2)</src>), otherwise it is set to the reference pixel.   
// The vector of world coordinates for <src>pixelAxis</src> is returned as formatted 
// Strings.  If for some reason it can't make the conversion, the element
// element is returned as "?"    Returns </src>False</src> if the lengths of
// <<src>blc</src> and <src>trc</src> are not equal to the number of pixel axes
// in the coordinate system.
   static Bool pixToWorld (Vector<String>& sWorld,
                           const CoordinateSystem& cSys,
                           const Int& pixelAxis,
                           const Vector<Int>& cursorAxes,
                           const IPosition& blc,
                           const IPosition& trc,
                           const Vector<Double>& pixels,
                           const Int& prec);

// Set the display axes (0 relative). These are the axes that are not cursor axes.   If the
// cursor axes are all of the available image axes, the displayAxes array is of length 0.
   static void setDisplayAxes (Vector<Int>& displayAxes, 
                               const Vector<Int>& cursorAxes, 
                               const Int& nImageDim);

// Check and fill in defaults for a <src>Vector<Int></src> containing the 
// number of subplots in x and y to be put on a plot.  The <src>Vector<Int></src> 
// is resized to 2 before assignment.  A return value of <src>False</src> indicates 
// invalid arguments.
   static Bool setNxy (Vector<Int>& nxy,
                       ostream& os);

// Return a Scratch Table where the Table name was constructed from the directory 
// of a given file, a specified string and a unique number appended to the string
// and worked out by this function.  This can be handy to generate, say, a scratch 
// storage image (maybe it is a <src>PagedArray</src>) in the same directory as the 
// image being traversed. 
   static Table setScratchTable (const String &inFileName, const String &tableName);

// A storage image is used to accumulate information as a function of the display
// axes as an image is iterated through.  This function sets the storage image shape 
// to that appropriate to the shape of the display axes and the desired size of the first
// or last dimension.  
   static void setStorageImageShape (IPosition& storeImageShape,
                                     const Bool& last,
                                     const Int& axisSize,
                                     const Vector<Int>& displayAxes,
                                     const IPosition& imageShape);

// Convert long axis names "Right Ascension", "Declination", "Frequency" and
// "Velocity" to "RA", "Dec", "Freq", "Vel" respectively.  Unknown strings
// are returned as given.
   static String shortAxisName (const String& axisName);


// Stretch a range by 10%
   static void stretchMinMax (Float& min, 
                              Float& max);
// Verify an image region specification.  Illegal (inlcuding blc > trc) or 
// unspecified values are  given 0 (blc) imageShape (trc) or 
// unity (inc).  Returns <src>True</src> if any of the region objects 
// are changed from their input values, else returns <src>False</src>
   static Bool verifyRegion (IPosition& blc,
                             IPosition& trc,
                             IPosition& inc,
                             const IPosition& imageShape);

};

#endif

