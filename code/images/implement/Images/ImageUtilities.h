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
// This class offers a programmer who is writing image analysis applications
// some simple helper functions to ease access and iteration though images.    
// The functions fall very roughly into three types.   1)  Functions to
// interpret some things that the user might have passed in to an application,
// 2) functions to set up objects which can be used to create image iterators, 
// and 3) miscellaneous functions which might be commonly useful.
//
// Our main goal in an image analysis application is to iterate through an 
// image and do something with the chunk of data received with each iteration.
// The chunk (or "cursor") received is specified to the image iterator 
// by two arrays.   These are the cursor shape and the cursor order arrays.
// The cursor shape is the size of the cursor.  For example, consider a 3-d (xyz) 
// image.  To iterate through it by xz planes, the cursor shape would be
// [nx,1,nz]  and the cursor order [0,1,2].  
//
// Thus, this class expects the user to specify what are called the cursor axes.  These
// are the axes whose cursor shape will be non-unit (i.e. axes 0 and 2 in the above
// example).   The class then offers you a function to generate, from the
// <src>cursorAxes</src>, the cursor shape (<src>cursorShape</src>) which you can
// feed to a Lattice iterator constructor (with implicit "natural" cursor order).
//
// The other array that these functions generate is the display axes array.  
// As a function of those axes whose <src>cursorShape</src> is unity, you might 
// want to show the results of operating on the data chunk currently in the cursor
// The <src>displayAxes</src> array contains the axes that aren't in the cursorAxes array.
// Do with them what you will.
//
// If computer memory was limitless this is all we would need to do.  To read
// and image just set the cursor shape to the full image and read it into core.
// However, with current physical limitations, one cannot do this as images
// might be very large.  Thus, the function <src>setCursor</src> that generates
// the <src>cursorShape</src> array, can be told a couple of extra things if you 
// desire. The variables <src>optimumEntireImage</src> and <src>maxDim</src> can 
// be used to control the size of the cursor.  It's then up to you the application programmer
// to deal with the cursor shape accordingly.  I will discuss them here as it's
// a bit lengthy to include all the detail in the function call documentation.
//
// An aips++ image is stored in a tiled fashion 
// and this means it can be read optimally fast if you don't care about the
// order in which data chunks are returned.
//
// <ul>
//   <li>  <src>optimumEntireImage = True</src>.    If <src>cursorAxes</src>
//        requests the entire image, then the cursor shape is set 
//        to that for which the image can be iterated through optimally fast, 
//        otherwise, the cursor shape is just set to reflect the values in 
//        <src>cursorAxes</src>         
// 
//   <li> <src>optimumEntireImage = False</src>.  The cursor shape is just set to 
//        reflect the values in <src>cursorAxes</src>. 
//
//        However, it may be additionally modified by the <src>maxDim</src> argument.  
//        This sets the maximum number of dimensions the cursor shape can have.  This 
//        allows you to prevent huge arrays being potentially allocated for the cursor 
//        if you are reading large multi-dimensional images but the data order is 
//        important (hence <src>optimumEntireImage=False</src>).  Setting, say, 
//        <src>maxDim=2</src> limits the cursor shape to a plane and 
//        you have to iterate through the image rather trying to get it all in 
//        one chunk.  Setting <src>maxDim=1</src> would limit the cursor shape to a profile.
//
//   <li> <src>nVirCursorIter</src>:  The user's desired "virtual" cursor shape (defined by 
//        <src>cursorAxes</src>) may not be read in one iteration -- it may be capped 
//        by <src>maxDim</src> or set according to <src>optimumEntireIMage</src>. This tells 
//        how many actual cursor chunks fits into the virtual cursor that the user asked for.
// </ul>
// 
//
// The functions in this class are written as static functions as they don't 
// logically belong to an object, by and large.
// </synopsis>
//
// <example>
// <srcBlock>
//#include <aips/aips.h>
//#include <aips/Inputs/Input.h>
//#include <aips/Arrays/ArrayMath.h>
//#include <aips/Exceptions/Error.h>
//
//#include <trial/Images/ImageUtilities.h>
//#include <trial/Images/PagedImage.h>
//#include <trial/Lattices/LatticeIterator.h>
// 
//#include <iostream.h>
//
//main (int argc, char **argv)
//{
//try {
//
//   Input inputs(1);
//   inputs.Version ("$Revision$");
//
//// Get inputs
//
//   inputs.Create("in", "", "Input file name");
//   inputs.Create("axes", "-10", "Cursor axes");
//   inputs.ReadArguments(argc, argv);
//
//   const String file = inputs.GetString("in");
//   const Block<Int> cursorAxesB(inputs.GetIntArray("axes"));
//
//   Vector<Int> cursorAxes(cursorAxesB);
//
//// Open image
//
//   if (in.empty()) {
//      cout << "You must give an image name") << endl;
//      return 1;
//   }
//
//// Detect image type
//
//   DataType imageType = imagePixelType(in);
//   if (imageType != TpFloat) {
//      cout << "Cannot handle image of type " << imageType << endl;
//      return 1;
//   }
//     
//   PagedImage<Float>image(file;)
//   const PagedImage<Float>* pImage = &image;
// 
//// Set cursor arrays -- not limiting dimensionality of the cursor array
//// and catching default (-10)
//
//   IPosition cursorShape;
//   Int nVirCursorIter;
//   if (cursorAxes.nelements() == 1 && cursorAxes(0) = -10) cursorAxes.resize(0);
//   if (!ImageUtilities::setCursor(nVirCursorIter, cursorShape, cursorAxes,
//                                  pImage->shape(), 
//                                  pImage->niceCursorShape(pImage->maxPixels()),
//                                  False, 0, cout)) return 1;
//
//
//// Set display axis array
//
//   Vector<Int> displayAxes; 
//   ImageUtilities::setDisplayAxes (displayAxes, cursorAxes, pImage->ndim());
//
//
//// Set up pixel iterator 
//
//   RO_LatticeIterator<Float> pixelIterator(image, cursorShape);
//
//
//// Iterate through image
//
//   Int i;
//   for (i=0,pixelIterator.reset(); !pixelIterator.atEnd(); i++,
//         pixelIterator++) {
//
//// Do something trivial with data chunk
//
//     cout << "Mean of chunk = " << i << " is " << mean(pixelIterator.cursor());
//   }  
//}
//
//  catch (AipsError x) {
//     cerr << "aipserror: error " << x.getMesg() << endl;
//     return 1;
//  } end_try;
//
//  return 0;
//}
//
// </srcBlock>
//
// This example shows a simple program which gets some user inputs, sets up cursor arrays,
// and iterates through an image working out the mean of each data chunk.  
// The <src>displayAxes</src> array is created but not used.   It's just there to 
// show its creation. Do what you like with it.
// </example>
//
// <motivation>
// The primary motivation was to provide an easily accessible set of functions 
// that could be used by many applications to access and iterate through images.
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

// Determine whether the value of an integer is matched by the value of any of 
// the elements of a <src>Vector<Int></src>. Returns the index if found, else -1
   static Int inVector (const Int& target, 
                        const Vector<Int>& vector);

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

// Set the cursor shape and cursor order arrays (0 relative) from the cursor axes array.  
// The cursor order is always the image natural order (0,1,2...).  See general
// description above for details on call objects.   A return value of False
// indicates invalid arguments.
   static Bool setCursor (Int& nVirCursorIter,
                          IPosition& cursorShape,
                          Vector<Int>& cursorAxes, 
                          const IPosition& imageShape,
                          const IPosition& imageTileShape,
                          const Bool& optimumEntireImage,
                          const Int& maxDim,
                          ostream& os);

// Set the display axes (0 relative). These are the axes that are not cursor axes.   If the
// cursor axes are all of the available image axes, the displayAxes array is of length 0.
   static void setDisplayAxes (Vector<Int>& displayAxes, 
                               const Vector<Int>& cursorAxes, 
                               const Int& nImageDim);

// Convert the <src>Vector<Double></src> include or exclude pixel ranges to
// a single range and Booleans specifying whether the range is inclusion 
// or exclusion (you can't have both).  <src>include</src> and/or <src>exclude</src> 
// should be of zero length if you do not wish to set and inclusion and/or exclusion
// range. A vector of length 1 for <src>include</src> and/or <src>exclude</src>
// means that the range will be set to (say for <src>include</src>)
// <src>range(0)=-abs(include(0))<src> and <src>range(1)=abs(include(0))</src>
// <src> range will be of xero length on output if neither an inclusion
// nor an exclusion range is given. A return value of <src>False</src>
// indicates that both an inclusion and exlcusion range were given.
   static Bool setIncludeExclude (Vector<Float>& range,
                                  Bool& noInclude,
                                  Bool& noExclude,
                                  const Vector<Double>& include,
                                  const Vector<Double>& exclude,
                                  ostream& os);

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

// Verify that an array of axes are valid for this image. That is, make sure they are 
// in the range 0 -> nDim-1.   A return value of <src>False</src> indicates
// an invalid axis in the list.
   static Bool verifyAxes (const Int& nDim,
                           const Vector<Int>& axes,
                           ostream& os);

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

