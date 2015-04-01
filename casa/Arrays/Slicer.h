//# Slicer.h:  specify which elements to extract from an n-dimensional array
//# Copyright (C) 1994,1995,1997,1999
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

#ifndef CASA_SLICER_H
#define CASA_SLICER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Slice;


// <summary>
// Specify which elements to extract from an n-dimensional array
// </summary>

// <reviewed reviewer="Paul Shannon" date="1994/07/07" tests="tSlicer">
//  The review and modification of this class were undertaken, in part,
//  with the aim of making this class header an example -- this is what
//  the Casacore project thinks a class header should look like.
// </reviewed>

// <prerequisite>
// You should have at least a preliminary understanding of these classes:
//   <li> <linkto class=IPosition>IPosition</linkto>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto class=Slice>Slice</linkto>
// </prerequisite>

// <etymology>
// The class name "Slicer" may be thought of as a short form 
// of "n-Dimensional Slice Specifier."  Some confusion is possible
// between class "Slice" and this class.
// </etymology>
//
// <synopsis>
// If you need to extract or operate upon a portion of an array,
// the Slicer class is the best way to specify the subarray you are
// interested in.
//
// Slicer has many constructors.  Of these, some require that the
// programmer supply a full specification of the array elements he
// wants to extract;  other constructors make do with partial information.
// In the latter case, the constructor will assume sensible default values or, 
// when directed, infer missing information from the array that's getting 
// sliced (hereafter, the "source" array).
//
// <h4> Constructing With Full Information </h4>
//
// To fully specify a subarray, you must supply three pieces of information
// for each axis of the subarray: 
// 
// <ol>
//    <li> where to start
//    <li> how many elements to extract
//    <li> what stride (or "increment" or "interval") to use:  a stride of
//         "n" means pick extract only every "nth" element along an axis
// </ol>
//
// The most basic constructor for Slicer illustrates this.  To create
// an Slicer for getting selected elements from a 3D array:
//
// <srcblock> 
//   IPosition start (3,0,0,0), length (3,10,10,10), stride (3,3,3,3);
//   Slicer slicer (start, length, stride);
//   // assume proper declarations, and meaningful values in the source array
//   subArray = sourceArray (slicer);
// </srcblock>
//
// <note role=caution> If you wish to extract elements from the array 
// at intervals, these intervals must be regular.   The interval is one 
// constant integer for each dimension of the array:  it cannot be a function.
// </note>
// 
// <note role=caution> "length", the second parameter to the Slicer 
// constructor above, may actually be used in two ways.  In normal 
// (and default) use, it specifies how many elements to select from the 
// source.  In the alternative use, it specifies the index of the last element
// to extract from the source array.  This ambiguity (does "end" mean 
// "length" or does it mean "last index"?) is handled by a default 
// fourth parameter to the constructor.  This code fragment will
// extract the same subarray as the example above:
// <srcblock>
//   IPosition start (3,0,0,0), end (3,27,27,27), stride (3,3,3,3);
//   Slicer slicer (start, end, stride, Slicer::endIsLast);
//   subArray = sourceArray (slicer);
// </srcblock>
// (We use "end" as the name of the formal parameter because it supports 
// both meanings -- "last index" or "length."  You may wish to use a 
// clarifying name for the actual parameter in your code, as we have 
// above when we used "length".)
// </note>
//
// <h4> Constructing with Partial Information </h4>
// 
// Some of the constructors don't require complete information:  Slicer
// either calculates sensible default values or deduces them from the
// source array.  If you do not specify a "stride" argument, for example,
// a value of 1 will be used for all dimensions.  If you specify a "start" 
// but nothing else, a stride of 1, and (perhaps against expectation)
// a length of 1 will be used. 
// 
//
// To instruct the Slicer to get otherwise unspecified information 
// from the source array, you can create an IPosition like  "end"
// as shown here:
//
// <srcblock>
//   IPosition start (3,0,0,0), stride (3,3,3,3);
//   IPosition end   (3,Slicer::MimicSource, Slicer::MimicSource, 
//                    Slicer::MimicSource);
//   Slicer smartSlicer (start, end, stride);
//   // assume proper declarations...
//   subArray = sourceArray (smartSlicer)
// </srcblock>
//
// If you are a library programmer, and write a class that can be sliced
// by the Slicer class, you need to understand the mechanism for
// completing the information which the application programmer, in using
// your class, specified incompletely. (If you are an application
// programmer, who wants to slice a library class, this explanation will
// be only of academic interest.) 
// 
// When the source array (the library class you provide) gets the Slicer --
// which typically comes when the source array is asked to return a
// reference to a subarray -- the source does a callback to the Slicer
// object.  The source array passes its own shape as one of the arguments
// to the Slicer callback and asks the Slicer to fill in the missing
// values from that shape.
// 
// In use, and with an imagined class "MyVector", code would look
// like this:
// <srcblock>
//   // first, a fragment from the application program:
//   IPosition start (1,10), end (1, Slicer::MimicSource);
//   Slicer slicer (start, end);
//   MyVector <Int> v0 (100);
//   MyVector <Int> v1 = v0 (slicer);
//   //....
//   // second, a fragment from a constructor of the library class "MyVector":
//   // the MyVector class will construct v1 as a reference to
//   // selected elements of v0, using (among other things) a
//   // callback to the slicer it was passed (above, in the
//   // construction of v1.
//   // 
//   IPosition start, end, stride;
//   fullSliceInformation = 
//      slicer.inferShapeFromSource (MyVector::shape(), start, end, stride);
//   // now the MyVector instance knows everything it needs to
//   // construct the instance.
// </srcblock>
// Please note that v1 will have a length of 90, and refer to elements
// 10-99 of v0.
//
// <note role=warning> An exception will be thrown if the positions
//  defined in the Slicer exceed the source array's shape.
// </note>
// </synopsis>
//
// <example>
// Given a large image, 4k on a side, extract (by sampling) an image
// 1k on a side, but covering the same region as the original.
//
// <srcblock>
//   Image <Float>  image ("N5364.fits");   // a 4-d VLA map, 4096 x 4096 x 3 x 1
//   IPosition start (4,0,0,0,0), stride (4,4,4,1,1);
//   IPosition end   (4, Slicer::MimicSource, Slicer::MimicSource, 
//                    Slicer::MimicSource, Slicer::MimicSource);
//   Slicer smartSlicer (start, end, stride);
//   // assume proper declarations...
//   Image <Float> subImage = image (smartSlicer);
// </srcblock>
//  
// </example>

// <motivation>
// Slicer is particularly convenient for designers of other library
// classes: Array and Image, for example. (In fact, this convenience 
// was the original motivation for the class.)  The benefit
// is this:  the application programmer, who needs a slice of an Array,
// may provide slicing specifications in many different ways, but the 
// Array class author needs to provide only one member function to 
// return the slice.  The Slicer class, in effect, and with its
// many constructors, provides a way to funnel all of the variety
// into a single member function call to the array or image class.
//
// For example, imagine a 100 x 100 x 100 array from which you want to 
// extract various subarrays.  Here are some of the ways you might
// specify the the subarray in the -absence- of Slicer.  
//
// <srcblock>
//   // preliminaries: create a cube and assign values to all elements --
//   //  this will be "source" array
//   Cube <Int> bigCube (IPosition (3, 100, 100, 100));
//   assignValues (bigCube);
//   // declare a smaller cube, the destination array.
//   Cube <Int> smallCube (IPosition (3, 10, 10, 10));
//
//   //  example 1: use Slice objects to extract a subcube -- the first
//   //   ten elements along each axis
//   Slice xIndices (0,10,1), yIndices (0,10,1), zIndices (0,10,1);
//   smallCube = bigCube (xIndices, yIndices, zIndices);
//
//   // example 2: get the same subcube using three IPosition arguments
//   IPosition start (3,0,0,0), end (3,10,10,10), stride (3,1,1,1);
//   smallCube = bigCube (start, end, stride);
//
//   // example 3: use 2 IPositions, letting the 3rd (stride) default to
//   //            IPosition (3,1,1,1)
//   smallCube = bigCube (start, end);
// </srcblock>
//
// So the Cube class (together with its base class) must define three separate
// member functions for the essentially identical operation of
// extracting a subcube.  The same replication is also required of
// Image, Array, and the other Array subclasses (Matrix and Vector).
//
// The Slicer class collapses all of this into a single member
// function per class:
//   
// <srcblock>
//   Slicer slicer = (call the constructor that best suits your problem)
//   smallCube = bigCube (slicer);
// </srcblock>
// 
// Since there are many constructors available for Slicer, you
// can still specify the subarray that you may want in a number of 
// different ways, by constructing the Slicer in the way most natural
// to your circumstances.  You then pass the Slicer to the array, and 
// you will get back the slice you want.  
//
// This class also offers the application programmer considerable
// flexibility by allowing the shape of the source array to determine
// some of the slice specification.  This benefit is explained and
// demonstrated above.
// </motivation>

// <todo asof="1994/07/01">
//   <li> This class, and the TableArray, Array and Image classes, 
//   could allow for the extraction of a subarray with fewer axes than the
//   source array.  At present, for example, you cannot, directly slice
//   a matrix from a cube.
// </todo>


class Slicer
{
public:

    // Define the "MimicSource" value which defines the open start or end.
    // This value should be different from MIN_INT in IPosition.h.
    // It should also not be the lowest possible value, since that
    // will probably be used as an undefined value.
    enum {MimicSource= -2147483646};

    // Define the possible interpretations of the end-value.
    enum LengthOrLast {
	// The end-values given in the constructor define the lengths.
	endIsLength,
	// The end-values given in the constructor define the trc.
	endIsLast
    };

    // Construct a 1-dimensional Slicer.
    // Start and end are inferred from the source; stride=1.
    // "endIsLength" and "endIsLast" are identical here, so there's
    // no need to discriminate between them by using a default parameter.
    Slicer();

    // The member function <src>inferShapeFromSource</src>
    // (invoked as a callback by the
    // source array) will use the shape of the source array for the 
    // unspecified values:  IPosition elements with the value
    //  Slicer::MimicSource
    // <thrown>
    //    <li> ArraySlicerError
    // </thrown>
    // Create a Slicer with a given start, end (or length), and stride.
    // An exception will be thrown if a negative length or non-positive
    // stride is given or if the IPositions start, end, and stride
    // do not have the same dimensionality.
    // If length or stride is not given, they default to 1.
    // <br> It is possible to leave values in start and end undefined
    // by giving the value <src>MimicSource</src>. They can be filled
    // in later with the actual array shape using function
    // <src>inferShapeFromSource</src>.
    // <group>
    Slicer (const IPosition& start, const IPosition& end, 
	    const IPosition& stride, 
	    LengthOrLast endInterpretation = endIsLength);
    Slicer (const IPosition& start, const IPosition& end,
	    LengthOrLast endInterpretation = endIsLength);
    explicit Slicer (const IPosition& start);
    // </group>

    // Create a Slicer object from Slice objects.
    // In a Slice object one defines the start, length, and stride for 
    // one axis.
    // The default Slice constructor (called with no arguments) creates
    // a Slice with start and length equal to zero, and an undefined stride.
    // <group>
    // Create a Slicer for a 1-dimensional array.
    Slicer (const Slice& x, LengthOrLast endInterpretation = endIsLength);

    // Create a Slicer for a 2-dim array.
    Slicer (const Slice& x, const Slice& y, 
	    LengthOrLast endInterpretation = endIsLength);

    // Create a Slicer for a 3-dim array.
    Slicer (const Slice& x, const Slice& y, const Slice& z,
	    LengthOrLast endInterpretation = endIsLength);
    // </group>

    // Copy constructor (copy semantics).
    Slicer (const Slicer&);

    // Assignment (copy semantics).
    Slicer& operator= (const Slicer&);

    // Equality
    Bool operator==(const Slicer&) const;

    // Return the number of dimensions of the Slicer.
    uInt ndim() const;

    // This function checks all of the start, length (or end),
    // and stride IPositions, and fills in missing values by
    // getting the corresponding values from the shape of the
    // source array.
    // These will first be resized, if necessary.
    // If, for a given axis, (end < start) , it means that a 
    // length of zero was specified.
    // An exception is thrown if the
    // start, end, or length exceeds the array shape or if the
    // dimensionality of the array and Slicer do not conform.
    // <thrown>
    //   <li> ArraySlicerError
    // </thrown>
    // <group>
    // Infer the slicer's shape from an array, using a zero origin.
    IPosition inferShapeFromSource 
           (const IPosition& shape, IPosition& startResult,
            IPosition& endResult, IPosition& strideResult) const;

    // Infer the slicer shape from an array, with the given origin.
    // The returned values are based on a zero origin.
    IPosition inferShapeFromSource 
          (const IPosition& shape, const IPosition& origin,
           IPosition& startResult,  IPosition& endResult,
           IPosition& strideResult) const;
    // </group>

    // Report the defined starting position.
    const IPosition& start() const;

    // Report the defined ending position. 
    const IPosition& end() const;

    // Report the defined stride.
    const IPosition& stride() const;

    // Report the length of the resulting axes.
    const IPosition& length() const;

    // Are all values fixed (i.e., no MimicSource given)?
    Bool isFixed() const;

    // Set the start and end positions. No explicit checking is done that
    // the input parameters make sense, so you must be certain if you
    // call these. These are useful if you have a loop with many iterations
    // and you do not wish the overhead of creating a new Slicer object
    // for each iteration if the only thing you are doing is adjusting
    // the start and end positions. Other than for performance reasons,
    // these methods should not be called and you should prefer the
    // error checking provided by constructing a new Slicer object.
    // Note that the length is not updated, so in principle care should
    // be taken that the length does not change.
    // <group>
    void setStart (const IPosition& start)
      { start_p = start; }
    void setEnd (const IPosition& end)
      { end_p = end; }
    // </group>


private:
    LengthOrLast asEnd_p;
    IPosition    start_p;
    IPosition    end_p;
    IPosition    stride_p;
    IPosition    len_p;         // Length of input
    Bool         fixed_p;       // no MimicSource used

    // Define a private constructor taking an ssize_t.
    // This is to prevent the user from the unexpected and meaningless
    // Slicer that would result when the ssize_t argument is promoted to
    // an IPosition.
    Slicer (ssize_t);

    // Check the given start, end/length and stride.
    // Fill in the length or end.
    // It also calls <src>fillFixed</src> to fill the fixed flag.
    void fillEndLen();

    // Fill in start, len and stride from a Slice.
    void fillSlice (const Slice&, ssize_t& start, ssize_t& length,
                    ssize_t& stride);

    // Fill the fixed flag.
    void fillFixed();
};


// <summary>IO functions for Slicer's</summary>
// <group name="Slicer IO">
// Print the contents of the specified Slicer to the specified stream.
std::ostream& operator << (std::ostream& stream, const Slicer& slicer);
// </group>



inline uInt Slicer::ndim() const
    { return start_p.nelements(); }

inline const IPosition& Slicer::start() const
    { return start_p; }

inline const IPosition& Slicer::end() const
    { return end_p; }

inline const IPosition& Slicer::stride() const
    { return stride_p; }

inline const IPosition& Slicer::length() const
    { return len_p; }

inline Bool Slicer::isFixed() const
    { return fixed_p; }



} //# NAMESPACE CASACORE - END

#endif

