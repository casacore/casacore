//# LatticeConcat.h: concatenate lattice along an axis
//# Copyright (C) 1996,1997,1998,1999
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

#if !defined(AIPS_LATTICECONCAT_H)
#define AIPS_LATTICECONCAT_H


//# Includes
#include <aips/aips.h>
#include <aips/Containers/Block.h>
#include <trial/Lattices/MaskedLattice.h>

//# Forward Declarations
class IPosition;


// <summary>
// Concatenates lattices along a specified axis
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MaskedLattice>MaskedLattice</linkto> (base class)
// </prerequisite>

// <etymology>
// This is a class designed to concatenate lattices along a specified axis
// </etymology>

// <synopsis>
// This is a class designed to concatenate lattices along a specified 
// axis. This means you can join them together.  E.g., 
// join lattices of shape  [10,20,30] and [10,20,40] into a lattice 
// of shape [10,20,70]. 
//
// In addition, you can increase the dimensionality
// and join lattices [10,20] and [10,20] to [10,20,2].  This is
// done by specifying the concatenation axis to be higher than
// currently exists in the input lattices
// </synopsis>
//
// <example>
// <srcBlock>
//
//// Make ArrayLattices 
//
//    ArrayLattice<Float> al1(a1); al1.set(1.0);
//    ArrayLattice<Float> al2(a2); al2.set(10.0);
//
//// Turn these into MaskedLattices
//
//   SubLattice<Float> ml1(al1, True);
//   SubLattice<Float> ml2(al2, True);
//
//// Concatenate along axis 1
//         
//   LatticeConcat<Float> lc (1, False);
//   lc.setLattice(ml1);
//   lc.setLattice(ml2);
//         
//// Make output
//         
//   ArrayLattice<Float> al3(lc.shape());
//   SubLattice<Float> ml3(al3, True);
//
//// Do it
//         
//   lc.copyData(ml3);
//
// 
// </srcBlock>
// In this example no masks are involved.   See tLatticeConcat
// for more examples.
// </example>

// 
// <motivation>
// Image concatentation is a useful enduser requirement.  This is the
// base class for ImageConcat.
// </motivation>

// <todo asof="1999/09/23">
//   <li> This class is probably better to be derived from MaskedLattice
//        like the LEL classes.  Then it can be used by a LEL concat function
// </todo>


template <class T> class LatticeConcat
{
public:

// Constructor. Specify the concatenation axis (0 relative)
// and if you want to see a progress meter.  
   LatticeConcat (uInt axis, Bool showProgress);

// Default constructor.  Sets the concatenation axis to 0
// showProgress to False
   LatticeConcat ();

// Copy constructor (reference semantics)
   LatticeConcat(const LatticeConcat<T> &other);

// Destructor
   ~LatticeConcat ();

// Assignment operator (reference semantics)
   LatticeConcat<T> &operator=(const LatticeConcat<T> &other);

// Set concatenation axis.  You can call this more than once
// and at any time - the state of the object changes as needed
   void setAxis(uInt axis);

// Sets a new lattice into the list to be concatenated.  
// Exception thrown if lattices are incompatible
   void setLattice (MaskedLattice<T>& lattice);

// Resets the state of the object to axis=0. no lattices
// and no progress meter
   void reset();

// Find the shape that the concatenated lattice will be.
// Use this so you can create the MaskedLattice for the
// output.  Returns a null IPosition if function setLattice has not yet 
// been called
   IPosition shape () const;

// Is the input masked ? If so, your output ML needs a 
// writable mask too.
   Bool isMasked () const;

// Return the number of lattices set so far
   uInt nelements() const {return itsLattices.nelements();};

// Returns the current concatenation axis (0 relative)
   uInt axis () const {return itsAxis;};

// Fill the supplied Lattice with the input lattices
// so as to effectively concatenate them.  The lattices are 
// concatenated in the order in which you give them in
// the setLattice function.
// Throws exceptions for shape/dimension incompatibilities.
   void copyData(MaskedLattice<T>& lattice);

protected:

   PtrBlock<MaskedLattice<T>* > itsLattices;
   uInt itsAxis;

private:

   IPosition itsShape;
   Bool itsShowProgress, itsIsMasked;

//
   void copyDataAndMask(MaskedLattice<Float>& out,
                        MaskedLattice<Float>& in) const;
   void copyDataOnly(MaskedLattice<Float>& out,
                     MaskedLattice<Float>& in) const;
   void checkAxis(uInt axis, uInt ndim) const;

};
#endif
