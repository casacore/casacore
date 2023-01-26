//# LatticeConcat.h: concatenate lattices along an axis
//# Copyright (C) 1996,1997,1998,1999,2000,2003
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

#ifndef LATTICES_LATTICECONCAT_H
#define LATTICES_LATTICECONCAT_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class IPosition;
class Slicer;


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
//
// The LatticeConcat object does not copy the input lattices, it
// just references them.  You can use the Lattice<T>::copyData(Lattice<T>)
// function to fill an output lattice with the concatenated input lattices.
//
// If you use the putSlice function, be aware that it will change the
// underlying lattices if they are writable.
// </synopsis>
//
// <example>
// <srcBlock>
//
//// Make ArrayLattices 
//
//    ArrayLattice<float> al1(a1); al1.set(1.0);
//    ArrayLattice<float> al2(a2); al2.set(10.0);
//
//// Turn these into MaskedLattices
//
//   SubLattice<float> ml1(al1, true);
//   SubLattice<float> ml2(al2, true);
//
//// Concatenate along axis 1
//         
//   LatticeConcat<float> lc (1);
//   lc.setLattice(ml1);
//   lc.setLattice(ml2);
//         
//// Make output
//         
//   ArrayLattice<float> al3(lc.shape());
//   SubLattice<float> ml3(al3, true);
//
//// Copy data to output (mask has to be copied separately)
//         
//   ml3.copyData(lc);
//   
// 
// </srcBlock>
// In this example no masks are involved.   See tLatticeConcat
// for more examples.
// </example>

// 
// <motivation>
// Image concatentation is a useful enduser requirement.  An object of
// this class is contained by an ImageConcat object.
// </motivation>

// <todo asof="1999/10/23">
// </todo>


template <class T> class LatticeConcat : public MaskedLattice<T>
{
public:

// Constructor. Argument <src>axis</src> specifies the concatenation 
// axis (0 relative).  If this is one more than the number of axes
// in the input lattices (set with function <src>setLattice</src>)
// then the resultant concatenated lattice has dimension
// one greater than that the input lattices.
// Argument <src>tempClose</src> specifies whether you wish 
// all internal lattice copies to be
// opened/closed on demand, rather than just being left open.
// This prevents open file limits being reached
   LatticeConcat (uint32_t axis, bool tempClose=true);

// Default constructor.  Sets the concatenation axis to 0
// and tempClose is true
   LatticeConcat ();

// Copy constructor (reference semantics)
   LatticeConcat(const LatticeConcat<T> &other);

// Destructor
   virtual ~LatticeConcat ();

// Assignment operator (reference semantics)
   LatticeConcat<T> &operator=(const LatticeConcat<T> &other);

// Adds a clone of the lattice to the list to be concatenated.  
// Exception thrown if lattices are incompatible
   void setLattice (MaskedLattice<T>& lattice);

// Return the number of lattices set so far
   uint32_t nlattices() const
     {return lattices_p.nelements();}

// Returns the current concatenation axis (0 relative)
   uint32_t axis () const
     {return axis_p;}

// Set the tempClose state.
   void setTempClose (bool tmpClose)
     { tempClose_p = tmpClose; }

// Returns the tempClose constructor state
   bool isTempClose () const 
     {return tempClose_p;} 

// Returns the number of dimensions of the *input* lattices (may be different 
// by one from output lattice).  Returns 0 if none yet set.
   uint32_t latticeDim() const;

// Return pointer for specified lattice.  Do not delete it.
   MaskedLattice<T>* lattice(uint32_t i) const
     { return lattices_p[i]; }

// Handle the (un)locking and syncing, etc.
// <group>
   virtual bool lock (FileLocker::LockType, uint32_t nattempts);
   virtual void unlock();
   virtual bool hasLock (FileLocker::LockType) const;
   virtual void resync();
   virtual void flush();
   virtual void tempClose();
   virtual void reopen();
// </group>

// Close/reopen a specific lattice.  It is your responsibility to leave the
// LatticeConcat object in a fully closed state.  So always pair
// a reopen with a tempClose.
// <group>
   void tempClose(uint32_t which);
   void reopen(uint32_t which);
// </group>

// Name.  Since many lattices may go into the concatenation, the name 
// is rather meaningless.  Returns the string "Concatenation :"
   virtual String name (bool stripPath=false) const;

// Make a copy of the derived object (reference semantics).
   virtual LatticeConcat<T>* cloneML() const;

// Has the object really a mask?
   virtual bool isMasked() const;

// Get the region used (always returns 0).
   virtual const LatticeRegion* getRegionPtr() const;
   
// If all of the underlying lattices are writable returns true
   virtual bool isWritable() const;

// Does the lattice have a pixelmask?
   virtual bool hasPixelMask() const;

// Get access to the pixelmask.
// An exception is thrown if the lattice does not have a pixelmask
// <group>
   virtual const Lattice<bool>& pixelMask() const;
   virtual Lattice<bool>& pixelMask();
// </group>

// Find the shape that the concatenated lattice will be.
// Returns a null IPosition if function setLattice has not yet 
// been called
   virtual IPosition shape () const;

// Return the best cursor shape.  This isn't very meaningful  for a LatticeConcat
// Lattice since it isn't on disk !  But if you do copy it out, this is 
// what you should use.  The maxPixels aregument is ignored.
   virtual IPosition doNiceCursorShape (uint32_t maxPixels) const;

// Do the actual get of the data.
// The return value is always false, thus the buffer does not reference
// another array.  Generally the user should use function getSlice
   virtual bool doGetSlice (Array<T>& buffer, const Slicer& section);
   
// Do the actual get of the mask data.
// The return value is always false, thus the buffer does not reference
// another array. Generally the user should use function getMaskSlice
   virtual bool doGetMaskSlice (Array<bool>& buffer, const Slicer& section);
   
// Do the actual put of the data into the Lattice.  This will change the underlying
// lattices (if they are writable) that were used to create the
// LatticeConcat object. It throws an exception if not writable.
// Generally the user should use function putSlice
   virtual void doPutSlice (const Array<T>& sourceBuffer,
                            const IPosition& where,
                            const IPosition& stride);

 
private:
   PtrBlock<MaskedLattice<T>* > lattices_p;
   uint32_t axis_p;
   IPosition shape_p;
   bool isMasked_p, dimUpOne_p, tempClose_p;
   LatticeConcat<bool>* pPixelMask_p;
//
   void checkAxis(uint32_t axis, uint32_t ndim) const;
//
   void setup1 (IPosition& blc, IPosition& trc, IPosition& stride,
                IPosition& blc2, IPosition& trc2,
                IPosition& blc3, IPosition& trc3, IPosition& stride3,
                const Slicer& section);
   Slicer setup2 (bool& first, IPosition& blc2, IPosition& trc2,
                  int32_t shape2, int32_t axis, const IPosition& blc,
                  const IPosition& trc, const IPosition& stride, int32_t start);
   bool getSlice1 (Array<T>& buffer, const Slicer& section,
                   uint32_t nLattices);
   bool getSlice2 (Array<T>& buffer, const Slicer& section,
                   uint32_t nLattices);
   bool putSlice1 (const Array<T>& buffer, const IPosition& where,
                   const IPosition& stride, uint32_t nLattices);

   bool putSlice2 (const Array<T>& buffer, const IPosition& where,
                   const IPosition& stride, uint32_t nLattices);
   bool getMaskSlice1 (Array<bool>& buffer, const Slicer& section,
                       uint32_t nLattices);
   bool getMaskSlice2 (Array<bool>& buffer, const Slicer& section,
                       uint32_t nLattices);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/LatticeConcat.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
