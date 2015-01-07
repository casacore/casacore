//# RebinLattice.h: rebin a masked lattices
//# Copyright (C) 2003
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
//# You should have receied a copy of the GNU Library General Public License
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

#ifndef LATTICES_REBINLATTICE_H
#define LATTICES_REBINLATTICE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

class IPosition;


// <summary>
// Rebin a masked lattice.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tRebinLattice.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="MaskedLattice">MaskedLattice</linkto>
// </prerequisite>

// <synopsis>
//  This class enables you to rebin (data are averaged over bin) a MaskedLattice by 
//  a given factor per axis
// </synopsis>

// <example>
// <srcblock>
//    IPosition shape(2, 10, 20);
//    TiledShape tShape(shape);
//    TempLattice<Float> latIn(tShape);
//    IPosition factors(2, 2, 5);
//    RebinLattice<Float> rl(latIn, factors);
//    cerr << "Binned data = " << rl.get() << endl;
// </srcblock>
// </example>

// <motivation>
// </motivation>


template<class T>
class RebinLattice : public MaskedLattice<T>
{
public:

  // Default constructor (Object is unuseable)
  RebinLattice();

  // Constructor.  The bins don't have to fit integrally. Whatever
  // is left over at the end is treated as a full bin.
  RebinLattice(const MaskedLattice<T>& lattice, const IPosition& bin);

  // Copy constructor (reference semantics)
  RebinLattice(const RebinLattice<T>& other);

  // Destructor.
  virtual ~RebinLattice();

  // Assignment (reference semantics)
  RebinLattice<T>& operator=(const RebinLattice<T>& other);

  // Make a copy of the object (reference semantics).
  virtual MaskedLattice<T>* cloneML() const;

  // Is the lattice masked?
  // It is if its parent lattice is masked.
  virtual Bool isMasked() const;

  // Is the lattice paged to disk?
  virtual Bool isPaged() const;

  // The lattice is not writable.
  virtual Bool isWritable() const;

  // Handle locking of the lattice which is delegated to its parent.
  // <br>It is strongly recommended to use class
  // <linkto class=LatticeLocker>LatticeLocker</linkto> to
  // handle lattice locking. It also contains a more detailed
  // explanation of the locking process.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the Lattice object with the lattice file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  virtual void resync();

  // Flush the data.
  virtual void flush();

  // Close the Lattice temporarily (if it is paged to disk).
  // It'll be reopened automatically when needed or when
  // <src>reopen</src> is called explicitly.
  virtual void tempClose();

  // If needed, reopen a temporarily closed Lattice.
  virtual void reopen();

  // Get a pointer the region/mask object.
  // It returns 0.
  virtual const LatticeRegion* getRegionPtr() const;

  // Returns the shape of the lattice.
  virtual IPosition shape() const;
  
  // Return the name of the parent lattice.
  virtual String name (Bool stripPath=False) const;

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // Do the actual getting of an array of values.
  // Slicers with non-unit stride are not yet supported
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual putting of an array of values.
  // The lattice is not writable.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  // Slicers with non-unit stride are not yet supported
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Static function needed by LEL.  Applies binning factors
  // to shape to give the shape of the output lattice.  Will
  // give the same result as function 'shape'
  static IPosition rebinShape (const IPosition& shapeLatticeIn,
			       const IPosition& bin);  

private:
  Slicer findOriginalSlicer (const Slicer& section) const;
  void getDataAndMask (const Slicer& section);
  void bin(const Array<T>& dataIn);
  void bin(const Array<T>& dataIn, const Array<Bool>& maskIn);
//
  MaskedLattice<T>* itsLatticePtr;
  IPosition itsBin;
  Bool      itsAllUnity;
// Cache
  Array<T>    itsData;
  Array<Bool> itsMask;
  Slicer      itsSlicer;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/RebinLattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
