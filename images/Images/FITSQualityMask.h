 //# FITSMask.h: A Lattice that can be used for temporary storage
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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

#ifndef LATTICES_FITSQUALMASK_H
#define LATTICES_FITSQUALMASK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/lattices/Lattices/Lattice.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class FITSImage;
class FITSErrorImage;

// <summary>
// Provides an on-the-fly mask for FITS quality images
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="FITSImage">FITSQualityImage</linkto>
// </prerequisite>

// <etymology>
// This class provides a pixel mask for the FITSQualityImage class.
// </etymology>

// <synopsis>
// Masked values are indicated in FITS images via magic
// value blanking.  This class provides an on-the-fly mask.
// The doGetSlice function reads the data values and returns
// an Array<Bool> which is True (good) or False (bad - blanked)
//
// Because FITSMask inherits from Lattice<Bool> it can be
// used as the private pixel mask data member for FITSQualityImage
// returned by the MaskedLattice::pixelMask() functions
//
// The FITSQualityMask object is constructed from the FITSImage objects
// of the data and the error extension. These must be the same one that
// the FITSQUalityImage object constructs internally.  They shared by both
// FITSImage and FITSMask.
//
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// FITSQualityImage provides access to FITS images with a data and and error
// extension. It needed an efficient way to handle the pixel mask
// other than iterating all the way through the image
// first to set a mask.
// </motivation>

//# <todo asof="yyyy/mm/dd">
//#   <li> add this feature
//#   <li> fix this bug
//#   <li> start discussion of this possible extension
//# </todo>


class FITSQualityMask : public Lattice<Bool>
{
public:

	// The pointers are not cloned, just copied.
	FITSQualityMask (FITSImage *fitsData, FITSErrorImage *fitsError);

	// Copy constructor (reference semantics).
	FITSQualityMask (const FITSQualityMask& other) ;
    
	// Destructor
	virtual ~FITSQualityMask();

	// The assignment operator with reference semantics.
	FITSQualityMask& operator= (const FITSQualityMask& other);

	// Make a copy of the object (reference semantics).
	virtual Lattice<Bool>* clone() const;

  // Is the FITSMask writable? Returns False. Although it is not hard
  // to implement writing of the mask, data values would be lost
  // because of magic blanking. 
  virtual Bool isWritable() const;

  // Return the shape of the Lattice including all degenerate 
  // axes (ie. axes with a length of one) 
  IPosition shape() const;

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<Bool>& buffer, const Slicer& section);

  // Do the actual getting of an array of values. Throws an exception.
  virtual void doPutSlice (const Array<Bool>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  // Set the switch for filtering 0.0
  virtual void setFilterZero(Bool filterZero);

private:
  FITSQualityMask();

  // Mask out ONLY NaN's
  Bool filterNaN(bool* pMask, const float* pData, const uInt nelems);

  // Mask out NaN's and values 0.0
  Bool filterZeroNaN(Bool* pMask, const Float* pData, const uInt nelems);

  //
  FITSImage      *itsFitsData;
  FITSErrorImage *itsFitsError;
  Array<Float>    itsBuffer;
  Bool            itsFilterZero;
};



} //# NAMESPACE CASACORE - END

#endif
