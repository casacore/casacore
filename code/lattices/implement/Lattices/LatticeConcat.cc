//# LatticeConcat.cc: concatenate lattices
//# Copyright (C) 1995,1997,1998,1999
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
//# $Id: 

#include <trial/Lattices/LatticeConcat.h>

#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Tasking/ProgressMeter.h>



template<class T>
LatticeConcat<T>::LatticeConcat()
: itsAxis(0),
  itsShape(IPosition(0)),
  itsShowProgress(False),
  itsIsMasked(False)
{
}

template<class T>
LatticeConcat<T>::LatticeConcat(uInt axis, Bool showProgress)
: itsAxis(axis),
  itsShape(IPosition(0)),
  itsShowProgress(showProgress),
  itsIsMasked(False)
{
}

template<class T>
LatticeConcat<T>::LatticeConcat (const LatticeConcat<T>&other) 
: itsLattices(other.itsLattices.nelements()),
  itsAxis (other.itsAxis),
  itsShape(other.itsShape),
  itsShowProgress(other.itsShowProgress),
  itsIsMasked(other.itsIsMasked)
{
   const uInt n = itsLattices.nelements();
   for (uInt i=0; i<n; i++) {
      itsLattices[i] = other.itsLattices[i]->cloneML();
   }
}

template<class T>
LatticeConcat<T>::~LatticeConcat()
{
   const uInt n = itsLattices.nelements();
   for (uInt i=0; i<n; i++) {
      delete itsLattices[i];
   }
}

template<class T>
LatticeConcat<T>& LatticeConcat<T>::operator= (const LatticeConcat<T>& other)
{
  if (this != &other) {
    itsAxis     = other.itsAxis;
    itsShape = other.itsShape;
    itsShowProgress = other.itsShowProgress;
    itsIsMasked = other.itsIsMasked;
//
    uInt n = itsLattices.nelements();
    for (uInt j=0; j<n; j++) {
            delete itsLattices[j];
            itsLattices[j] = 0;
    }
    itsLattices.resize(other.itsLattices.nelements(), True);
    n = itsLattices.nelements();
    for (uInt i=0; i<n; i++) {
       itsLattices[i] = other.itsLattices[i]->cloneML();
    }
  }
  return *this;
}

template<class T>
void LatticeConcat<T>::setAxis(uInt axis)
{
// Nothing to do if no change

   if (axis==itsAxis) return;

// Recheck lattices for consistency with new axis

   const uInt n = itsLattices.nelements();
   IPosition newShape;
   if (n>0) {
      newShape = itsLattices[0]->shape();
      checkAxis(axis, itsLattices[0]->ndim());
//
      if (n>1) {
         for (uInt j=1; j<n; j++) {
            if (newShape.nelements() != itsLattices[j]->ndim()) {
               throw (AipsError("Lattice dimensions inconsistent"));
            }
//
            const IPosition shape = itsLattices[j]->shape();
            for (uInt i=0; i<shape.nelements(); i++) {
               if (i!=axis && shape(i) != newShape(i)) {
                  throw (AipsError("Lattice shapes inconsistent"));
               }
            }
//
// Update concatenation axis shape and mask indicator
//
            newShape(axis) += shape(axis);
         }
      }
   }

// Update.  Leave it until now so that state
// is not modified if exception thrown

   itsShape.resize(newShape.nelements());
   itsShape = newShape;
   itsAxis = axis;
}


template<class T>
void LatticeConcat<T>::setLattice(MaskedLattice<T>& lattice)
{
   const uInt n = itsLattices.nelements();
   const uInt ndim = lattice.ndim();
//
// Check for consistency
//
   if (n==0) {
      checkAxis(itsAxis, ndim);
      itsShape = lattice.shape();
   } else {
      if (itsShape.nelements() != lattice.ndim()) {
         throw(AipsError("Lattice dimensions are inconsistent"));
      }
//
      const IPosition shape = lattice.shape();
      for (uInt i=0; i<shape.nelements(); i++) {
         if (i!=itsAxis && shape(i) != itsShape(i)) {
            throw (AipsError("Lattice shapes inconsistent"));
         }
      }
//
// Update concatenation axis shape 
//
      itsShape(itsAxis) += shape(itsAxis);
   }
//
// Assign lattice
//
   itsLattices.resize(n+1, True);
   itsLattices[n] = lattice.cloneML();
   if (lattice.isMasked()) itsIsMasked = True;
} 


template<class T>
void LatticeConcat<T>::reset()
{
   const uInt n = itsLattices.nelements();
   for (uInt i=0; i<n; i++) {
      delete itsLattices[i];
   }
   itsLattices.resize(0, True);
   itsAxis = 0;
   itsShape.resize(0);
   itsShowProgress = False;
   itsIsMasked = False;
}




template<class T>
IPosition LatticeConcat<T>::shape() const
{
   return itsShape;
} 

template<class T>
Bool LatticeConcat<T>::isMasked() const
{
   return itsIsMasked;
} 


template<class T>
void LatticeConcat<T>::copyData(MaskedLattice<T>& lattice)
{
// Check

   const uInt nLattices = itsLattices.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices to concatenate = use function setLattice"));
   }
//
   if (!itsShape.isEqual(lattice.shape())) {
      throw (AipsError("Output lattice has wrong shape"));
   }

// Setup progress meter

   ProgressMeter* clockPtr = 0;
   Double meterValue = 0.0;
   if (itsShowProgress) {
      const Double nPixels = Double(itsShape.product());
      clockPtr = new ProgressMeter(0.0, Double(nPixels), "Image Concatenation",
                                  "pixels copied", "", "", True, 1);
   }


// Is the output masked and writable ?

   Bool isOutputMasked = lattice.isMasked();
   if (itsIsMasked) {
      if (!isOutputMasked) {
         throw (AipsError("The input lattices are masked but the output lattice is not masked"));
      }
//
      if (isOutputMasked && !lattice.isMaskWritable()) {
         throw (AipsError("The input lattices are masked but the output lattice mask is not writable"));
      }
   }

// Copy

   const uInt dim = itsShape.nelements();
   IPosition blcOut(dim,0);
   IPosition trcOut(dim);
//
   for (uInt i=0; i<nLattices; i++) {
      trcOut = blcOut + itsLattices[i]->shape() - 1;
      LCBox regionOut(blcOut, trcOut, lattice.shape());
      SubLattice<Float> subLatticeOut(lattice, LatticeRegion(regionOut), True);
//
      if (itsIsMasked && isOutputMasked) {
         copyDataAndMask (subLatticeOut, *(itsLattices[i]));
      } else {
         subLatticeOut.copyData(*(itsLattices[i]));
      }

// Update

      blcOut(itsAxis) += itsLattices[i]->shape()(itsAxis);
      if (itsShowProgress) {
         meterValue += itsLattices[i]->shape().product();
         clockPtr->update(meterValue);
      }
   }
   if (clockPtr!=0) {
      delete clockPtr;
      clockPtr = 0;
   }
} 


template<class T>
void LatticeConcat<T>::copyDataAndMask(MaskedLattice<Float>& out,
                                       MaskedLattice<Float>& in) const
//
// The input and output are known to have a mask. The output mask
// must be writable
//
{
// Get hold of the mask

   if (!out.hasPixelMask()) {
      throw(AipsError("Cannot gain access to pixel mask of output lattice"));
   }
   Lattice<Bool>& maskOut = out.pixelMask();

// Use the same stepper for input and output.

   IPosition cursorShape = out.niceCursorShape();
   LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);
   
// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.

   LatticeIterator<Float> dummyIter(out);
   RO_LatticeIterator<Float> iter(in, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {
      out.putSlice (iter.cursor(), iter.position()); 
      maskOut.putSlice(in.getMaskSlice(iter.position(), iter.cursorShape()),
                       iter.position());
   }
}


template<class T>
void LatticeConcat<T>::checkAxis(uInt axis, uInt ndim) const
{
   if (axis >= ndim) {
      throw(AipsError("Axis number and lattice dimension are inconsistent"));
   }
}

