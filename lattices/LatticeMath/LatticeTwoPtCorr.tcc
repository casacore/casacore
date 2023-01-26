//# LatticeTwoPtCorr.cc: compute two point correlation functions of a lattice
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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

#ifndef LATTICES_LATTICETWOPTCORR_TCC
#define LATTICES_LATTICETWOPTCORR_TCC

#include <casacore/lattices/LatticeMath/LatticeTwoPtCorr.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/ArrayIO.h>

/*
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/iostream.h>
*/

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> 
void LatticeTwoPtCorr<T>::autoCorrelation (MaskedLattice<T>& latOut, 
                                           const MaskedLattice<T>& latIn,
                                           const IPosition& axes, 
                                           Method method,
                                           bool showProgress) const
{ 
   LogIO os(LogOrigin("LatticeTwoPtCorr", "autoCorrelation(...)", WHERE));

// Set up function pointer

   FuncPtr funcPtr=0;
   if (method==STRUCTUREFUNCTION) {
     funcPtr = &LatticeTwoPtCorr<T>::structureFunction;
   } else {
      os << "Unimplemented method" << LogIO::EXCEPTION;
   }

// Do the work

   autoCorrelation (latOut, latIn, axes, funcPtr, showProgress);
}

template <class T>
IPosition LatticeTwoPtCorr<T>::setUpShape (const IPosition& inShape, const IPosition& axes)
{
   AlwaysAssert (axes.nelements()==2, AipsError);
   AlwaysAssert (inShape.nelements()>=2, AipsError);
//
   IPosition outShape = inShape;
   outShape(axes(0)) = (inShape(axes(0))-1)*2 + 1;
   outShape(axes(1)) = (inShape(axes(1))-1)*2 + 1;
//
   return outShape;
}

template <class T> 
typename LatticeTwoPtCorr<T>::Method LatticeTwoPtCorr<T>::fromString (const String& methodU)
{
   String method = methodU;
   method.upcase();
   typename LatticeTwoPtCorr<T>::Method m = LatticeTwoPtCorr<T>::UNDEFINED;
//
   if (method.contains("STR")) {
     m = LatticeTwoPtCorr<T>::STRUCTUREFUNCTION;
   }
//
   return m;
}


template <class T> 
String LatticeTwoPtCorr<T>::toString (Method method)
{
   String m;
   if (method==LatticeTwoPtCorr<T>::STRUCTUREFUNCTION) {
     m = String("structurefunction");
   } else {
     m = String("undefined");
   }
//
   return m;
}



// Private functions

template <class T> 
void LatticeTwoPtCorr<T>::autoCorrelation  (MaskedLattice<T>& latOut, 
                                            const MaskedLattice<T>& latIn,
                                            const IPosition& axes, 
                                            FuncPtr funcPtr,
                                            bool showProgress) const
{ 
   LogIO os(LogOrigin("LatticeTwoPtCorr", "autoCorrelation(...)", WHERE));

// Check output lattice shape and axes

   check (os, latOut, latIn, axes);
//
   IPosition shapeIn = latIn.shape();
   IPosition shapeOut = latOut.shape();
   uint32_t nDim = shapeIn.nelements();
   IPosition axisPath = IPosition::makeAxisPath (nDim, axes);

// Make input iterator

   int32_t nxIn = shapeIn(axes(0));
   int32_t nyIn = shapeIn(axes(1));
   IPosition cursorShapeIn(2, nxIn, nyIn);
   LatticeStepper stepIn(shapeIn, cursorShapeIn, axes, axisPath);
   RO_MaskedLatticeIterator<T> itIn(latIn, stepIn);
   bool inIsMasked = latIn.hasPixelMask();

// Make output iterators

   int32_t nxOut = shapeOut(axes(0));
   int32_t nyOut = shapeOut(axes(1));
   IPosition cursorShapeOut(2, nxOut, nyOut);
   LatticeStepper stepOut(shapeOut, cursorShapeOut, axes, axisPath);
   LatticeIterator<T> itOut(latOut, stepOut);
   bool outIsMasked = latOut.hasPixelMask() && latOut.pixelMask().isWritable();
   LatticeIterator<bool>* itOutMaskPtr = 0;
   if (outIsMasked) {
      Lattice<bool>& outMask = latOut.pixelMask();
      itOutMaskPtr = new LatticeIterator<bool>(outMask, stepOut);
   }

// Matrices for plane by plane iteration results

   Matrix<T> sumOut(nxOut, nyOut);   
   Matrix<float> nPtsOut(nxOut, nyOut);
   Matrix<bool> maskOut(nxOut,nyOut);

// Iterate through image, plane by plane.  The algorithm is too
// complicated if I iterate tile by tile

   int32_t lxOff = (nxOut-1) / 2; 
   int32_t lyOff = (nyOut-1) / 2;
   int32_t lx = 0;
   int32_t ly = 0;
//
   for (itIn.reset(),itOut.reset(); !itIn.atEnd(); itIn++,itOut++) {
     if (showProgress) {
        os << LogIO::NORMAL << "Processing position " << itIn.position() << LogIO::POST;
     }

// Get data and mask

     const Matrix<T>& dataIn(itIn.matrixCursor());
     const Matrix<bool>& maskIn(itIn.getMask(true));

// Initialize output

     T zero(0.0);
     sumOut.set (zero);     
     nPtsOut.set(0.0);
     maskOut.set(false);

// Create ArrayAccessors to optimize access to Matricies

     ArrayAccessor<T, Axis<1> > jIt(dataIn);   // Outer loops
     ArrayAccessor<T, Axis<0> > iIt;
     ArrayAccessor<T, Axis<1> > jjIt(dataIn);  // Inner loops
     ArrayAccessor<T, Axis<0> > iiIt;
//
     ArrayAccessor<T, Axis<1> > jjItS(sumOut);   // Inner loops
     ArrayAccessor<T, Axis<0> > iiItS(sumOut);
     ArrayAccessor<float, Axis<1> > jjItN(nPtsOut);  // Inner loops
     ArrayAccessor<float, Axis<0> > iiItN(nPtsOut);
//
     int32_t i,j,ii,jj,id,jd;
     if (inIsMasked) {

// Create Mask accessors

       ArrayAccessor<bool, Axis<1> > jItM(maskIn);         // Outer loops
       ArrayAccessor<bool, Axis<0> > iItM;
       ArrayAccessor<bool, Axis<1> > jjItM(maskIn);        // Inner loops
       ArrayAccessor<bool, Axis<0> > iiItM;
//
       ArrayAccessor<bool, Axis<1> > jjItMOut(maskOut);   // Inner loops
       ArrayAccessor<bool, Axis<0> > iiItMOut(maskOut);
//
       for (j=0; j<nyIn; jIt++,jItM++,++j) {
         iIt = jIt;
         iItM = jItM;
         jd = lyOff - j;
         for (i=0; i<nxIn; iIt++,iItM++,++i) { 
           if (*iItM) {
             for (jjIt.reset(),jjItM.reset(),jj=0; jj<nyIn; jjIt++,jjItM++,++jj) {
               iiIt = jjIt;
               iiItM = jjItM;
               ly = jj + jd;

// Locate Accessors in output Matrices

               iiItS.reset(jjItS.begin(ly));                // Position at row ly
               iiItN.reset(jjItN.begin(ly));
               iiItMOut.reset(jjItMOut.begin(ly));
               id = lxOff - i;
//
               for (ii=0; ii<nxIn; iiIt++,iiItM++,++ii) { 
                 if (*iiItM) {
                   lx = ii + id;
//
                   iiItN[lx] += 1.0;
                   iiItMOut[lx] = true;
                   iiItS[lx] += ((*this).*funcPtr)(*iIt, *iiIt);
                 }
               }
             }
           }
         }
       }
     } else {

// No mask, so save a little time...

       for (j=0; j<nyIn; jIt++,++j) {
         iIt = jIt;
         jd = lyOff - j;
         for (i=0; i<nxIn; iIt++,++i) { 
           for (jjIt.reset(),jj=0; jj<nyIn; jjIt++,++jj) {
             iiIt = jjIt;
             ly = jj + jd;

// Locate Accessors in output Matrices

             iiItS.reset(jjItS.begin(ly));                // Position at row ly
             iiItN.reset(jjItN.begin(ly));
             id = lxOff - i;
//
             for (ii=0; ii<nxIn; iiIt++,++ii) { 
               lx = ii + id;
               iiItN[lx] += 1.0;
               iiItS[lx] += ((*this).*funcPtr)(*iIt, *iiIt);
             }
           }
         }
       }

// There is no input mask so make all output mask points good

       maskOut.set(true);
     }

// Normalize; use STL iterators for fastest access

     typename Array<T>::iterator outIter;
     typename Array<T>::iterator sumIter;
     typename Array<float>::iterator nIter;
     typename Array<float>::iterator nIterEnd = nPtsOut.end();
     for (outIter=itOut.rwMatrixCursor().begin(),sumIter=sumOut.begin(),nIter=nPtsOut.begin();
          nIter!=nIterEnd; ++nIter,++sumIter,++outIter) {
        if (*nIter > 0.5) {
           *outIter = *sumIter / *nIter;
        }
     }
//
     if (itOutMaskPtr) itOutMaskPtr->rwMatrixCursor() = maskOut;

// Increment output mask iterator

     if (itOutMaskPtr) (*itOutMaskPtr)++;
  }

// Cleanup

  if (itOutMaskPtr) delete itOutMaskPtr;
}



template <class T>
void LatticeTwoPtCorr<T>::check (LogIO& os, const MaskedLattice<T>& latOut,   
                                 const MaskedLattice<T>& latIn,
                                 const IPosition& axes) const
{
   AlwaysAssert (latIn.ndim() == latOut.ndim(), AipsError);
   IPosition inShape = latIn.shape();
   IPosition outShape = LatticeTwoPtCorr<T>::setUpShape (inShape, axes);

//
   if (!outShape.isEqual(latOut.shape())) {
      os << "Input  shape =  " << inShape << LogIO::POST;
      os << "Actual   output shape = " << latOut.shape() << LogIO::POST;
      os << "Expected output shape = " << outShape << LogIO::POST;
      os << "Output lattice has wrong shape" << LogIO::EXCEPTION;
   }
}


} //# NAMESPACE CASACORE - END


#endif
