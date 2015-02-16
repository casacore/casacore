//# LELSpectralIndex.cc: LEL function to calculate spectral index/
//# Copyright (C) 2001
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

#ifndef LATTICES_LELSPECTRALINDEX_TCC
#define LATTICES_LELSPECTRALINDEX_TCC


#include <casacore/lattices/LEL/LELSpectralIndex.h>
#include <casacore/lattices/LEL/LELLattCoord.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LELSpectralIndex<T>::LELSpectralIndex (const Block<LatticeExprNode>& expr)
{
  arg0_p = expr[0];
  arg1_p = expr[1];
  // Spectralindex cannot handle scalars.
  // Expect 2 equal data types.
  Block<Int> argType(2);
  argType[0] = arg0_p.dataType();
  argType[1] = argType[0];
  setAttr (LatticeExprNode::checkArg (expr, argType, True, False));
  // Get the spectral coordinate info of the arguments.
  const LELAttribute& attr0 = arg0_p.getAttribute();
  const LELAttribute& attr1 = arg1_p.getAttribute();
  Vector<Double> freq0, freq1;
  itsFreqAxis = attr0.coordinates().coordinates().getSpectralInfo
                                                      (freq0, attr0.shape());
  Int freqAxis1 = attr1.coordinates().coordinates().getSpectralInfo
                                                      (freq1, attr1.shape());
  Vector<Double> logFreq;
  if (freq0.nelements() == 1) {
    logFreq = log (freq0(0) / freq1);
  } else if (freq1.nelements() == 1) {
    logFreq = log (freq0 / freq1(0));
  } else {
    AlwaysAssert (freq0.nelements() == freq1.nelements(), AipsError);
    logFreq = log (freq0 / freq1);
  }
  // Note that a Block is faster than Vector (in function eval),
  // so we rather use a Block.
  itsLogFreq.resize (logFreq.nelements());
  for (uInt i=0; i<logFreq.nelements(); i++) {
    if (logFreq(i) == 0) {
      itsLogFreq[i] = 0;
    } else {
      itsLogFreq[i] = 1/logFreq(i);
    }
  }
  // Compare the coordinates and shapes.
  Int result = attr0.compareCoord (attr1);
  if (result == 0) {
    AlwaysAssert (itsFreqAxis == freqAxis1, AipsError);
  } else if (result == -1) {
    // left is subset of right, so extend left.
    const LELLattCoordBase* cbptr = &(attr0.coordinates().coordinates());
    const LELLattCoord* cptr = dynamic_cast<const LELLattCoord*>(cbptr);
    AlwaysAssert (cptr != 0, AipsError);
    arg0_p = cptr->makeExtendLattice (arg0_p,
				      attr1.shape(),
				      attr1.coordinates().coordinates());
    itsFreqAxis = freqAxis1;
  } else if (result == 1) {
    // right is subset of left, so extend right.
    const LELLattCoordBase* cbptr = &(attr1.coordinates().coordinates());
    const LELLattCoord* cptr = dynamic_cast<const LELLattCoord*>(cbptr);
    AlwaysAssert (cptr != 0, AipsError);
    arg1_p = cptr->makeExtendLattice (arg1_p,
				      attr0.shape(),
				      attr0.coordinates().coordinates());
  } else {
    throw AipsError ("LELSpectralIndex - coordinates of operands mismatch");
  }
}

template <class T>
LELSpectralIndex<T>::~LELSpectralIndex()
{}


template <class T>
void LELSpectralIndex<T>::eval (LELArray<T>& result,
				const Slicer& section) const
{
#if defined(AIPS_TRACE)
  cout << "LELFunctionFloat:: eval" << endl;
#endif

  // Get the values of the left and right operand.
  // The resulting mask is the combination of the tow.
  LELArrayRef<T> tempr(result.shape());
  arg0_p.eval(result, section);
  arg1_p.evalRef(tempr, section);
  result.combineMask (tempr);
  // For each frequency channel the data has to be replaced by
  // log(left/right) / log(leftfreq/rightfreq)
  // Note that the freq factor has already been calculated in the constructor.
  // 'Split' the data into the part before the frequency axis and
  // the part after the frequency axis.
  const IPosition& shp = result.value().shape();
  uInt nrt = result.value().nelements();
  uInt stf = 0;           // start in itsLogFreq
  uInt endf = 0;          // end in itsLogFreq
  uInt incrf = 1;         // step in itsLogFreq
  uInt nrb = 1;
  uInt nre = 1;
  if (itsFreqAxis < 0) {
    nrb = nrt;
  } else {
    stf = section.start()(itsFreqAxis);
    endf = section.end()(itsFreqAxis);
    incrf = section.stride()(itsFreqAxis);
    for (uInt i=0; i<shp.nelements(); i++) {
      if (Int(i) < itsFreqAxis) {
	nrb *= shp(i);
      } else if (Int(i) > itsFreqAxis) {
	nre *= shp(i);
      }
    }
  }
  // Loop through all the data in that way.
  Bool deleteRes, deleteTmp;
  T* res = result.value().getStorage (deleteRes);
  T* resd = res;
  const T* tmp = tempr.value().getStorage (deleteTmp);
  const T* tmpd = tmp;
  for (uInt i=0; i<nre; i++) {
    for (uInt j=stf; j<=endf; j+=incrf) {
      const Float fact = itsLogFreq[j];
      if (fact == 0) {
	for (uInt k=0; k<nrb; k++) {
	  *resd = 0;
	  resd++;
	  tmpd++;
	}
      } else {
	for (uInt k=0; k<nrb; k++) {
	  if (*tmpd == 0) {
	    *resd = 0;
	  } else {
	    *resd = log(*resd / *tmpd) * fact;
	  }
	  resd++;
	  tmpd++;
	}
      }
    }
  }
  result.value().putStorage (res, deleteRes);
  tempr.value().freeStorage (tmp, deleteTmp);
}

template <class T>
String LELSpectralIndex<T>::className() const
{
  return "LELSpectralIndex";
}


template <class T>
Bool LELSpectralIndex<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  if (! arg0_p.lock (type, nattempts)) {
    return False;
  }
  return arg1_p.lock (type, nattempts);
}
template <class T>
void LELSpectralIndex<T>::unlock()
{
  arg0_p.unlock();
  arg1_p.unlock();
}
template <class T>
Bool LELSpectralIndex<T>::hasLock (FileLocker::LockType type) const
{
  if (! arg0_p.hasLock (type)) {
    return False;
  }
  return arg1_p.hasLock (type);
}
template <class T>
void LELSpectralIndex<T>::resync()
{
  arg0_p.resync();
  arg1_p.resync();
}


template <class T>
LELScalar<T> LELSpectralIndex<T>::getScalar() const
{
#if defined(AIPS_TRACE)
  cout << "LELSpectralIndex::getScalar" << endl;
#endif
  throw AipsError ("LELSpectralIndex::getScalar - invalid operation");
  return LELScalar<T>();
}


template <class T>
Bool LELSpectralIndex<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
  cout << "LELSpectralIndex::prepare" << endl;
#endif
  return False;
}

} //# NAMESPACE CASACORE - END


#endif
