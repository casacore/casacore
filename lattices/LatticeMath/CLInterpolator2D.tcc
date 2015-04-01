//# CLInterpolator2D.h: Base class interpolator for CurvedLattice2D
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

#ifndef LATTICES_CLINTERPOLATOR2D_TCC
#define LATTICES_CLINTERPOLATOR2D_TCC


#include <casacore/lattices/LatticeMath/CLInterpolator2D.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
CLInterpolator2D<T>::~CLInterpolator2D()
{}

template<class T>
CLInterpolator2D<T>::CLInterpolator2D (const CLInterpolator2D<T>& that)
{
  set (that.itsLatticePtr, that.itsAxesMap, that.itsAxis1,
       that.itsAxis2, that.itsCurveAxis);
}

template<class T>
CLInterpolator2D<T>& CLInterpolator2D<T>::operator=
                                     (const CLInterpolator2D<T>& that)
{
  if (this != &that) {
    set (that.itsLatticePtr, that.itsAxesMap, that.itsAxis1,
	 that.itsAxis2, that.itsCurveAxis);
  }
  return *this;
}

template<class T>
void CLInterpolator2D<T>::set (MaskedLattice<T>* lattice,
			       const AxesMapping& axesMap,
			       uInt axis1, uInt axis2, uInt curveAxis)
{
  itsLatticePtr = lattice;
  itsAxesMap    = axesMap;
  itsAxis1      = axis1;
  itsAxis2      = axis2;
  itsCurveAxis  = curveAxis;
  if (lattice) {
    itsIsRef    = lattice->canReferenceArray();
    preset();
  } else {
    itsIsRef    = False;
  }
}

template<class T>
void CLInterpolator2D<T>::preset()
{}

} //# NAMESPACE CASACORE - END


#endif
