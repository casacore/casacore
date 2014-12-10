//# LinearXform2.cc: 
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
//#
//#
//# $Id$

#include <casacore/coordinates/Coordinates/LinearXform.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LinearXform* LinearXform::fourierInvert (String& errMsg, const Vector<Bool>& axes, 
                                         const Vector<Double>& crpix, 
                                         const Vector<Double>& scale) const
{
   if (axes.nelements() != nWorldAxes()) {
      errMsg = "axes length is invalid";
      return 0;
   }
   if (crpix.nelements() != nWorldAxes()) {
      errMsg = "crpix length is invalid";
      return 0;
   }
   if (scale.nelements() != nWorldAxes()) {
      errMsg = "scale length is invalid";
      return 0;
   }
//
   Matrix<Double> pc0;
   if (isPCDiagonal_p) {

// Short cut which enables us to separate out axes

      pc0 = pc();
      Vector<Double> d(pc0.diagonal().copy());
      for (uInt i=0; i<nWorldAxes(); i++) {
         if (axes[i]) d[i] = 1.0 / d[i];
      }
      pc0.diagonal() = d;
   } else {
      if (!allEQ(axes, True)) {
         errMsg = "Cannot invert non-diagonal PC matrix (probably a rotated CoordinateSystem) when some axes not being transformed";
         return 0;
      }
//
      pc0 = invert(pc());
   }
//
   Vector<Double> cdelt0(cdelt().copy());
   Vector<Double> crpix0(LinearXform::crpix().copy());
   for (uInt i=0; i<nWorldAxes(); i++) {
      if (axes[i]) {
         cdelt0[i] = scale[i] / cdelt0[i];
         crpix0[i] = crpix[i];
      }
   }
//
   return new LinearXform(crpix0, cdelt0, pc0);
}

} //# NAMESPACE CASACORE - END

