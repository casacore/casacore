//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1997,1998,1999,2000
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

#include <trial/Coordinates/LinearXform.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/String.h>

LinearXform LinearXform::fourierInvert (const Vector<Bool>& axes, 
                                        const Vector<Double>& crpix, 
                                        const Vector<Double>& scale) const
{
   if (axes.nelements() != nWorldAxes()) {
      throw (AipsError("axes length is invalid"));
   }
   if (crpix.nelements() != nWorldAxes()) {
      throw (AipsError("crpix length is invalid"));
   }
   if (scale.nelements() != nWorldAxes()) {
      throw (AipsError("scale length is invalid"));
   }
//
   Matrix<Double> pc0;
   if (isPCDiagonal_p) {

// Short cut which enables us to separate out axes

      pc0 = pc();
      Vector<Double> d = pc0.diagonal();
      for (uInt i=0; i<nWorldAxes(); i++) {
         if (axes(i)) d(i) = 1.0 / d(i);
      }
      pc0.diagonal() = d;
   } else {
      if (!allEQ(axes, True)) {
         throw(AipsError("Cannot invert non-diagonal PC matrix (probably a rotated CoordinateSystem) when some axes not being transformed"));
      }
//
      pc0 = invert(pc());
   }
//
   Vector<Double> cdelt0 = cdelt();   
   Vector<Double> crpix0 = LinearXform::crpix();
   for (uInt i=0; i<nWorldAxes(); i++) {
      if (axes(i)) {
         cdelt0(i) = scale(i) / cdelt0(i);
         crpix0(i) = crpix(i);
      }
   }
   return LinearXform(crpix0, cdelt0, pc0);
}
