//# tImageExpr2.cc: Test program for boolean images with different shapes
//# Copyright (C) 2016
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA


#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <stdexcept>
#include <iostream>

using namespace casacore;

int main()
{
  try {
    {  
      PagedImage<Float> a(IPosition(4, 20, 20, 1, 20),
                          CoordinateUtil::defaultCoords4D(), "A.im");
      PagedImage<Float> b(IPosition(4, 20, 20, 1, 1),
                          CoordinateUtil::defaultCoords4D(), "B.im");
    }
    ImageRegion* reg = 0;
    reg = ImageRegion::fromLatticeExpression("(A.im + B.im) > 0");
    delete reg;
    reg = ImageRegion::fromLatticeExpression("A.im > 0 && B.im < 0");
    delete reg;
  } catch (const std::exception& x) {
    std::cout << x.what() << std::endl;
  }
}

