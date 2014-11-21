//# ArrayOpsDiffShapes.cc: Operations for 2 Arrays with possibly different shapes.
//# Copyright (C) 2009
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
#include <casacore/casa/Arrays/ArrayOpsDiffShapes.h>
#include <casacore/casa/Arrays/ArrayMath.h>
//#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casa {

Bool rightExpandableToLeft(const IPosition& leftShape, const IPosition& rightShape)
{
  uInt n_desired_dim = rightShape.nelements();
  Bool expandable = (leftShape.nelements() > n_desired_dim);
    
  for(uInt axnum = 0; expandable && axnum < n_desired_dim; ++axnum)
    expandable = (leftShape[axnum] == rightShape[axnum]);

  return expandable;
}

} //#End casa namespace
