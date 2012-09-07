//# MArrayBase.cc: Base class for array used in a TableExprNode with an optional mask
//# Copyright (C) 2012
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

//# Includes
#include <casa/Arrays/ArrayMath.h>  //# needed for correct build
#include <casa/Arrays/MArray.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  Array<Bool> MArrayBase::combineMask (const MArrayBase& other) const
  {
    if (itsMask.empty()) {
      return other.itsMask;
    } else if (other.itsMask.empty()) {
      return itsMask;
    }
    return itsMask || other.itsMask;
  }

  void MArrayBase::fillNValid() const
  {
    if (hasMask()) {
      itsNValid = nfalse(itsMask);
    } else {
      itsNValid = itsSize;
    }
  }

} //# NAMESPACE CASA - END
