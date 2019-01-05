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
//# $Id: MArrayBase.cc 21262 2012-09-07 12:38:36Z gervandiepen $

//# Includes
#include <casacore/casa/Arrays/ArrayMath.h>  //# needed for correct build
#include <casacore/tables/TaQL/MArray.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <iostream>
#include <sstream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Construct from a mask.
  MArrayBase::MArrayBase (const ArrayBase& arr, const Array<Bool>& mask,
                          Bool isNull)
    : itsMask   (mask),
      itsShape  (arr.shape()),
      itsSize   (arr.size()),
      itsNValid (arr.size()),
      itsNull   (isNull)
  {
    init();
  }

  MArrayBase::MArrayBase (const ArrayBase& arr, const MArrayBase& marray)
    : itsMask   (marray.mask()),
      itsShape  (arr.shape()),
      itsSize   (arr.size()),
      itsNValid (arr.size()),
      itsNull   (marray.isNull())
  {
    init();
  }

  void MArrayBase::init()
  {
    if (itsNull) {
      AlwaysAssert (itsShape.empty()  &&  itsMask.empty(), AipsError);
    } else if (! itsMask.empty()) {
      itsNValid = -1;
      if (! itsShape.isEqual (itsMask.shape())) {
        std::ostringstream os;
        os << "MArrayBase - array shape " << itsShape
           << " and mask shape " << itsMask.shape() << " mismatch";
        throw ArrayError (os.str());
      }
    }
  }

  void MArrayBase::resizeBase (const ArrayBase& arr, Bool useMask)
  {
    itsShape.resize (arr.ndim());
    itsShape = arr.shape();
    itsSize  = arr.size();
    itsNull  = False;
    if (useMask) {
      itsMask.resize (arr.shape());
      itsNValid = -1;
    } else {
      removeMask();
    }
  }

  void MArrayBase::referenceBase (const MArrayBase& other)
  {
    itsMask.reference (other.itsMask);
    itsShape.resize (other.itsShape.size());
    itsShape  = other.itsShape;
    itsSize   = other.itsSize;
    itsNValid = other.itsNValid;
    itsNull   = other.itsNull;
  }

  void MArrayBase::setBase (const ArrayBase& arr, const Array<Bool>& mask)
  {
    itsShape.resize (arr.ndim());
    itsShape = arr.shape();
    itsSize  = arr.size();
    itsNull  = False;
    setMask (mask);
  }

  void MArrayBase::setMask (const Array<Bool>& mask)
  {
    if (mask.empty()) {
      removeMask();
    } else {
      AlwaysAssert (itsShape.isEqual (mask.shape()), AipsError);
      itsMask.reference (mask);
      itsNValid = -1;
    }
  }

  Array<Bool> MArrayBase::combineMask (const MArrayBase& other) const
  {
    if (itsMask.empty()) {
      return other.itsMask;
    } else if (other.itsMask.empty()) {
      return itsMask;
    }
    // Combine the flags of masked-off values.
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

} //# NAMESPACE CASACORE - END
