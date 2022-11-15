//# ArrayError.cc: Exception classes thrown by Array and related classes/functions
//# Copyright (C) 1993,1994,1995,1997,1999,2000
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

// .SUMMARY General, Indexing, and Conformace errors thrown by Array classes.

#include "ArrayError.h"


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayError::ArrayError() : std::runtime_error("ArrayError") {}

ArrayError::ArrayError(const char *m) : std::runtime_error(m) {}

ArrayError::ArrayError(const std::string &m) : std::runtime_error(m) {}

ArrayError::~ArrayError() noexcept{}



ArrayIndexError::ArrayIndexError() : ArrayError("ArrayIndexError") {}

ArrayIndexError::ArrayIndexError(const char* m) : ArrayError(m) {}

ArrayIndexError::ArrayIndexError(const std::string &m) : ArrayError(m) {}

ArrayIndexError::ArrayIndexError(const IPosition &in, 
				 const IPosition &sh, const char* m)
: ArrayError(m),
  i(in),
  l(sh)
{
    // Nothing
}

ArrayIndexError::~ArrayIndexError() noexcept{}

IPosition ArrayIndexError::index() const
{
    return i;
}

IPosition ArrayIndexError::shape() const
{
    return l;
}

ArrayConformanceError::ArrayConformanceError() 
: ArrayError("ArrayConformanceError") 
{
    // Nothing
}

ArrayConformanceError::ArrayConformanceError(const char* m) : ArrayError(m) {}

ArrayConformanceError::ArrayConformanceError(const std::string &m) : ArrayError(m) {}

ArrayConformanceError::~ArrayConformanceError() noexcept{}



ArrayNDimError::ArrayNDimError(int ndim1, int ndim2, const char* m)
: ArrayConformanceError(m + std::string(" -- ndim ") + std::to_string(ndim1)
                        + " differs from " + std::to_string(ndim2)),
  r1(ndim1),
  r2(ndim2)
{}

ArrayNDimError::ArrayNDimError(int ndim1, int ndim2, const std::string& m)
: ArrayConformanceError(m + std::string(" -- ndim ") + std::to_string(ndim1)
                        + " differs from " + std::to_string(ndim2)),
  r1(ndim1),
  r2(ndim2)
{}

ArrayNDimError::~ArrayNDimError() noexcept{}

void ArrayNDimError::ndims(int &ndim1, int &ndim2) const
{
    ndim1 = r1; 
    ndim2 = r2;
}



ArrayShapeError::ArrayShapeError(const IPosition &s1, const IPosition & s2,
				 const char* m)
: ArrayConformanceError(m + std::string(" shape ") + s1.toString()
                        + " differs from " + s2.toString()),
  sh1(s1), sh2(s2)
{
    // Nothing
}

ArrayShapeError::~ArrayShapeError() noexcept{}

void ArrayShapeError::shapes(IPosition &shape1, IPosition &shape2) const
{
    shape1 = sh1; shape2 = sh2;
}



ArrayIteratorError::ArrayIteratorError() : ArrayError("ArrayIteratorError") {}

ArrayIteratorError::ArrayIteratorError(const char* m) : ArrayError(m) {}

ArrayIteratorError::ArrayIteratorError(const std::string &m) : ArrayError(m) {}

ArrayIteratorError::~ArrayIteratorError() noexcept{}



ArraySlicerError::ArraySlicerError() : ArrayError("Slicer error") {}

ArraySlicerError::ArraySlicerError(const std::string &m)
: ArrayError("Slicer error:" + m) {}

ArraySlicerError::~ArraySlicerError() noexcept{}

} //# NAMESPACE CASACORE - END

