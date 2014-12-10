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
//#
//# $Id$

// .SUMMARY General, Indexing, and Conformace errors thrown by Array classes.

#include <casacore/casa/Arrays/ArrayError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayError::ArrayError(Category c) : AipsError("ArrayError",c) {}

ArrayError::ArrayError(const Char *m,Category c) : AipsError(m,c) {}

ArrayError::ArrayError(const String &m,Category c) : AipsError(m,c) {}

ArrayError::~ArrayError() throw(){}



ArrayIndexError::ArrayIndexError(Category c) : ArrayError("ArrayIndexError",c) {}

ArrayIndexError::ArrayIndexError(const Char *m,Category c) : ArrayError(m,c) {}

ArrayIndexError::ArrayIndexError(const String &m,Category c) : ArrayError(m,c) {}

ArrayIndexError::ArrayIndexError(const IPosition &in, 
				 const IPosition &sh, const Char *m,Category c)
: ArrayError(m,c),
  i(in),
  l(sh)
{
    // Nothing
}

ArrayIndexError::~ArrayIndexError() throw(){}

IPosition ArrayIndexError::index() const
{
    return i;
}

IPosition ArrayIndexError::shape() const
{
    return l;
}



ArrayConformanceError::ArrayConformanceError(Category c) 
: ArrayError("ArrayConformanceError",c) 
{
    // Nothing
}

ArrayConformanceError::ArrayConformanceError(const Char *m,Category c) : ArrayError(m,c) {}

ArrayConformanceError::ArrayConformanceError(const String &m,Category c) : ArrayError(m,c) {}

ArrayConformanceError::~ArrayConformanceError() throw(){}



ArrayNDimError::ArrayNDimError(Int ndim1, Int ndim2, const Char *m,Category c)
: ArrayConformanceError(m + String(" ndim ") + String::toString(ndim1)
                        + " differs from " + String::toString(ndim2), c),
  r1(ndim1),
  r2(ndim2)
{}

ArrayNDimError::~ArrayNDimError() throw(){}

void ArrayNDimError::ndims(Int &ndim1, Int &ndim2) const
{
    ndim1 = r1; 
    ndim2 = r2;
}



ArrayShapeError::ArrayShapeError(const IPosition &s1, const IPosition & s2,
				 const Char *m,Category c)
: ArrayConformanceError(m + String(" shape ") + s1.toString()
                        + " differs from " + s2.toString(), c),
  sh1(s1), sh2(s2)
{
    // Nothing
}

ArrayShapeError::~ArrayShapeError() throw(){}

void ArrayShapeError::shapes(IPosition &shape1, IPosition &shape2) const
{
    shape1 = sh1; shape2 = sh2;
}



ArrayIteratorError::ArrayIteratorError(Category c) : ArrayError("ArrayIteratorError",c) {}

ArrayIteratorError::ArrayIteratorError(const Char *m,Category c) : ArrayError(m,c) {}

ArrayIteratorError::ArrayIteratorError(const String &m,Category c) : ArrayError(m,c) {}

ArrayIteratorError::~ArrayIteratorError() throw(){}



ArraySlicerError::ArraySlicerError(Category c) : ArrayError("Slicer error",c) {}

ArraySlicerError::ArraySlicerError(const String &m,Category c)
: ArrayError("Slicer error:" + m,c) {}

ArraySlicerError::~ArraySlicerError() throw(){}

} //# NAMESPACE CASACORE - END

