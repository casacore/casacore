//# ArrayError.cc: Exception classes thrown by Array and related classes/functions
//# Copyright (C) 1993,1994,1995,1997,1999
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

#include <aips/Arrays/ArrayError.h>

rtti_imp_init(ArrayError);
rtti_imp_mbrf(ArrayError);

ArrayError::ArrayError() : AipsError("ArrayError") {}

ArrayError::ArrayError(const Char *m) : AipsError(m) {}

ArrayError::ArrayError(const String &m) : AipsError(m) {}

ArrayError::ArrayError(ExcpError *excp) 
: AipsError(excp)
{
    ArrayError *tmp;
    PCAST(tmp,ArrayError,excp);
    if (tmp) {
        _equal = True;
    } else {
        _equal = False;
    }
}

ArrayError::~ArrayError() {}


rtti_imp_init(ArrayIndexError);
rtti_imp_mbrf(ArrayIndexError);

ArrayIndexError::ArrayIndexError() : ArrayError("ArrayIndexError") {}

ArrayIndexError::ArrayIndexError(const Char *m) : ArrayError(m) {}

ArrayIndexError::ArrayIndexError(const String &m) : ArrayError(m) {}

ArrayIndexError::ArrayIndexError(const IPosition &in, 
				 const IPosition &sh, const Char *m)
: ArrayError(m),
  i(in),
  l(sh)
{
    // Nothing
}

ArrayIndexError::ArrayIndexError(ExcpError *excp) 
: ArrayError(excp)
{
    ArrayIndexError *tmp;
    PCAST(tmp,ArrayIndexError,excp);
    if (tmp) {
        _equal = True;
	i = tmp->i;
	l = tmp->l;
    } else {
        _equal = False;
    }
}

ArrayIndexError::~ArrayIndexError() {}

IPosition ArrayIndexError::index() const
{
    return i;
}

IPosition ArrayIndexError::shape() const
{
    return l;
}


rtti_imp_init(ArrayConformanceError);
rtti_imp_mbrf(ArrayConformanceError);

ArrayConformanceError::ArrayConformanceError() 
: ArrayError("ArrayConformanceError") 
{
    // Nothing
}

ArrayConformanceError::ArrayConformanceError(const Char *m) : ArrayError(m) {}

ArrayConformanceError::ArrayConformanceError(const String &m) : ArrayError(m) {}

ArrayConformanceError::ArrayConformanceError(ExcpError *excp)
: ArrayError(excp)
{
    ArrayConformanceError *tmp;
    PCAST(tmp,ArrayConformanceError,excp);
    if (tmp) {
        _equal = True;
    } else {
        _equal = False;
    }
}

ArrayConformanceError::~ArrayConformanceError() {}


rtti_imp_init(ArrayNDimError);
rtti_imp_mbrf(ArrayNDimError);

ArrayNDimError::ArrayNDimError(ExcpError *excp) 
: ArrayConformanceError(excp)
{
    ArrayNDimError *tmp;
    PCAST(tmp,ArrayNDimError,excp);
    if (tmp) {
        _equal = True;
	r1 = tmp->r1;
	r2 = tmp->r2;
    } else {
        _equal = False;
    }
}

ArrayNDimError::ArrayNDimError(Int ndim1, Int ndim2, const Char *m)
: ArrayConformanceError(m),
  r1(ndim1),
  r2(ndim2)
{}

ArrayNDimError::~ArrayNDimError() {}

void ArrayNDimError::ndims(Int &ndim1, Int &ndim2) const
{
    ndim1 = r1; 
    ndim2 = r2;
}


rtti_imp_init(ArrayShapeError);
rtti_imp_mbrf(ArrayShapeError);

ArrayShapeError::ArrayShapeError(ExcpError *excp) 
: ArrayConformanceError(excp)
{
    ArrayShapeError *tmp;
    PCAST(tmp,ArrayShapeError,excp);
    if (tmp) {
        _equal = True;
	sh1 = tmp->sh1;
	sh2 = tmp->sh2;
    } else {
        _equal = False;
    }
}

ArrayShapeError::ArrayShapeError(const IPosition &s1, const IPosition & s2,
				 const Char *m)
: ArrayConformanceError(m),
  sh1(s1), sh2(s2)
{
    // Nothing
}

ArrayShapeError::~ArrayShapeError() {}

void ArrayShapeError::shapes(IPosition &shape1, IPosition &shape2) const
{
    shape1 = sh1; shape2 = sh2;
}


rtti_imp_init(ArrayIteratorError);
rtti_imp_mbrf(ArrayIteratorError);

ArrayIteratorError::ArrayIteratorError() : ArrayError("ArrayIteratorError") {}

ArrayIteratorError::ArrayIteratorError(const Char *m) : ArrayError(m) {}

ArrayIteratorError::ArrayIteratorError(const String &m) : ArrayError(m) {}

ArrayIteratorError::ArrayIteratorError(ExcpError *excp) 
: ArrayError(excp)
{
    ArrayIteratorError *tmp;
    PCAST(tmp,ArrayIteratorError,excp);
    if (tmp) {
        _equal = True;
    } else {
        _equal = False;
    }
}

ArrayIteratorError::~ArrayIteratorError() {}


rtti_imp_init(ArraySlicerError);
rtti_imp_mbrf(ArraySlicerError);

ArraySlicerError::ArraySlicerError() : ArrayError("Slicer error") {}

ArraySlicerError::ArraySlicerError(const String &m)
: ArrayError("Slicer error:" + m) {}

ArraySlicerError::ArraySlicerError(ExcpError *excp) 
: ArrayError(excp)
{
    ArraySlicerError *tmp;
    PCAST(tmp,ArraySlicerError,excp);
    if (tmp) {
        _equal = True;
    } else {
        _equal = False;
    }
}

ArraySlicerError::~ArraySlicerError() {}
