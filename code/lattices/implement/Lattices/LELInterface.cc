//# LELInterface.cc:  this defines LELInterface.cc
//# Copyright (C) 1997,1998,1999
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

#include <trial/Lattices/LELInterface.h>
#include <trial/Lattices/LELUnary.h>


template<class T>
LELInterface<T>::~LELInterface()
{}

template<class T>
void LELInterface<T>::setAttr (const LELAttribute& attr)
{
    attr_p = attr;
}

template<class T>
Bool LELInterface<T>::replaceScalarExpr (CountedPtr<LELInterface<T> >& expr)
{
// Recursively prepare (optimize) a scalar subexpression
    Bool isInvalidScalar = expr->prepareScalarExpr();
// If the value is a valid scalar expression, replace it by its result
// (which can be an invalid scalar in itself).
    if (!isInvalidScalar  &&  expr->isScalar()) {
        LELScalar<T> tmp = expr->getScalar();
	if (tmp.mask()) {
	    expr = new LELUnaryConst<T> (tmp.value());
	} else {
	    isInvalidScalar = True;
	}
    }
// If the value is an invalid scalar expression, replace by scalar
// with false mask.
    if (isInvalidScalar) {
	expr = new LELUnaryConst<T>();
    }
    return isInvalidScalar;
}
