//# EclecticFunctionFactory.cc: a class for creating various Function objects from GlishRecords
//# Copyright (C) 2002
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
#include <trial/Functionals/EclecticFunctionFactory.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>

template<class T> 
EclecticFunctionFactory<T>::EclecticFunctionFactory() :
    FunctionFactory<T>(), 
    lookup(OrderedPair<FunctionFactory<T>*, Bool>(0,False))
{
  
}

template<class T>
EclecticFunctionFactory<T>::~EclecticFunctionFactory() {
    MapIter<String, OrderedPair<FunctionFactory<T>*, Bool> > iter(lookup);
    OrderedPair<FunctionFactory<T>*, Bool> val;

    for(; ! iter.atEnd(); ++iter) {
	val = iter.getVal();
	if (val.x() != 0 && val.y()) delete val.x();
    }
}

template<class T> 
Function<T> *EclecticFunctionFactory<T>::create(const GlishRecord& gr) const 
    throw(FunctionFactoryError)
{
    String type;
    if (! gr.exists("functype")) 
	throw InvalidGlishSerializationError("No functype field defined");
    const GlishArray &typef = gr.get("functype");
    if (typef.elementType() != GlishArray::STRING) 
	throw InvalidGlishSerializationError("Wrong type for functype field");
    if (typef.shape().product() < 1) 
	throw InvalidGlishSerializationError("Empty value for functype field");
    typef.get(type, 0);

    if (! lookup.isDefined(type)) throw UnrecognizedFunctionError(type);
    FunctionFactory<T> *fac = lookup(type).x();
    if (fac == 0) throw UnrecognizedFunctionError(type);
    return fac->create(gr);
}

template<class T> 
void EclecticFunctionFactory<T>::addFactory(const String& type, 
					    FunctionFactory<T> *factory,
					    Bool own) 
{
    lookup.define(type, 
		  OrderedPair<FunctionFactory<T>*, Bool>(factory, own));
}

