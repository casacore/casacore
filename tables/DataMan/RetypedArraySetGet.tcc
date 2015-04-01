//# RetypedArraySetGet.cc: Helper functions for users of RetypedArrayEngine
//# Copyright (C) 1994,1995,1996
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

#ifndef TABLES_RETYPEDARRAYSETGET_TCC
#define TABLES_RETYPEDARRAYSETGET_TCC

//# Includes
#include <casacore/tables/DataMan/RetypedArraySetGet.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Utilities/Copy.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Copy the entire target array to the source array.
// It will check if the shapes and sizes match.
template<class SourceType, class TargetType>
void retypedArrayEngineSet (Array<SourceType>& out,
			    const Array<TargetType>& in)
{
    Bool deleteIn, deleteOut;
    SourceType* dataOut = out.getStorage (deleteOut);
    const TargetType* dataIn = in.getStorage (deleteIn);
    objcopy ((TargetType*)dataOut, dataIn, in.nelements());
    in.freeStorage (dataIn, deleteIn);
    out.putStorage (dataOut, deleteOut);
}    

// Copy the entire source array to the target array.
// It will check if the shapes and sizes match.
template<class SourceType, class TargetType>
void retypedArrayEngineGet (Array<TargetType>& out,
			    const Array<SourceType>& in)
{
    Bool deleteIn, deleteOut;
    TargetType* dataOut = out.getStorage (deleteOut);
    const SourceType* dataIn = in.getStorage (deleteIn);
    objcopy (dataOut, (const TargetType*)dataIn, out.nelements());
    in.freeStorage (dataIn, deleteIn);
    out.putStorage (dataOut, deleteOut);
}

// Fill an array with SourceType objects from the target array.
// This is called when the target is incomplete.
// The shape and extra argument can help to fill the source.
template<class SourceType, class TargetType>
void retypedArrayEngineSet (Array<SourceType>& out,
			    const Array<TargetType>& in,
			    const IPosition& shape,
			    const void* extraArgument)
{
    Bool deleteIn, deleteOut;
    SourceType* dataOut = out.getStorage (deleteOut);
    const TargetType* dataIn = in.getStorage (deleteIn);
    // Set element by element.
    uInt n = shape.product();
    SourceType* op  = dataOut;
    const TargetType* ip = dataIn;
    const TargetType* last = ip + in.nelements();
    while (ip < last) {
	op++->setElem (ip, shape, extraArgument);
	ip += n;
    }
    in.freeStorage (dataIn, deleteIn);
    out.putStorage (dataOut, deleteOut);
}

// Fill an array with TargetType objects from the source array.
// This is called when the target is incomplete.
// The shape and extra argument can help to extract the correct
// elements from the source.
template<class SourceType, class TargetType>
void retypedArrayEngineGet (Array<TargetType>& out,
			    const Array<SourceType>& in,
			    const IPosition& shape,
			    const void* extraArgument)
{
    Bool deleteIn, deleteOut;
    TargetType* dataOut = out.getStorage (deleteOut);
    const SourceType* dataIn = in.getStorage (deleteIn);
    // Set element by element.
    uInt n = shape.product();
    TargetType* op  = dataOut;
    const SourceType* ip = dataIn;
    const SourceType* last = ip + in.nelements();
    while (ip < last) {
	ip++->getElem (op, shape, extraArgument);
	op += n;
    }
    in.freeStorage (dataIn, deleteIn);
    out.putStorage (dataOut, deleteOut);
}

} //# NAMESPACE CASACORE - END


#endif
