//# RecordFieldWriter.cc: templated implementation of RecordFieldWriter.h
//# Copyright (C) 1996,1997,2001
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

#ifndef CASA_RECORDFIELDWRITER_TCC
#define CASA_RECORDFIELDWRITER_TCC

#include <casacore/casa/Containers/RecordFieldWriter.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class outType, class inType>
RecordFieldCopier<outType,inType>::RecordFieldCopier(RecordInterface &outRecord, 
						     RecordFieldId whichOutField,
						     const RecordInterface &inRecord, 
						     RecordFieldId whichInField)
      : out_p(outRecord, whichOutField), in_p(inRecord, whichInField)
{
    // Nothing
}

template<class outType, class inType>
void RecordFieldCopier<outType,inType>::writeField() 
{
    copy();
}

template<class T>
UnequalShapeCopier<T>::UnequalShapeCopier(RecordInterface &outRecord, 
					  RecordFieldId whichOutField,
					  const RecordInterface &inRecord, 
					  RecordFieldId whichInField)
  : out_p(outRecord, whichOutField), in_p(inRecord, whichInField)
{
  // Nothing
}

template<class T>
void UnequalShapeCopier<T>::writeField()
{ 
    uInt n = (*out_p).nelements();
    AlwaysAssert(n == (*in_p).nelements(), AipsError);
    Bool deleteOut, deleteIn;
    T *out = (*out_p).getStorage(deleteOut);
    const T *in = (*in_p).getStorage(deleteIn);
    objcopy(out, in, n);
    (*out_p).putStorage(out, deleteOut);
    (*in_p).freeStorage(in, deleteIn);
}

} //# NAMESPACE CASACORE - END


#endif
