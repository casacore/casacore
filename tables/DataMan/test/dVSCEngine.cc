//# dVSCEngine.cc: Example virtual column engine to handle data type A
//# Copyright (C) 1994,1995,1996,1997,2001
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

//# Define a main program to allow compalition and linking by the make system.
//# The variable is set in tVSCEngine.cc to skip it.
#if !defined(DVSCENGINE_MAIN)
int main()
{ return 0; }
#endif


//# Includes
#include "dVSCEngine.h"
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


#include <casacore/casa/namespace.h>
VSCExampleVSCEngine::VSCExampleVSCEngine()
{}

VSCExampleVSCEngine::VSCExampleVSCEngine (const String& sourceColumnName,
					  const String& xTargetColumnName,
					  const String& yTargetColumnName)
: VSCEngine<VSCExample> (sourceColumnName),
  xTargetName_p (xTargetColumnName),
  yTargetName_p (yTargetColumnName)
{}

VSCExampleVSCEngine::VSCExampleVSCEngine (const VSCExampleVSCEngine& that)
: VSCEngine<VSCExample> (that),
  xTargetName_p (that.xTargetName_p),
  yTargetName_p (that.yTargetName_p)
{}

VSCExampleVSCEngine::~VSCExampleVSCEngine()
{}

DataManager* VSCExampleVSCEngine::clone() const
{
    DataManager* dmPtr = new VSCExampleVSCEngine (sourceColumnName(),
					       xTargetName_p, yTargetName_p);
    if (dmPtr == 0) {
	throw (AllocError ("VSCExampleVSCEngine::clone()", 1));
    }
    return dmPtr;
}


// Store the target column names in the keywords of this column.
void VSCExampleVSCEngine::create (uInt)
{
    TableColumn src (table(), sourceColumnName());
    src.rwKeywordSet().define ("_xTargetName", xTargetName_p);
    src.rwKeywordSet().define ("_yTargetName", yTargetName_p);
}

// Prepare the engine by allocating column objects
// for the used columns.
// Get their names from the keywords of this column.
void VSCExampleVSCEngine::prepare()
{
    TableColumn src (table(), sourceColumnName());
    xTargetName_p = src.keywordSet().asString ("_xTargetName");
    yTargetName_p = src.keywordSet().asString ("_yTargetName");
    colx.attach (table(), xTargetName_p);
    coly.attach (table(), yTargetName_p);
}

void VSCExampleVSCEngine::get (uInt rownr, VSCExample& value)
{
    colx.get (rownr, value.x());
    coly.get (rownr, value.y());
}
void VSCExampleVSCEngine::put (uInt rownr, const VSCExample& value)
{
    colx.put (rownr, value.x());
    coly.put (rownr, value.y());
}


DataManager* VSCExampleVSCEngine::makeObject (const String&, const Record&)
{
    DataManager* dmPtr = new VSCExampleVSCEngine();
    if (dmPtr == 0) {
	throw (AllocError ("VSCExampleVSCEngine::makeObject()", 1));
    }
    return dmPtr;
}
void VSCExampleVSCEngine::registerClass()
{
    DataManager::registerCtor ("VSCExampleVSCEngine", makeObject);
}


#ifdef AIPS_NO_TEMPLATE_SRC
// Instantiate the templates here and not by means of the templates file.
// This is needed in case -f_no-implicit-templates is not used.
// In that case weak symbols are also created for Vector<bool>, etc.
// Thereafter the linker wants to eliminate double defined weak symbols,
// and also takes the dRetypedArrayEngine symbols into account.
// That is fine when linking dRetypedArrayEngine, but gives undefined
// linkonce symbols for other test programs which might use Vector<bool> or so.
#include <casacore/casa/Arrays/Array.tcc>
#include <casacore/casa/Arrays/MaskedArray.tcc>
#include <casacore/casa/Arrays/Vector.tcc>
#include <casacore/casa/Containers/Block.h>
#include <casacore/tables/Tables/ScaColData.tcc>
#include <casacore/tables/Tables/ScaColDesc.tcc>
#include <casacore/tables/Tables/ScalarColumn.tcc>
#include <casacore/tables/DataMan/VirtScaCol.tcc>
#include <casacore/casa/Utilities/Compare.tcc>
#include <casacore/casa/Utilities/Copy.tcc>
#include <casacore/casa/Utilities/CountedPtr.tcc>
#include <casacore/casa/Utilities/ValTypeId.h>

namespace casacore {

template class Array<VSCExample>;
template class MaskedArray<VSCExample>;
template class Vector<VSCExample>;
template class Block<VSCExample>;
template class ScalarColumnData<VSCExample>;
template class ScalarColumnDesc<VSCExample>;
template class ScalarColumn<VSCExample>;
template class VSCEngine<VSCExample>;
template class VirtualScalarColumn<VSCExample>;
template class ObjCompare<VSCExample>;
template void objcopy<VSCExample>(VSCExample *, VSCExample const *, uInt);
template void objcopy<VSCExample>(VSCExample *, VSCExample const *, uInt, uInt, uInt);
template void objset<VSCExample>(VSCExample *, VSCExample, uInt);
template void objset<VSCExample>(VSCExample *, VSCExample, uInt, uInt);
template void objmove<VSCExample>(VSCExample *, VSCExample const *, uInt);
template class CountedPtr<Block<VSCExample> >;
template class PtrRep<Block<VSCExample> >;
template String valDataTypeId(VSCExample const *);

}
#endif
