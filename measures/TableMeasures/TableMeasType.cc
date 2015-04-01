//# TableMeasDef.cc: Definition of a Measure in a Table.
//# Copyright (C) 1997,1999,2000
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
#include <casacore/measures/TableMeasures/TableMeasType.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableMeasType::TableMeasType()
: itsNtypes(0),
  itsStypes(0),
  itsTyps  (0)
{}

TableMeasType::TableMeasType (const Measure& meas)
: itsNtypes(0),
  itsStypes(0),
  itsTyps  (0),
  itsMeasHolder(meas)
{
  Int nextras;
  itsStypes = itsMeasHolder.asMeasure().allTypes (itsNtypes, nextras, 
						  itsTyps);
}

TableMeasType::TableMeasType (const TableMeasType& that)
: itsNtypes(that.itsNtypes),
  itsStypes(that.itsStypes),
  itsTyps  (that.itsTyps),
  itsMeasHolder(that.itsMeasHolder)
{}

TableMeasType::~TableMeasType()
{
  // Note that Measure::allTypes returns pointers to internal (static)
  // data structures. So those data should not be deleted here.
}

TableMeasType& TableMeasType::operator= (const TableMeasType& that)
{
  if (this != &that) {
    itsNtypes = that.itsNtypes;
    itsStypes = that.itsStypes;
    itsTyps   = that.itsTyps;
    itsMeasHolder = that.itsMeasHolder;
  }
  return *this;
}

const String& TableMeasType::type() const 
{ 
  return itsMeasHolder.asMeasure().tellMe();
}

uInt TableMeasType::refCode (const String& refString) const 
{
  Int i;
  for (i=0; i<itsNtypes; i++) {
    if (itsStypes[i] == refString) {
      break;
    }
  }
  if (i >= itsNtypes) {
    throw (AipsError ("TableMeasDesc::refCode() - refType " + refString +
		      " unknown for measType " + type()));
  }
  return itsTyps[i];
}

const String& TableMeasType::refType (uInt refCode) const 
{ 
  Int i;
  for (i=0; i<itsNtypes; i++) {
    if (itsTyps[i] == refCode) {
      break;
    }
  }
  if (i >= itsNtypes) {
    throw (AipsError ("TableMeasDescBase::refType - refCode " + 
		      String::toString(refCode) + 
		      " unknown for measure" + type()));
  }
  return itsStypes[i];
}

void TableMeasType::toRecord (RecordInterface& rec)
{
  String error;
  itsMeasHolder.toType (error, rec);
}

} //# NAMESPACE CASACORE - END

