//# NewMSDataDescColumns.cc:  provides easy access to NewMeasurementSet columns
//# Copyright (C) 1996,1999,2000
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

#include <aips/MeasurementSets/NewMSDataDescColumns.h>
#include <aips/MeasurementSets/NewMSDataDescription.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableDesc.h>

RONewMSDataDescColumns::RONewMSDataDescColumns()
  :flagRow_p(),
   polarizationId_p(),
   spectralWindowId_p(),
   lagId_p()
{
}

RONewMSDataDescColumns::
RONewMSDataDescColumns(const NewMSDataDescription& msDataDesc):
  flagRow_p(msDataDesc, NewMSDataDescription::
	    columnName(NewMSDataDescription::FLAG_ROW)),
  polarizationId_p(msDataDesc, NewMSDataDescription::
		   columnName(NewMSDataDescription::POLARIZATION_ID)),
  spectralWindowId_p(msDataDesc, NewMSDataDescription::
		     columnName(NewMSDataDescription::SPECTRAL_WINDOW_ID)),
  lagId_p()
{
  attachOptionalCols(msDataDesc);
}

RONewMSDataDescColumns::~RONewMSDataDescColumns() {}

Int RONewMSDataDescColumns::match(uInt spwId, uInt polId, Int tryRow=-1) {
  uInt r = nrow();
  if (r == 0) return -1;
  const Int spw = spwId;
  const Int pol = polId;
  // Main matching loop
  if (tryRow >= 0) {
    const uInt tr = tryRow;
    if (tr >= r) {
      throw(AipsError("RONewMSDataDescColumns::match(...) - "
                      "the row you suggest is too big"));
    }
    if (!flagRow()(tr) &&
	spectralWindowId()(tr) == spw && 
	polarizationId()(tr) == pol) { 
      return tr;
    }
    if (tr == r-1) r--;
  }
  while (r > 0) {
    r--;
    if (!flagRow()(r) &&
	spectralWindowId()(r) == spw && 
	polarizationId()(r) == pol) { 
      return r;
    }
  }
  return -1;
}

void RONewMSDataDescColumns::attach(const NewMSDataDescription& msDataDesc)
{
  flagRow_p.attach(msDataDesc, NewMSDataDescription::columnName
		   (NewMSDataDescription::FLAG_ROW));
  polarizationId_p.attach(msDataDesc, NewMSDataDescription::columnName
			  (NewMSDataDescription::POLARIZATION_ID));
  spectralWindowId_p.attach(msDataDesc, NewMSDataDescription::columnName
			    (NewMSDataDescription::SPECTRAL_WINDOW_ID));
  attachOptionalCols(msDataDesc);
}

void RONewMSDataDescColumns::
attachOptionalCols(const NewMSDataDescription& msDataDesc)
{
  const ColumnDescSet& cds=msDataDesc.tableDesc().columnDescSet();
  if (cds.isDefined(NewMSDataDescription::
		    columnName(NewMSDataDescription::LAG_ID))) {
    lagId_p.attach(msDataDesc,NewMSDataDescription::
		   columnName(NewMSDataDescription::LAG_ID));
  }
}

NewMSDataDescColumns::NewMSDataDescColumns():
  RONewMSDataDescColumns(),
  flagRow_p(),
  polarizationId_p(),
  spectralWindowId_p(),
  lagId_p()
{
}

NewMSDataDescColumns::NewMSDataDescColumns(NewMSDataDescription& msDataDesc):
  RONewMSDataDescColumns(msDataDesc),
  flagRow_p(msDataDesc, NewMSDataDescription::
	    columnName(NewMSDataDescription::FLAG_ROW)),
  polarizationId_p(msDataDesc, NewMSDataDescription::
		   columnName(NewMSDataDescription::POLARIZATION_ID)),
  spectralWindowId_p(msDataDesc,NewMSDataDescription::
		     columnName(NewMSDataDescription::SPECTRAL_WINDOW_ID)),
  lagId_p()
{
  attachOptionalCols(msDataDesc);
}

NewMSDataDescColumns::~NewMSDataDescColumns() {}

void NewMSDataDescColumns::attach(NewMSDataDescription& msDataDesc)
{
  RONewMSDataDescColumns::attach(msDataDesc);
  flagRow_p.attach(msDataDesc, NewMSDataDescription::columnName
		   (NewMSDataDescription::FLAG_ROW));
  polarizationId_p.attach(msDataDesc, NewMSDataDescription::columnName
			  (NewMSDataDescription::POLARIZATION_ID));
  spectralWindowId_p.attach(msDataDesc, NewMSDataDescription::columnName
			    (NewMSDataDescription::SPECTRAL_WINDOW_ID));
  attachOptionalCols(msDataDesc);
}

void NewMSDataDescColumns::
attachOptionalCols(NewMSDataDescription& msDataDesc)
{
  const ColumnDescSet& cds = msDataDesc.tableDesc().columnDescSet();
  if (cds.isDefined(NewMSDataDescription::
		    columnName(NewMSDataDescription::LAG_ID))) {
    lagId_p.attach(msDataDesc, NewMSDataDescription::
		   columnName(NewMSDataDescription::LAG_ID));
  }
}
// Local Variables: 
// compile-command: "gmake NewMSDataDescColumns"
// End: 
