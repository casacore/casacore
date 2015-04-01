//# MSDataDescColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescription.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/TableDesc.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROMSDataDescColumns::ROMSDataDescColumns()
  :flagRow_p(),
   polarizationId_p(),
   spectralWindowId_p(),
   lagId_p()
{
}

ROMSDataDescColumns::
ROMSDataDescColumns(const MSDataDescription& msDataDesc):
  flagRow_p(msDataDesc, MSDataDescription::
	    columnName(MSDataDescription::FLAG_ROW)),
  polarizationId_p(msDataDesc, MSDataDescription::
		   columnName(MSDataDescription::POLARIZATION_ID)),
  spectralWindowId_p(msDataDesc, MSDataDescription::
		     columnName(MSDataDescription::SPECTRAL_WINDOW_ID)),
  lagId_p()
{
  attachOptionalCols(msDataDesc);
}

ROMSDataDescColumns::~ROMSDataDescColumns() {}

Int ROMSDataDescColumns::match(uInt spwId, uInt polId, Int tryRow) {
  uInt r = nrow();
  if (r == 0) return -1;
  const Int spw = spwId;
  const Int pol = polId;
  // Main matching loop
  if (tryRow >= 0) {
    const uInt tr = tryRow;
    if (tr >= r) {
      throw(AipsError("ROMSDataDescColumns::match(...) - "
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

void ROMSDataDescColumns::attach(const MSDataDescription& msDataDesc)
{
  flagRow_p.attach(msDataDesc, MSDataDescription::columnName
		   (MSDataDescription::FLAG_ROW));
  polarizationId_p.attach(msDataDesc, MSDataDescription::columnName
			  (MSDataDescription::POLARIZATION_ID));
  spectralWindowId_p.attach(msDataDesc, MSDataDescription::columnName
			    (MSDataDescription::SPECTRAL_WINDOW_ID));
  attachOptionalCols(msDataDesc);
}

void ROMSDataDescColumns::
attachOptionalCols(const MSDataDescription& msDataDesc)
{
  const ColumnDescSet& cds=msDataDesc.tableDesc().columnDescSet();
  if (cds.isDefined(MSDataDescription::
		    columnName(MSDataDescription::LAG_ID))) {
    lagId_p.attach(msDataDesc,MSDataDescription::
		   columnName(MSDataDescription::LAG_ID));
  }
}

MSDataDescColumns::MSDataDescColumns():
  ROMSDataDescColumns(),
  flagRow_p(),
  polarizationId_p(),
  spectralWindowId_p(),
  lagId_p()
{
}

MSDataDescColumns::MSDataDescColumns(MSDataDescription& msDataDesc):
  ROMSDataDescColumns(msDataDesc),
  flagRow_p(msDataDesc, MSDataDescription::
	    columnName(MSDataDescription::FLAG_ROW)),
  polarizationId_p(msDataDesc, MSDataDescription::
		   columnName(MSDataDescription::POLARIZATION_ID)),
  spectralWindowId_p(msDataDesc,MSDataDescription::
		     columnName(MSDataDescription::SPECTRAL_WINDOW_ID)),
  lagId_p()
{
  attachOptionalCols(msDataDesc);
}

MSDataDescColumns::~MSDataDescColumns() {}

void MSDataDescColumns::attach(MSDataDescription& msDataDesc)
{
  ROMSDataDescColumns::attach(msDataDesc);
  flagRow_p.attach(msDataDesc, MSDataDescription::columnName
		   (MSDataDescription::FLAG_ROW));
  polarizationId_p.attach(msDataDesc, MSDataDescription::columnName
			  (MSDataDescription::POLARIZATION_ID));
  spectralWindowId_p.attach(msDataDesc, MSDataDescription::columnName
			    (MSDataDescription::SPECTRAL_WINDOW_ID));
  attachOptionalCols(msDataDesc);
}

void MSDataDescColumns::
attachOptionalCols(MSDataDescription& msDataDesc)
{
  const ColumnDescSet& cds = msDataDesc.tableDesc().columnDescSet();
  if (cds.isDefined(MSDataDescription::
		    columnName(MSDataDescription::LAG_ID))) {
    lagId_p.attach(msDataDesc, MSDataDescription::
		   columnName(MSDataDescription::LAG_ID));
  }
}
// Local Variables: 
// compile-command: "gmake MSDataDescColumns"
// End: 

} //# NAMESPACE CASACORE - END

