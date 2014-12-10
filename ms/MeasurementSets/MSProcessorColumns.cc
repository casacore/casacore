//# MSProcessorColumns.cc:  provides easy access to MeasurementSet columns
//# Copyright (C) 1999,2000
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

#include <casacore/ms/MeasurementSets/MSProcessorColumns.h>
#include <casacore/ms/MeasurementSets/MSProcessor.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROMSProcessorColumns::
ROMSProcessorColumns(const MSProcessor& msProcessor):
  flagRow_p(msProcessor, MSProcessor::columnName(MSProcessor::FLAG_ROW)),
  modeId_p(msProcessor, MSProcessor::columnName(MSProcessor::MODE_ID)),
  type_p(msProcessor, MSProcessor::columnName(MSProcessor::TYPE)),
  typeId_p(msProcessor, MSProcessor::columnName(MSProcessor::TYPE_ID)),
  subType_p(msProcessor, MSProcessor::columnName(MSProcessor::SUB_TYPE)),
  passId_p()
{
  attachOptionalCols(msProcessor);
}

ROMSProcessorColumns::~ROMSProcessorColumns() {}

ROMSProcessorColumns::ROMSProcessorColumns():
  flagRow_p(),
  modeId_p(),
  type_p(),
  typeId_p(),
  subType_p(),
  passId_p()
{
}

void ROMSProcessorColumns::attach(const MSProcessor& msProcessor)
{
  flagRow_p.attach(msProcessor, MSProcessor::
		   columnName(MSProcessor::FLAG_ROW));
  modeId_p.attach(msProcessor, MSProcessor::
		  columnName(MSProcessor::MODE_ID));
  type_p.attach(msProcessor, MSProcessor::
		columnName(MSProcessor::TYPE));
  typeId_p.attach(msProcessor, MSProcessor::
		  columnName(MSProcessor::TYPE_ID));
  subType_p.attach(msProcessor, MSProcessor::
		   columnName(MSProcessor::SUB_TYPE));
  attachOptionalCols(msProcessor);
}

void ROMSProcessorColumns::
attachOptionalCols(const MSProcessor& msProcessor)
{
  const ColumnDescSet& cds=msProcessor.tableDesc().columnDescSet();
  const String& passId=MSProcessor::columnName(MSProcessor::PASS_ID);
  if (cds.isDefined(passId)) passId_p.attach(msProcessor, passId);
}

MSProcessorColumns::MSProcessorColumns(MSProcessor& msProcessor):
  ROMSProcessorColumns(msProcessor),
  flagRow_p(msProcessor, MSProcessor::columnName(MSProcessor::FLAG_ROW)),
  modeId_p(msProcessor, MSProcessor::columnName(MSProcessor::MODE_ID)),
  type_p(msProcessor, MSProcessor::columnName(MSProcessor::TYPE)),
  typeId_p(msProcessor, MSProcessor::columnName(MSProcessor::TYPE_ID)),
  subType_p(msProcessor, MSProcessor::columnName(MSProcessor::SUB_TYPE))
{
  const ColumnDescSet& cds=msProcessor.tableDesc().columnDescSet();
  const String& passId=MSProcessor::columnName(MSProcessor::PASS_ID);
  if (cds.isDefined(passId)) passId_p.attach(msProcessor, passId);
}

MSProcessorColumns::~MSProcessorColumns() {}

MSProcessorColumns::MSProcessorColumns():
  ROMSProcessorColumns(),
  flagRow_p(),
  modeId_p(),
  type_p(),
  typeId_p(),
  subType_p(),
  passId_p()
{
}

void MSProcessorColumns::attach(MSProcessor& msProcessor)
{
  ROMSProcessorColumns::attach(msProcessor);
  flagRow_p.attach(msProcessor, MSProcessor::
		   columnName(MSProcessor::FLAG_ROW));
  modeId_p.attach(msProcessor, MSProcessor::
		  columnName(MSProcessor::MODE_ID));
  type_p.attach(msProcessor, MSProcessor::
		columnName(MSProcessor::TYPE));
  typeId_p.attach(msProcessor, MSProcessor::
		  columnName(MSProcessor::TYPE_ID));
  subType_p.attach(msProcessor, MSProcessor::
		   columnName(MSProcessor::SUB_TYPE));
  attachOptionalCols(msProcessor);
}

void MSProcessorColumns::attachOptionalCols(MSProcessor& msProcessor)
{
  const ColumnDescSet& cds=msProcessor.tableDesc().columnDescSet();
  const String& passId=MSProcessor::columnName(MSProcessor::PASS_ID);
  if (cds.isDefined(passId)) passId_p.attach(msProcessor, passId);
}
// Local Variables: 
// compile-command: "gmake MSProcessorColumns"
// End: 

} //# NAMESPACE CASACORE - END

