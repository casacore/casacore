//# NewMSProcessorColumns.cc:  provides easy access to NewMeasurementSet columns
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

//# Includes
#include <aips/MeasurementSets/NewMSProcessorColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>

NewMSProcessorColumns::NewMSProcessorColumns(NewMSProcessor& msProcessor):
flagRow_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::FLAG_ROW)),
modeId_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::MODE_ID)),
type_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::TYPE)),
typeId_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::TYPE_ID)),
subType_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::SUB_TYPE))
{
  const ColumnDescSet& cds=msProcessor.tableDesc().columnDescSet();
  const String& passId=NewMSProcessor::columnName(NewMSProcessor::PASS_ID);
  if (cds.isDefined(passId)) passId_p.attach(msProcessor,passId);
}

NewMSProcessorColumns::~NewMSProcessorColumns() {}

RONewMSProcessorColumns::RONewMSProcessorColumns(const NewMSProcessor& msProcessor):
flagRow_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::FLAG_ROW)),
modeId_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::MODE_ID)),
type_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::TYPE)),
typeId_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::TYPE_ID)),
subType_p(msProcessor,NewMSProcessor::columnName(NewMSProcessor::SUB_TYPE))
{
  const ColumnDescSet& cds=msProcessor.tableDesc().columnDescSet();
  const String& passId=NewMSProcessor::columnName(NewMSProcessor::PASS_ID);
  if (cds.isDefined(passId)) passId_p.attach(msProcessor,passId);
}

RONewMSProcessorColumns::~RONewMSProcessorColumns() {}



