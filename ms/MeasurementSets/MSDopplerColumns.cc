//# MSDopplerColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSDopplerColumns.h>
#include <casacore/ms/MeasurementSets/MSDoppler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
MSDopplerColumns::MSDopplerColumns():
  isNull_p(True)
{
}

MSDopplerColumns::MSDopplerColumns(const MSDoppler& msDoppler):
  isNull_p(True)
{
  attach(msDoppler);
}

MSDopplerColumns::~MSDopplerColumns() {}

void MSDopplerColumns::attach(const MSDoppler& msDoppler)
{
  isNull_p = msDoppler.isNull();
  if (!isNull()) {
    dopplerId_p.attach(msDoppler, MSDoppler::
		       columnName(MSDoppler::DOPPLER_ID));
    sourceId_p.attach(msDoppler, MSDoppler::
		      columnName(MSDoppler::SOURCE_ID));
    transitionId_p.attach(msDoppler, MSDoppler::
			  columnName(MSDoppler::TRANSITION_ID));
    velDef_p.attach(msDoppler, MSDoppler::columnName(MSDoppler::VELDEF));
    velDefMeas_p.attach(msDoppler, MSDoppler::
			columnName(MSDoppler::VELDEF));
    velDefQuant_p.attach(msDoppler, MSDoppler::
			 columnName(MSDoppler::VELDEF));
  }
}

void MSDopplerColumns::setVelDefRef(MDoppler::Types ref)
{
  velDefMeas_p.setDescRefCode(ref,False);
}

} //# NAMESPACE CASACORE - END
