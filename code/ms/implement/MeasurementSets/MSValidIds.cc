//# NewMSValidIds.cc:  this defines NewMSValidIds.
//# Copyright (C) 2000
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

#include <trial/MeasurementSets/NewMSValidIds.h>

#include <aips/Exceptions/Error.h>
#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

NewMSValidIds::NewMSValidIds()
    : romsCols_p(0), hasDoppler_p(False), hasSource_p(False)
{ ;}

NewMSValidIds::NewMSValidIds(const NewMeasurementSet &ms)
    : romsCols_p(0), hasDoppler_p(False), hasSource_p(False)
{ attach(ms);}

NewMSValidIds::NewMSValidIds(const NewMSValidIds &other)
    : romsCols_p(0), hasDoppler_p(False), hasSource_p(False)
{ *this = other;}

NewMSValidIds::~NewMSValidIds()
{ clear(); }

NewMSValidIds &NewMSValidIds::operator=(const NewMSValidIds &other)
{ 
    if (this != &other) {
	attach(other.ms_p);
    }
    return *this;
}

void NewMSValidIds::attach(const NewMeasurementSet &ms)
{
    clear();
    
    ms_p = ms;

    romsCols_p = new RONewMSColumns(ms_p);
    AlwaysAssert(romsCols_p, AipsError);

    // check on existance of optional sub-tables
    hasDoppler_p = ms_p.keywordSet().isDefined("DOPPLER");
    hasSource_p = ms_p.keywordSet().isDefined("SOURCE");
}

Int NewMSValidIds::antenna1(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->antenna1()(rownr), ms_p.antenna());
    }
    return result;
}

Int NewMSValidIds::antenna2(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->antenna2()(rownr), ms_p.antenna());
    }
    return result;
}

Int NewMSValidIds::dataDescId(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->dataDescId()(rownr), ms_p.dataDescription());
    }
    return result;
}

Int NewMSValidIds::fieldId(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->fieldId()(rownr), ms_p.field());
    }
    return result;
}

Int NewMSValidIds::observationId(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->observationId()(rownr), ms_p.observation());
    }
    return result;
}

Int NewMSValidIds::processorId(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->processorId()(rownr), ms_p.processor());
    }
    return result;
}

Int NewMSValidIds::stateId(uInt rownr) const
{
    Int result = -1;
    if (checkRow(rownr) && romsCols_p) {
	result = checkResult(romsCols_p->stateId()(rownr), ms_p.state());
    }
    return result;
}

Int NewMSValidIds::polarizationId(uInt rownr) const
{
    Int result = dataDescId(rownr);
    if (result >= 0) {
	result = checkResult(romsCols_p->dataDescription().polarizationId()(result), ms_p.polarization());
    }
    return result;
}

Int NewMSValidIds::spectralWindowId(uInt rownr) const
{
    Int result = dataDescId(rownr);
    if (result >= 0) {
	result = checkResult(romsCols_p->dataDescription().spectralWindowId()(result), ms_p.spectralWindow());
    }
    return result;
}

Int NewMSValidIds::dopplerId(uInt rownr) const
{
    Int result = hasDoppler_p ? spectralWindowId(rownr) : -1;
    if (result >= 0) {
	result = romsCols_p->spectralWindow().dopplerId().isNull() ? -1 : 
	    romsCols_p->spectralWindow().dopplerId()(result);
    }
    return result;
}

Int NewMSValidIds::sourceId(uInt rownr) const
{
    Int result = hasSource_p ? fieldId(rownr) : -1;
    if (result >= 0) {
	result = romsCols_p->field().sourceId()(result);
    }
    return result;
}

void NewMSValidIds::clear()
{
    delete romsCols_p;
    romsCols_p = 0;

    hasDoppler_p = hasSource_p = False;
}
