//# NewMSValidIds: ensures that required MS Ids are valid or -1 by row number
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
//#
//# $Id$

#if !defined(AIPS_NEWMSVALIDIDS_H)
#define AIPS_NEWMSVALIDIDS_H

#include <aips/aips.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>

//# Forward declarations
class RONewMSColumns;

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMeasurementSet
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>

class NewMSValidIds
{
public:
    // Construct one unattached to a NewMeasurementSet.  All functions return -1.
    // Use the attach function to attach this to a NewMeasurementSet after construction.
    NewMSValidIds();

    // Construct one attached to the indicated NewMeasurementSet
    NewMSValidIds(const NewMeasurementSet &ms);

    // Construct one from another
    NewMSValidIds(const NewMSValidIds &other);

    // The destructor
    ~NewMSValidIds();

    // Assignment operator, reference semantics.
    NewMSValidIds &operator=(const NewMSValidIds &other);

    // Attach this one to a NewMeasurementSet.  This can also be used to
    // re-attach to the same NewMeasurementSet when additional optional 
    // subtables have been added since this object was constructed.
    void attach(const NewMeasurementSet &ms);

    // These functions check on the validity of the appropriate value in
    // the main table or sub-tables in the case of some Ids.  The actual
    // value stored is returned unless the sub-table does not exist (for
    // optional subtables) or the indicated row number does not exist
    // in that sub-table where appropriate.
    // <group>
    Int antenna1(uInt rownr) const;
    Int antenna2(uInt rownr) const;
    Int dataDescId(uInt rownr) const;
    Int fieldId(uInt rownr) const;
    Int observationId(uInt rownr) const;
    Int processorId(uInt rownr) const;
    Int stateId(uInt rownr) const;
    // The polarizationId comes from the DATA_DESCRIPTION subtable, so dataDescId must
    // first be valid in order for this to also be valid.
    Int polarizationId(uInt rownr) const;
    // The spectralWindowId comes from the DATA_DESCRIPTION subtable, so dataDescId must
    // first be valid in order for this to also be valid.
    Int spectralWindowId(uInt rownr) const;
    // the dopplerId comes from the SPECTRAL_WINDOW subtable so spectralWindowId must
    // first be valid in order for this to also be valid.  Since the DOPPLER subtable
    // is not simply indexed by DOPPLER_ID, the DOPPLER subtable exists and a dopplerId
    // can be found in the SPECTRAL_WINDOW subtable, that value will be returned, whatever
    // it is.
    Int dopplerId(uInt rownr) const;
    // The sourceId comes from the FIELD subtable so fieldId must first be valid
    // in order for this to also be valid.  Since the SOURCE table is also
    // indexed by TIME, the only additional check is that a SOURCE table must
    // exist in order for this to be valid.
    Int sourceId(uInt rownr) const;
    // </group>
private:
    NewMeasurementSet ms_p;
    RONewMSColumns *romsCols_p;

    Bool hasDoppler_p, hasSource_p;
    
    void clear();
    Int checkResult(Int testResult, const Table &mstable) const
    { return (testResult < 0 || uInt(testResult) >= mstable.nrow()) ? -1 : testResult;}

    Bool checkRow(uInt rownr) const {return rownr < ms_p.nrow();}
};

#endif
