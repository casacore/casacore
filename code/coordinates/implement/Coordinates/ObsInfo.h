//# ObsInfo.h: Miscellaneous information related to an observation
//# Copyright (C) 1998
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

#if !defined(AIPS_OBS_INFO_H)
#define AIPS_OBS_INFO_H

#include <aips/aips.h>
#include <aips/Utilities/RecordTransformable.h>

#include <aips/Utilities/String.h>
#include <aips/Measures/MEpoch.h>

#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
class ostream;
#endif

// <summary>
// Miscellaneous information related to an observation.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordTransformable>RecordTransformable</linkto>
//   <li> <linkto class=MEpoch>MEpoch</linkto>
// </prerequisite>
//
// <synopsis>
// This class is used to record miscellaneous information about an observation.
// At present it contains the following:
// <ol>
//    <li> Telescope name
//    <li> Observer name
//    <li> Observation date
// </ol>
// This list can easily be extended if necessary.
//
// This class has functions to interconvert between a record in a "lossless"
// fashion, and to also interconvert between a record that contains a list of
// FITS keywords.
// </synopsis>
//
// <example>
// The interface is a simple get/set interface. Note that the "set" methods
// can be chained together since each set method returns a reference to its
// object (rather like cout).
// <srcblock>
//    ObsInfo oi;
//    oi.setTelescope("VLA").setObserver("Glendenning");
//    ...
//    cout << "The date observed is: " << oi.obsDate() << endl;
// </srcblock>
// </example>
//
// <motivation>
// Record in a typesafe way image summary information users are used to from
// classic AIPS.
// </motivation>
//
// <todo asof="1998/08/25">
//   <li> Nothing known
// </todo>

class ObsInfo : public RecordTransformable
{
public:
    ObsInfo();
    ~ObsInfo();

    // Copy all fields from "other" to this object. Uses copy semantics.
    // <group>
    ObsInfo(const ObsInfo &other);
    ObsInfo &operator=(const ObsInfo &other);
    // </group>

    // Telescope identifier. If this is a "standard" telescope, you should use
    // the same name as is available in the Observatories method of class
    // <linkto class=MeasTable>MeasTable</linkto>. Defaults to "UNKNOWN".
    // <group>
    String telescope() const;
    ObsInfo& setTelescope(const String &telescope);
    // </group>

    // The name (or initialis) of the observer. Defaults to "UNKNOWN".
    // <group>
    String observer() const;
    ObsInfo& setObserver(const String &observer);
    // </group>
    
    // When was the observation taken (start time)? This is somewhat
    // problematical for observations which are taken at multiple
    // epochs. You should use the start time of the first observation.
    // The default is the MEpoch default: MJD 0 UTC
    // <group>
    MEpoch obsDate() const;
    ObsInfo &setObsDate(const MEpoch &obsDate);
    // </group>

    // Functions to interconvert between an ObsInfo and a record. These 
    // functions are inherited from class
    // <linkto class=RecordTransformable>RecordTransformable</class>. As new
    // fields get added to ObsInfo these functions should be augmented. Missing
    // fields should not generate an error to in fromRecord to allow for 
    // backwards compatibility - null values should be supplied instead.
    // <group>
    virtual Bool toRecord(String & error, RecordInterface & outRecord) const;
    virtual Bool fromRecord(String & error, const RecordInterface & inRecord);
    // </group>

    // Functions to interconvert between an ObsInfo and FITS keywords
    // (converted to a Record).
    // <group>
    Bool toFITS(String & error, RecordInterface & outRecord) const;
    Bool fromFITS(String & error, const RecordInterface & inRecord);
    // </group>

    // In some circumstances it might be useful to know what the defaults for
    // the various values are so you can check if they have been set.
    // <group>
    static String defaultTelescope();
    static String defaultObserver();
    static MEpoch defaultObsDate();
    // </group>

    // It might be useful to know what FITS keyword names are used in to/from
    // FITS so we can remove them so they won't be used more than once. The
    // names are in lower case.
    static Vector<String> keywordNamesFITS();
private:
    String telescope_p;
    String observer_p;
    MEpoch obsdate_p;

    // Common copy ctor/assignment operator code.
    void copy_other(const ObsInfo &other);
};

// <summary> Global functions </summary>
// <group name=Output>
// Output declaration - useful for debugging.
ostream &operator<<(ostream &os, const ObsInfo &info);
// </group>

#endif
