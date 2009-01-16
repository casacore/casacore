//# ObsInfo.h: Store miscellaneous information related to an observation
//# Copyright (C) 1998,2000,2001
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

#ifndef COORDINATES_OBSINFO_H
#define COORDINATES_OBSINFO_H

#include <casa/aips.h>
#include <casa/Utilities/RecordTransformable.h>

#include <casa/BasicSL/String.h>
#include <measures/Measures/MEpoch.h>
#include <casa/Quanta/MVDirection.h>

//# Forward declarations
#include <casa/iosfwd.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Store miscellaneous information related to an observation.
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
//    <li> Pointing centre (as distinct from the phase center or tangent point)
// </ol>
// This list can easily be extended if necessary.
//
// This class has functions to interconvert with a record in a "lossless"
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
// Record information to allow more full measures conversion, e.g. topo->lsr
// requires observatory location and time. Also record in a typesafe way
// image summary information users are used to from classic AIPS.
// </motivation>
//
// <todo asof="2000/04/20">
//   <li> Nothing known
// </todo>

class ObsInfo : public RecordTransformable
{
public:

    // Default constructor makes an object where all the 
    // parameters are set to their default values (see below)
    ObsInfo();

    // Destructor
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

    // The name (or initials) of the observer. Defaults to "UNKNOWN".
    // <group>
    String observer() const;
    ObsInfo& setObserver(const String &observer);
    // </group>
    
    // When was the observation taken (start time)? This is somewhat
    // problematical for observations which are taken at multiple
    // epochs. You should use the start time of the first sample.
    // The default is the MEpoch default: MJD 0 UTC
    // <group>
    MEpoch obsDate() const;
    ObsInfo& setObsDate(const MEpoch &obsDate);
    // </group>

    // What was the pointing centre, as distinct from the phase centre ? 
    // This value is specified as an MVDirection.
    // This means it is you, the callers responsibility, to know what its reference
    // type is in order to turn it into an MDirection.
    // The default is (0,0) (or [1,0,0]).  After you have called setPointingCenter,
    // the function isPointingCenterInitial will return False.
    // <group>
    MVDirection pointingCenter() const;
    ObsInfo& setPointingCenter (const MVDirection& direction);
    // </group>
    
    // Because the default pointing center is a valid value (0,0), this
    // function is available to tell you whether the pointing center has
    // been set (with setPointingCenter) to some value other than its
    // initial (return False)
    Bool isPointingCenterInitial () const {return isPointingCenterInitial_p;};


    // Functions to interconvert between an ObsInfo and a record. These 
    // functions are inherited from class
    // <linkto class=RecordTransformable>RecordTransformable</linkto>. As new
    // fields get added to ObsInfo these functions should be augmented. Missing
    // fields should not generate an error to in fromRecord to allow for 
    // backwards compatibility - null values should be supplied instead.
    // The field names are "observer", "telescope", "obsdate", and
    // "pointingcenter"
    // <group>
    virtual Bool toRecord(String & error, RecordInterface & outRecord) const;
    virtual Bool fromRecord(String & error, const RecordInterface & inRecord);
    // </group>

    // Functions to interconvert between an ObsInfo and FITS keywords
    // (converted to a Record).  For the pointing center, the FITS
    // keywords OBSRA and OBSDEC are used.    Failure of <src>fromFITS</src>
    // should probably not be regarded as fatal as the default ObsInfo
    // values are viable.  For each item contained
    // in the ObsInfo, an attempt to decode it from FITS is made.
    // If any of them fail, False is returned, but it attempts to decode
    // them all.  For those that fail
    // an error message is held in <src>error</src> 
    // in the order telescope (error(0)), observer (error(1)), date
    // (error(2)), pointing center (error(3)).  <src>error</src> will
    // be returned of length 0 if the return value is True, else
    // it will be length 4.
    // <group>
    Bool toFITS(String & error, RecordInterface & outRecord) const;
    Bool fromFITS(Vector<String>& error, const RecordInterface & inRecord);

    // In some circumstances it might be useful to know what the defaults for
    // the various values are so you can check if they have been set.
    // <group>
    static String defaultTelescope();
    static String defaultObserver();
    static MEpoch defaultObsDate();
    static MVDirection defaultPointingCenter();
    // </group>

    // It might be useful to know what FITS keyword names are used in to/from
    // FITS so we can remove them so they won't be used more than once. The
    // names are in lower case.
    static Vector<String> keywordNamesFITS();

private:
    String telescope_p;
    String observer_p;
    MEpoch obsdate_p;
    MVDirection pointingCenter_p;
    Bool isPointingCenterInitial_p;    // True when ObsInfo contructed. 
                                       // False after setPointingCenter called

// Common copy ctor/assignment operator code.

    void copy_other(const ObsInfo &other);
};

// <summary> Global functions </summary>
// <group name=Output>
// Output declaration - useful for debugging.
ostream &operator<<(ostream &os, const ObsInfo &info);
// </group>


} //# NAMESPACE CASA - END

#endif
