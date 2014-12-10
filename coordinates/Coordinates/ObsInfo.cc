//# ObsInfo.cc: Miscellaneous information related to an observation
//# Copyright (C) 1998,1999,2000,2001,2002
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

#include <casacore/coordinates/Coordinates/ObsInfo.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/casa/BasicSL/Constants.h>

#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

String ObsInfo::defaultTelescope()
{
    return "UNKNOWN";
}

String ObsInfo::defaultObserver()
{
    return defaultTelescope();
}

MEpoch ObsInfo::defaultObsDate()
{
    MEpoch tmp;
    return tmp;
}

MVDirection ObsInfo::defaultPointingCenter ()
{
   MVDirection tmp(0.0, 0.0);
   return tmp;
}


ObsInfo::ObsInfo()
 : telescope_p(defaultTelescope()), 
   observer_p(defaultObserver()), 
   obsdate_p(defaultObsDate()),
   isTelPositionSet_p(False),
   pointingCenter_p(defaultPointingCenter()),
   isPointingCenterInitial_p(True)
{
    // Nothing
}

ObsInfo::~ObsInfo()
{
    // Nothing
}

void ObsInfo::copy_other(const ObsInfo &other)
{
    if (this != &other) {
	telescope_p = other.telescope_p;
	observer_p = other.observer_p;
	obsdate_p = other.obsdate_p;
        telPosition_p = other.telPosition_p;
        isTelPositionSet_p = other.isTelPositionSet_p;
        pointingCenter_p = other.pointingCenter_p;
        isPointingCenterInitial_p = other.isPointingCenterInitial_p;
    }
}

ObsInfo::ObsInfo(const ObsInfo &other)
  : RecordTransformable()
{
    copy_other(other);
}

ObsInfo &ObsInfo::operator=(const ObsInfo &other)
{
    copy_other(other);
    return *this;
}

String ObsInfo::telescope() const
{
    return telescope_p;
}

ObsInfo& ObsInfo::setTelescope(const String &telescope)
{
    telescope_p = telescope;
    if (!isTelPositionSet_p) {
      MPosition pos;
      if (MeasTable::Observatory (pos, telescope)) {
        setTelescopePosition (pos);
      }
    }
    return *this;
}

ObsInfo& ObsInfo::setTelescopePosition(const MPosition &pos)
{
    telPosition_p = MPosition::Convert(pos, MPosition::ITRF)();
    isTelPositionSet_p = True;
    return *this;
}

String ObsInfo::telescopePositionString() const
{
    ostringstream oss;
    if (isTelPositionSet_p) {
        MVPosition pos1 = telPosition_p.getValue();
        oss << "[" << pos1.getValue()[0] << "m, "
            << pos1.getValue()[1] << "m, "
            << pos1.getValue()[2] << "m] (ITRF)";
    }
    return oss.str();
}

String ObsInfo::observer() const
{
    return observer_p;
}

ObsInfo& ObsInfo::setObserver(const String &observer)
{
    observer_p = observer;
    return *this;
}

MEpoch ObsInfo::obsDate() const
{
    return obsdate_p;
}

ObsInfo& ObsInfo::setObsDate(const MEpoch &obsDate)
{
    obsdate_p = obsDate;
    return *this;
}

MVDirection ObsInfo::pointingCenter() const
{
   return pointingCenter_p;
}

ObsInfo& ObsInfo::setPointingCenter (const MVDirection& direction)
//
// Because the default value (0,0) is a legal pointing center,
// we maintain isPointingCenterInitial_p so that we can detect
// when the user has set a real pointing center.
//
{
   pointingCenter_p = direction;
   isPointingCenterInitial_p = False;
   return *this;
}


Bool ObsInfo::toRecord(String & error, RecordInterface & outRecord) const
{
    error = "";
//
    outRecord.define("telescope", telescope());
//
    outRecord.define("observer", observer());
//
    Bool ok = True;
    {
       MeasureHolder mh(obsDate());
       Record rec;
       ok = mh.toRecord(error, rec);
       if (ok) {
          outRecord.defineRecord("obsdate", rec);
       }
    }
//
    {
       Record rec;
       Vector<Double> v = pointingCenter().get();       // radians
       rec.define("value", v);
       rec.define("initial", isPointingCenterInitial_p);
       outRecord.defineRecord("pointingcenter", rec);
    }
//
    if (isTelPositionSet_p) {
       MeasureHolder mh(telPosition_p);
       Record rec;
       ok = mh.toRecord(error, rec);
       if (ok) {
          outRecord.defineRecord("telescopeposition", rec);
       }
    }
    return ok;
}

Bool ObsInfo::fromRecord(String & error, const RecordInterface & inRecord)
{
    error = "";
//
    ObsInfo tmp;
    (*this) = tmp; // Make sure we are "empty" first;
//
    Int field = inRecord.fieldNumber("telescope");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of telescope field is not String!";
	    return False;
	}
	setTelescope(inRecord.asString(field));
    }
//
    field = inRecord.fieldNumber("observer");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of observer field is not String!";
	    return False;
	}
	setObserver(inRecord.asString(field));
    }
//
    field = inRecord.fieldNumber("obsdate");
    if (field >= 0) {
	if (inRecord.type(field) != TpRecord) {
	    error = "Type of obsdate field is not Record!";
	    return False;
	}
	MeasureHolder mh;
	Bool ok = mh.fromRecord(error, inRecord.asRecord(field));
	if (!ok) {
	    return False;
	}
	if (!mh.isMEpoch()) {
	    error = "obsdate field is not an MEpoch!";
	    return False;
	}
	setObsDate(mh.asMEpoch());
    }
//
    field = inRecord.fieldNumber("telescopeposition");
    if (field >= 0) {
	if (inRecord.type(field) != TpRecord) {
	    error = "Type of telescopeposition field is not Record!";
	    return False;
	}
	MeasureHolder mh;
	Bool ok = mh.fromRecord(error, inRecord.asRecord(field));
	if (!ok) {
	    return False;
	}
	if (!mh.isMPosition()) {
	    error = "obsdate field is not an MPosition!";
	    return False;
	}
	setTelescopePosition(mh.asMPosition());
    } else {
        isTelPositionSet_p = False;
    }
//
    field = inRecord.fieldNumber("pointingcenter");
    if (field >= 0) {
	if (inRecord.type(field) != TpRecord) {
	    error = "Type of pointingcenter field is not Record !";
	    return False;
	}
        Record rec = inRecord.asRecord(field);
//
        Vector<Double> v;
        Int field2 = rec.fieldNumber("value");
        if (field2 >= 0) {
           v = Vector<Double>(rec.toArrayDouble(field2));
        } else {
           error = "field pointingcenter does not contain subfield 'value'";
           return False;
        }
//
        Bool b = False;
        Int field3 = rec.fieldNumber("initial");
        if (field3 >= 0) {
   	   if (rec.type(field3) != TpBool) {
              error = "pointingcenter.initial field is not Bool";
              return False;
           } else {
              b = rec.asBool(field3);
           }
        } else {
           error = "field pointingcenter does not contain subfield 'initial'";
           return False;
        }

// Don't use function "setPointingCenter" as it will set 
// isPointingCenterInitial_p to False
        
        isPointingCenterInitial_p = b;
        pointingCenter_p = MVDirection(v);
    }
//
    return True;
}

Bool ObsInfo::toFITS(String & error, RecordInterface & outRecord) const
{
    error = "";
//
    String name = "telescop";
    if (telescope() != defaultTelescope()) {
	outRecord.define(name, telescope());
    } else {
	// Remove it if it already exists
	Int field = outRecord.fieldNumber(name);
	if (field >= 0 && !outRecord.isFixed()) {
	    outRecord.removeField(field);
	}
    }
//
    name = "observer";
    if (observer() != defaultObserver()) {
	outRecord.define(name, observer());
    } else {
	// Remove it if it already exists
	Int field = outRecord.fieldNumber(name);
	if (field >= 0 && !outRecord.isFixed()) {
	    outRecord.removeField(field);
	}
    }
//
    // Sort of yucky, but avoids a cast!
    name = "date-obs";
    MVTime time = obsDate().get("s");
    if (time != MVTime(defaultObsDate().get("s"))) {  // Roundoff problems?

// Very (but only) longwinded way to get at the MEpoch::Types

        MEpoch::Types dtype(MEpoch::castType(obsDate().getRefPtr()->getType()));
	String date, timesys;
	FITSDateUtil::toFITS(date, timesys, time, dtype);
	outRecord.define(name, date);
	outRecord.define("timesys", timesys);
    } else {

// Remove it if it already exists
// Maybe we should also remove TIMESYS, but it is conceivably needed
// for some other DATE field. FITS sure is yuck-o.

	Int field = outRecord.fieldNumber(name);
	if (field >= 0 && !outRecord.isFixed()) {
           outRecord.removeField(field);
	}
    }
//
    if (!isPointingCenterInitial_p) {
       String nameLong("obsra");
       String nameLat("obsdec");

// Normalize Longitude to 0->360 deg

       MVAngle lon1(pointingCenter().get()(0));
       MVAngle lon2 = lon1();           // +/- pi
       Double lon3 = lon2.degree();     // +/- 180
       if (lon3 < 0) lon3 += 360.0;     // 0 -> 360
       outRecord.define(nameLong, lon3);
//
       Double lat = pointingCenter().getLat(Unit("deg")).getValue();
       outRecord.define(nameLat, lat);
    } else {

// Remove it if it already exists

       String nameLong = "obsra";
       String nameLat  = "obsdec";
//
       Int field = outRecord.fieldNumber(nameLong);
       if (field >= 0 && !outRecord.isFixed()) {
          outRecord.removeField(field);
       }
       field = outRecord.fieldNumber(nameLat);
       if (field >= 0 && !outRecord.isFixed()) {
          outRecord.removeField(field);
       }
    }
//
    Vector<String> names(3);
    names[0] = "obsgeo-x";
    names[1] = "obsgeo-y";
    names[2] = "obsgeo-z";
    if (isTelPositionSet_p) {
       // Store position in ITRF meters.
       MVPosition mvpos = telPosition_p.getValue();
       for (int i=0; i<3; ++i) {
         outRecord.define (names[i], mvpos.getValue()[i]);
       }
    } else {
       // Remove it if it already exists
       for (int i=0; i<3; ++i) {
          Int field = outRecord.fieldNumber(names[i]);
          if (field >= 0 && !outRecord.isFixed()) {
             outRecord.removeField(field);
          }
       }
    }
//
    return True;
}

Bool ObsInfo::fromFITS(Vector<String>& error, const RecordInterface & rec)
{
    error.resize(4);
//
    Bool ok = True;
    ObsInfo tmp;
    (*this) = tmp; // Make sure we are "empty" first;

// Item 0 (might be in 'TELESCOP' or 'INSTRUME' and they might
// both be there and they might also hold only spaces)

    Bool done = False;
    String field("telescop");
    if (rec.isDefined(field)) {
       Record subRec = rec.asRecord(field);
       if (subRec.dataType(String("value")) != TpString) {
          error(0) = "Type of TELESCOP field is not String!";
          ok = False;
       } else {
          String ss = subRec.asString(String("value"));
          ss = ss.before(' ');
	  if (ss.length()>0) {
             setTelescope(ss);
             done = True;	       
          }
       }
    }
//
    if (!done) {
       field = String("instrume");
       if (rec.isDefined(field)) {
          Record subRec = rec.asRecord(field);
          if (subRec.dataType(String("value")) != TpString) {
             error(0) = "Type of INSTRUME field is not String!";
             ok = False;
         } else {
             setTelescope(subRec.asString(String("value")));
         }
       }	    
    }

// Item 1

    field = String("observer");
    if (rec.isDefined(field)) {
       Record subRec = rec.asRecord(field);
       if (subRec.dataType("value") != TpString) {
          error(1) = "Type of OBSERVER field is not String!";
          ok = False;
       } else {
          setObserver(subRec.asString("value"));
       }
    }

// Item 2

    String field1("date-obs");
    String field2("timesys");
    String timeSysStr("UTC");
//
    if (rec.isDefined(field1)) {
       Record subRec1 = rec.asRecord(field1);
       if (subRec1.dataType("value") != TpString) {
          error(2) = "Type of DATE-OBS field is not a String!";
          ok = False;
       } else {
          if (rec.isDefined(field2)) {
             Record subRec2 = rec.asRecord(field2);
             if (subRec2.dataType("value") == TpString) {
                timeSysStr = subRec2.asString("value");
             }
          }
//
          MVTime time; MEpoch::Types timeSys;
          String dateString = subRec1.asString("value");
          Bool ok2 = FITSDateUtil::fromFITS(time, timeSys, dateString, timeSysStr);
          if (ok2) {
             setObsDate(MEpoch(time.get(), timeSys));
          } else {
             error(2) = "Could not decode FITS date format from keywords";
             ok = False;
          }
       }
    }

// Item 3

    Int fieldLong = rec.fieldNumber("obsra");
    Int fieldLat  = rec.fieldNumber("obsdec");
    if (fieldLong>=0 && fieldLat>=0) {
       Record subRec1 = rec.asRecord(fieldLong);
       Record subRec2 = rec.asRecord(fieldLat);
	if (subRec1.dataType("value") != TpDouble ||
            subRec2.dataType("value") != TpDouble) {
	    error(3) = "Type of OBSRA or OBSDEC field is not Double!";
	    ok = False;
	} else {
           MVDirection mvd((subRec1.asDouble("value"))*C::pi/180.0,
                           (subRec2.asDouble("value"))*C::pi/180.0);
   	   setPointingCenter(mvd);
        }
    }

// Item 4

    Int fieldx = rec.fieldNumber("obsgeo-x");
    Int fieldy = rec.fieldNumber("obsgeo-y");
    Int fieldz = rec.fieldNumber("obsgeo-z");
    if (fieldx>=0 && fieldy>=0 && fieldz>=0) {
       Record subRec1 = rec.asRecord(fieldx);
       Record subRec2 = rec.asRecord(fieldy);
       Record subRec3 = rec.asRecord(fieldz);
	if (subRec1.dataType("value") != TpDouble ||
            subRec2.dataType("value") != TpDouble ||
            subRec3.dataType("value") != TpDouble) {
	    error(3) = "Type of OBSGEO fields is not Double!";
	    ok = False;
	} else {
           MVPosition mvp(subRec1.asDouble("value"),
                          subRec2.asDouble("value"),
                          subRec3.asDouble("value"));
   	   setTelescopePosition(MPosition(mvp, MPosition::ITRF));
        }
    }
//
    if (ok) error.resize(0);
    return ok;
}



Vector<String> ObsInfo::keywordNamesFITS()
{
    Vector<String> vs(9);
    vs(0) = "telescop";
    vs(1) = "observer";
    vs(2) = "date-obs";
    vs(3) = "timesys";
    vs(4) = "obsra";
    vs(5) = "obsdec";
    vs(6) = "obsgeo-x";
    vs(7) = "obsgeo-y";
    vs(8) = "obsgeo-z";
    return vs;
}

ostream &operator<<(ostream &os, const ObsInfo &info)
{
    os << "Telescope: " << info.telescope();
    if (info.isTelescopePositionSet()) {
        os << " Position: " << info.telescopePositionString();
    }
    os << " Observer: " <<
	info.observer() << " Date Observed: " << info.obsDate() <<
        " Pointing Center: " << info.pointingCenter();
    return os;
}

} //# NAMESPACE CASACORE - END

