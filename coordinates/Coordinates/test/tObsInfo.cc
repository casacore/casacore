//# tObsInfo.cc: test program for class ObsInfo
//# Copyright (C) 1998,2000,2001,2004
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

#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>

#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
int main()
{
// Default constructor

    ObsInfo oi;

// Default values

    AlwaysAssertExit(oi.telescope() == oi.defaultTelescope());
    AlwaysAssertExit(oi.defaultTelescope() == "UNKNOWN");
//
    AlwaysAssertExit(oi.observer() == oi.defaultObserver());
    AlwaysAssertExit(oi.defaultObserver() == "UNKNOWN");
//
    MEpoch dflt = oi.defaultObsDate();
    AlwaysAssertExit(near(oi.obsDate().get("s").getValue(), 0.0));
//
    AlwaysAssertExit(oi.isPointingCenterInitial());
    MVDirection dmvd = oi.defaultPointingCenter();
    Vector<Double> v = dmvd.get();
    AlwaysAssertExit(near(v(0), 0.0));
    AlwaysAssertExit(near(v(1), 0.0));
    MPosition telPos( MVPosition( Quantity( 10, "m"),
                                  Quantity( -6, "deg"),
                                  Quantity( 50, "deg")),
                      MPosition::Ref(MPosition::WGS84));

// Set items

    oi.setTelescope("telescope").setObserver("observer").
	setObsDate(MVEpoch(1234.0)).setPointingCenter(MVDirection(0.01, 0.02));
    AlwaysAssertExit(oi.telescope() == "telescope" &&
		     oi.observer() == "observer" &&
		     near(oi.obsDate().get("d").getValue(), 1234.0) &&
                     near(oi.pointingCenter().get()(0),0.01) &&
                     near(oi.pointingCenter().get()(1),0.02));
    AlwaysAssertExit(!oi.isTelescopePositionSet());
    AlwaysAssertExit(!oi.isPointingCenterInitial());
    oi.setTelescopePosition(telPos);
    AlwaysAssertExit(oi.isTelescopePositionSet());
    MPosition mpos1 = MPosition::Convert (oi.telescopePosition(),
                                          MPosition::WGS84)();
    MVPosition pos1 = mpos1.getValue();
    AlwaysAssertExit (near (pos1.get()[0], 10., 1e-5));
    AlwaysAssertExit (near (pos1.getLong("deg").getValue(), -6., 1e-5));
    AlwaysAssertExit (near (pos1.getLat("deg").getValue(), 50., 1e-5));

// Copy constructor and assignment

    ObsInfo oi2(oi);
    AlwaysAssertExit(oi2.telescope() == "telescope" &&
		     oi2.observer() == "observer" &&
		     near(oi2.obsDate().get("d").getValue(), 1234.0) &&
                     near(oi2.pointingCenter().get()(0),0.01) &&
                     near(oi2.pointingCenter().get()(1),0.02));
    AlwaysAssertExit(!oi2.isPointingCenterInitial());
    AlwaysAssertExit(oi2.isTelescopePositionSet());

//
    ObsInfo oi2a;
    Double dateVal = 55000.5;
    oi2a.setTelescope("telescope2").setObserver("observer2").
         setObsDate(MVEpoch(dateVal)).setPointingCenter(MVDirection(0.03,0.04));
    oi = oi2a;
    AlwaysAssertExit(oi.telescope() == "telescope2" &&
		     oi.observer() == "observer2" &&
		     near(oi.obsDate().get("d").getValue(), dateVal) &&
                     near(oi.pointingCenter().get()(0),0.03) &&
                     near(oi.pointingCenter().get()(1),0.04));
    AlwaysAssertExit(!oi.isPointingCenterInitial());
    AlwaysAssertExit(!oi.isTelescopePositionSet());
    oi.setTelescopePosition(telPos);

// Test output.  
    cout << oi << endl;

// Record interface

    Record rec;
    String error;
    AlwaysAssertExit(oi.toRecord(error, rec));
    ObsInfo oi3;
    AlwaysAssertExit(oi3.fromRecord(error, rec));
    AlwaysAssertExit(oi3.telescope() == "telescope2" &&
		     oi3.observer() == "observer2" &&
		     near(oi3.obsDate().get("d").getValue(), dateVal) &&
                     near(oi3.pointingCenter().get()(0),0.03) &&
                     near(oi3.pointingCenter().get()(1),0.04));
    AlwaysAssertExit(oi3.isTelescopePositionSet());
    MPosition mpos3 = MPosition::Convert (oi.telescopePosition(),
                                          MPosition::WGS84)();
    MVPosition pos3 = mpos3.getValue();
    AlwaysAssertExit (near (pos3.get()[0], 10., 1e-5));
    AlwaysAssertExit (near (pos3.getLong("deg").getValue(), -6., 1e-5));
    AlwaysAssertExit (near (pos3.getLat("deg").getValue(), 50., 1e-5));

    Record reca;
    AlwaysAssertExit(oi2a.toRecord(error, reca));
    AlwaysAssertExit(oi2.isTelescopePositionSet());
    AlwaysAssertExit(oi2.fromRecord(error, reca));
    AlwaysAssertExit(!oi2.isTelescopePositionSet());

// Forced errors

    {
       Record rec3;
       Double x = 0;
       rec3.define("telescope", x);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Double x = 0;
       rec3.define("observer", x);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Double x = 0;
       rec3.define("obsdate", x);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Record rec4;
       Double x = 0;
       rec4.define("doggies", x); 
       rec3.defineRecord("obsdate", rec4);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Double x = 0;
       rec3.define("pointingcenter", x);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Record rec4;
       rec3.defineRecord("pointingcenter", rec4);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Record rec4;
       Double x(0);
       rec4.define("value", x);
       rec3.defineRecord("pointingcenter", rec4);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }
    {
       Record rec3;
       Record rec4;
       Vector<Double> x(2);
       rec4.define("value", x);
       Double y = 0.0;
       rec4.define("initial", y);
       rec3.defineRecord("pointingcenter", rec4);
       AlwaysAssertExit(!oi3.fromRecord(error, rec3));
    }


// FITS

    Vector<String> error2;
    Record rec2;
    cerr << oi << endl;
    AlwaysAssertExit(oi.toFITS(error, rec2));
    AlwaysAssertExit(oi.toFITS(error, rec2));      // Second pass removes pre-existing fields

    // the record delivered by toFITS contains fields as fields,
    // the record accepted by fromFITS contains fields as subrecords
    //  -- a round trip is therefore not possible directly.
    Record rec3;
    for (uInt i=0; i<rec2.nfields(); ++i) {
      Record subrec;
      if (rec2.dataType(i) == TpDouble) {
        subrec.define ("value", rec2.asDouble(i));
      } else {
        subrec.define ("value", rec2.asString(i));
      }
      rec3.defineRecord (rec2.name(i), subrec);
    }
    // now try to import it
    ObsInfo oi4;

    AlwaysAssertExit(oi4.fromFITS(error2, rec3));
    cerr << oi4 << endl;

    AlwaysAssertExit(oi4.telescope() == "telescope2");
    AlwaysAssertExit(oi4.observer() == "observer2");
    AlwaysAssertExit(near(oi4.obsDate().get("d").getValue(),
                          oi.obsDate().get("d").getValue()));
    AlwaysAssertExit(near(oi4.pointingCenter().get()(0),
                          oi.pointingCenter().get()(0)));
    AlwaysAssertExit(near(oi4.pointingCenter().get()(1),
                          oi.pointingCenter().get()(1)));
    AlwaysAssertExit(oi4.isTelescopePositionSet());
    MPosition mpos = MPosition::Convert (oi4.telescopePosition(),
                                          MPosition::WGS84)();
    MVPosition mvpos3 = mpos.getValue();
    AlwaysAssertExit (near (mvpos3.get()[0], 10., 1e-5));
    AlwaysAssertExit (near (mvpos3.getLong("deg").getValue(), -6., 1e-5));
    AlwaysAssertExit (near (mvpos3.getLat("deg").getValue(), 50., 1e-5));


// Forced errors

    {
      Record recnum, recstr;
      recnum.define ("value", 0.0);
      recstr.define ("value", "xxx");
      {
	Record rec4;
	ObsInfo oi5;
	Bool rval = True;
	rec4.defineRecord("telescop", recnum);
	try {
	  rval = oi5.fromFITS(error2, rec4);
	} catch (AipsError x) {
	  cerr << (rval==True) << endl;
	  AlwaysAssertExit(!rval);
	}
      }	
      {
        Record rec4;
	ObsInfo oi5;
        rec4.defineRecord("observer", recnum);
        AlwaysAssertExit(!oi5.fromFITS(error2, rec4));
      }
      {
	Record rec4;
	ObsInfo oi5;
	rec4.defineRecord("date-obs", recnum);
	rec4.defineRecord("timesys", recnum);
	AlwaysAssertExit(!oi5.fromFITS(error2, rec4));
      }
      {
	Record rec4;
	ObsInfo oi5;
	rec4.defineRecord("obsra", recstr);
	rec4.defineRecord("obsdec", recstr);
	AlwaysAssertExit(!oi5.fromFITS(error2, rec4));
      }
    }

// This is a fragile test, but probably better to do it than not.

    Vector<String> vs = ObsInfo::keywordNamesFITS();
    AlwaysAssertExit (vs.nelements() == 9);
    AlwaysAssertExit(vs(0) == "telescop" &&
		     vs(1) == "observer" &&
		     vs(2) == "date-obs" &&
		     vs(3) == "timesys" &&
		     vs(4) == "obsra" &&
		     vs(5) == "obsdec" &&
		     vs(6) == "obsgeo-x" &&
		     vs(7) == "obsgeo-y" &&
		     vs(8) == "obsgeo-z");

    cout << "OK" << endl;
    return 0;
}
