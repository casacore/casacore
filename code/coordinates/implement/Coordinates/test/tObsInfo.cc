//# ObsInfo.cc: Miscellaneous information related to an observation
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

#include <trial/Coordinates/ObsInfo.h>

#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/Containers/Record.h>

int main()
{
    // ObsInfo();
    ObsInfo oi;

    // String telescope() const;
    // static String defaultTelescope();
    AlwaysAssertExit(oi.telescope() == oi.defaultTelescope());
    AlwaysAssertExit(oi.defaultTelescope() == "UNKNOWN");

    // String observer() const;
    // static String defaultObserver();
    AlwaysAssertExit(oi.observer() == oi.defaultObserver());
    AlwaysAssertExit(oi.defaultObserver() == "UNKNOWN");

    // MEpoch obsDate() const;
    // static MEpoch defaultObsDate();
    MEpoch dflt = oi.defaultObsDate();
    AlwaysAssertExit(near(oi.obsDate().get("s").getValue(), 0.0));

    // ObsInfo& setTelescope(const String &telescope);
    // ObsInfo& setObserver(const String &observer);
    // ObsInfo &setObsDate(const MEpoch &obsDate);
    oi.setTelescope("telescope").setObserver("observer").
	setObsDate(MVEpoch(1234.0));
    AlwaysAssertExit(oi.telescope() == "telescope" &&
		     oi.observer() == "observer" &&
		     near(oi.obsDate().get("d").getValue(), 1234.0));

    // ObsInfo(const ObsInfo &other);
    // ObsInfo &operator=(const ObsInfo &other);
    ObsInfo oi2(oi);
    AlwaysAssertExit(oi2.telescope() == "telescope" &&
		     oi2.observer() == "observer" &&
		     near(oi2.obsDate().get("d").getValue(), 1234.0));
    oi2.setTelescope("telescope2").setObserver("observer2").
	setObsDate(MVEpoch(55000.0));
    oi = oi2;
    AlwaysAssertExit(oi.telescope() == "telescope2" &&
		     oi.observer() == "observer2" &&
		     near(oi.obsDate().get("d").getValue(), 55000.0));


    // Bool toRecord(String & error, RecordInterface & outRecord) const;
    // Bool fromRecord(String & error, const RecordInterface & inRecord);
    Record rec;
    String error;
    AlwaysAssertExit(oi.toRecord(error, rec));
    ObsInfo oi3;
    AlwaysAssertExit(oi3.fromRecord(error, rec));
    AlwaysAssertExit(oi3.telescope() == "telescope2" &&
		     oi3.observer() == "observer2" &&
		     near(oi3.obsDate().get("d").getValue(), 55000.0));

    // Bool toFITS(String & error, RecordInterface & outRecord) const;
    // Bool fromFITS(String & error, const RecordInterface & inRecord);
    Record rec2;
    AlwaysAssertExit(oi.toFITS(error, rec2));
    ObsInfo oi4;
    AlwaysAssertExit(oi4.fromFITS(error, rec2));
    AlwaysAssertExit(oi4.telescope() == "telescope2" &&
		     oi4.observer() == "observer2" &&
		     near(oi4.obsDate().get("d").getValue(), 55000.0));

    // static Vector<String> keywordNamesFITS();
    // This is a fragile test, but probably better to do it than not.
    Vector<String> vs = ObsInfo::keywordNamesFITS();
    AlwaysAssertExit(
		     vs(0) == "telescop" &&
		     vs(1) == "observer" &&
		     vs(2) == "date-obs" &&
		     vs(3) == "timesys");

    // ~ObsInfo();
    // Implicit at end of execution

    cout << "OK" << endl;
    return 0;
}
