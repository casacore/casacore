//# Copyright (C) 1995,1996,1997,1999,2001,2002,2005
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/msfits/MSFits/MSFitsInput.h>
#include <casacore/ms/MSOper/MSMetaData.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

using namespace casacore;

void removeIfNecessary(const String& msname) {
    Directory d(msname);
    if (d.exists()) {
        d.removeRecursive();
    }
}

int main() {
    try {
        String *parts = new String[2];
        split(EnvironmentVariable::get("CASAPATH"), parts, 2, String(" "));
        String datadir = parts[0] + "/data/";
        delete [] parts;
        String fitsfile = datadir + "regression/unittest/uvfits/1331+305_I.UVFITS";
        if (! File(fitsfile).exists()) {
            cout << "Cannot find test fixture so tests cannot be run" << endl;
            return 0;
        }
        String msfile = "myoutms.ms";
        removeIfNecessary(msfile);
        MSFitsInput msfitsin(msfile, fitsfile);
        Bool thrown = False;
        try {
            msfitsin.readFitsFile();
        }
        catch (const std::exception& x) {
            thrown = True;
        }
        AlwaysAssert(thrown, AipsError);
    }
    catch (const std::exception& x) {
        cerr << x.what() << endl;
        cout << "FAIL" << endl;
        return 1;
    }
    catch (...) {
        cerr << "Exception not derived from AipsError" << endl;
        cout << "FAIL" << endl;
        return 2;
    }
    cout << "OK" << endl;
    return 0;
}
