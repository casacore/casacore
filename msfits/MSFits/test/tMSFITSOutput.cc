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
//#

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/msfits/MSFits/MSFitsOutput.h>

using namespace casacore;

int main() {
    try {
        String *parts = new String[2];
        split(EnvironmentVariable::get("CASAPATH"), parts, 2, String(" "));
        String datadir = parts[0] + "/data/";
        delete [] parts;
        String msname = datadir + "regression/unittest/uvfits/uvfits_test.ms";
        if (! File(msname).exists()) {
            cout << "Cannot find test fixture so tests cannot be run" << endl;
            return 0;
        }
        MeasurementSet ms(msname);
        cout << "Test overwrite parameter" << endl;
        String fitsFile = "test1.ms";
        AlwaysAssert(
            MSFitsOutput::writeFitsFile(
                fitsFile, ms, "DATA", 0, 1, 1, False,
                False, False, False, 1.0, False, 1, 0, True
            ), AipsError
        );
        // this should fail since overwrite is False
        Bool thrown = False;
        try {
            MSFitsOutput::writeFitsFile(
                fitsFile, ms, "DATA", 0, 1, 1, False,
                False, False, False, 1.0, False, 1, 0, False
            );
        }
        catch (const AipsError&) {
            thrown = True;
        }
        AlwaysAssert(thrown, AipsError);
        // this should succeed, since overwrite is True
        AlwaysAssert(
            MSFitsOutput::writeFitsFile(
                fitsFile, ms, "DATA", 0, 1, 1, False,
                False, False, False, 1.0, False, 1, 0, True
            ), AipsError
        );
        // clean up
        RegularFile(fitsFile).remove();
    }
    catch (const AipsError& x) {
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
