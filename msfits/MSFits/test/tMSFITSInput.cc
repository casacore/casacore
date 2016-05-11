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
//# $Id: tMSConcat.cc 21563 2015-02-16 07:05:15Z gervandiepen $

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
        catch (const AipsError& x) {
            thrown = True;
        }
        AlwaysAssert(thrown, AipsError);
        {
            cout << "test rotating antenna positions" << endl;
            fitsfile = datadir + "regression/unittest/uvfits/casa_4.6_vla.uvfits";
            String msfile = "rotated_positions.ms";
            removeIfNecessary(msfile);
            SHARED_PTR<MSFitsInput> reader(new MSFitsInput(msfile, fitsfile));
            reader->rotateAntennaPositions(True);
            reader->readFitsFile();
            MeasurementSet ms(msfile);
            SHARED_PTR<MSMetaData> md(new MSMetaData(&ms, 50));
            vector<MPosition> pos = md->getAntennaPositions(vector<uInt>(1, 0));
            Vector<Double> expec(3);
            expec[0] = -1601145.65187839;
            expec[1] = -5041988.31653595;
            expec[2] = 3554878.93106461;
            AlwaysAssert(allNearAbs(pos[0].getValue().getValue(), expec, 1e-6), AipsError);

            msfile = "nonrotated_positions.ms";
            removeIfNecessary(msfile);
            reader.reset(new MSFitsInput(msfile, fitsfile));
            reader->readFitsFile();
            ms = MeasurementSet(msfile);
            md.reset(new MSMetaData(&ms, 50));
            pos = md->getAntennaPositions(vector<uInt>(1, 0));
            AlwaysAssert(! allNearAbs(pos[0].getValue().getValue(), expec, 1e-6), AipsError);

            fitsfile = datadir + "regression/unittest/uvfits/casa_4.7_vla.uvfits";
            msfile = "4_7.ms";
            removeIfNecessary(msfile);
            reader.reset(new MSFitsInput(msfile, fitsfile));
            // This should have no effect on processing now, since in this uvfits
            // file, the array position is at the ITRF origin
            reader->rotateAntennaPositions(True);
            reader->readFitsFile();
            ms = MeasurementSet(msfile);
            md.reset(new MSMetaData(&ms, 50));
            pos = md->getAntennaPositions(vector<uInt>(1, 0));
            AlwaysAssert(allNearAbs(pos[0].getValue().getValue(), expec, 1e-6), AipsError);

            // and just check to make the default value of rotating antenna positions
            // works as expected
            removeIfNecessary(msfile);
            reader.reset(new MSFitsInput(msfile, fitsfile));
            reader->readFitsFile();
            ms = MeasurementSet(msfile);
            md.reset(new MSMetaData(&ms, 50));
            pos = md->getAntennaPositions(vector<uInt>(1, 0));
            AlwaysAssert(allNearAbs(pos[0].getValue().getValue(), expec, 1e-6), AipsError);

        }
    }
    catch (const AipsError& x) {
        cerr << x.getMesg() << endl;
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
