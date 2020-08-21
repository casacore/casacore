//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
//# $Id: HostInfoDarwin.h 21521 2014-12-10 08:06:42Z gervandiepen $

#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/scimath/Mathematics/GaussianBeam.h>
#include <casacore/casa/Containers/Record.h>

#include <casacore/casa/namespace.h>


int main() {
    try {
        // null beam
        GaussianBeam null;
        AlwaysAssert(null.isNull(), AipsError);
        Quantity qzero(0, "rad");
        AlwaysAssert(null.getMajor() == qzero, AipsError);
        AlwaysAssert(null.getMinor() == qzero, AipsError);
        AlwaysAssert(null.getPA() == qzero, AipsError);
        // non null beam constructor
        Quantity majAx(4, "arcsec");
        Quantity minAx(3, "arcsec");
        Quantity pa(20, "deg");
        GaussianBeam beam(majAx, minAx, pa);
        AlwaysAssert(beam.getMajor() == majAx, AipsError);
        AlwaysAssert(beam.getMinor() == minAx, AipsError);
        AlwaysAssert(beam.getPA() == pa, AipsError);

        // copy constructor
        GaussianBeam beam2(beam);
        AlwaysAssert(beam2 == beam, AipsError);
        AlwaysAssert(beam2 != null, AipsError);

        // = operator
        beam2 = beam;
        AlwaysAssert(beam2 == beam, AipsError);
        AlwaysAssert(beam2 != null, AipsError);



        Bool except = False;
        try {
            // bogus units
            majAx = Quantity(4, "m");
            GaussianBeam beam3(majAx, minAx, pa);

        }
        catch (std::exception& x) {
            cout << "Exception thrown as expected: " << x.what() << endl;
            except = True;
        }
        AlwaysAssert(except, AipsError);
        except = False;
        try {
            // major smaller than minor
            majAx = Quantity(2, "arcsec");
            GaussianBeam beam3(majAx, minAx, pa);

        }
        catch (std::exception& x) {
            cout << "Exception thrown as expected: " << x.what() << endl;
            except = True;
        }
        AlwaysAssert(except, AipsError);

        // getArea
        majAx = Quantity(1, "arcsec");
        minAx = majAx;
        beam = GaussianBeam(majAx, minAx, pa);
        AlwaysAssert(beam.getArea("arcsec2") == Quantity(C::pi/4/C::ln2), AipsError);
        except = False;
        try {
            // bogus units
            beam.getArea("arcsec");
        }
        catch (std::exception& x) {
            cout << "Exception thrown as expected: " << x.what() << endl;
            except = True;
        }
        AlwaysAssert(except, AipsError);

        // to/from Record
        Record rec = beam.toRecord();
        beam2 = GaussianBeam::fromRecord(rec);
        AlwaysAssert(beam == beam2, AipsError);
        except = False;
        try {
            // bogus record
            rec.define("bogus", 5);
            beam2 = GaussianBeam::fromRecord(rec);

        }
        catch (std::exception& x) {
            cout << "Exception thrown as expected: " << x.what() << endl;
            except = True;
        }
        AlwaysAssert(except, AipsError);
        Vector<Quantity> v(3);
        v[0] = majAx;
        v[1] = minAx;
        v[2] = pa;
        GaussianBeam beam3(v);
        AlwaysAssert(beam3 == beam, AipsError);

        v = beam.toVector();
        GaussianBeam beam4(v);
        AlwaysAssert(beam4 == beam3, AipsError);
        {
            cout << "Test  setPA() and getPA() using unwrapping" << endl;
            Double u = 60;
            for (Double d=-660; d<=660; d+=30, u+=30) {
                if (u > 90) {
                    u -= 180;
                }
                beam.setPA(Quantity(d, "deg"), False);
                AlwaysAssert(beam.getPA(False).getValue("deg") == d, AipsError);
                AlwaysAssert(beam.getPA(True).getValue("deg") == u, AipsError);
                beam.setPA(Quantity(d, "deg"), True);
                AlwaysAssert(beam.getPA(False).getValue("deg") == u, AipsError);
            }
        }
        {
            cout << "Test NaN and Inf throw exceptions" << endl;
            static const Quantity inf(doubleInf(), "arcsec");
            static const Quantity nan(doubleNaN(), "arcsec");
            static const Quantity qok(2, "arcsec");
            try {
                GaussianBeam badBeam(inf, qok, qok);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                GaussianBeam badBeam(nan, qok, qok);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                GaussianBeam badBeam(qok, inf, qok);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                GaussianBeam badBeam(qok, nan, qok);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                GaussianBeam badBeam(qok, qok, inf);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                GaussianBeam badBeam(qok, qok, nan);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            GaussianBeam bok(qok, qok, qok);
            try {
                bok.setMajorMinor(inf, qok);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                bok.setMajorMinor(nan, qok);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                bok.setMajorMinor(qok, inf);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                bok.setMajorMinor(qok, nan);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                bok.setPA(inf);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}
            try {
                bok.setPA(nan);
                AlwaysAssert(False, AipsError);
            }
            catch (const std::exception& x) {}

        }
    }
    catch (const std::exception& x) {
        cout << x.what() << endl;
        cout << "FAIL" << endl;
        return 1;
    }
    cout << "OK" << endl;
    return 0;
}
