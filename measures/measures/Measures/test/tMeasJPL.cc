//# tMeasJPL.cc: This program test JPL DE functions
//# Copyright (C) 1997-2002,2007
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
//# $Id$

//# Includes
#include <casa/aips.h>
#include <casa/iomanip.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayIO.h>
#include <measures/Measures.h>
#include <measures/Measures/MeasJPL.h>
#include <casa/Quanta/MVEpoch.h>
#include <casa/Quanta/MVDirection.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MeasIERS.h>
#include <measures/Measures/MeasTable.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MEpoch.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main()
{
  const MVEpoch dat = 51116;
  try {
    MVDirection mvd1;

    cout << "Test measure class MeasJPL" << endl;
    cout << "---------------------------" << endl;

    cout << setprecision(9);
    Vector<Double> val(6);

    cout << "DE200: " << dat << endl;
    cout << "---------------------------" << endl;
    cout << "Mercury0:   " <<
      MeasTable::Planetary(MeasTable::MERCURY, dat.get()) << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MERCURY, dat);
    cout << "Mercury:    " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::VENUS, dat);
    cout << "Venus:      " << val << endl;
    for (uInt i=0; i<3; i++) mvd1(i) = val(i); mvd1.adjust();
    cout << "Venus:      " << mvd1 << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::EARTH, dat);
    cout << "Earth:      " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MARS, dat);
    cout << "Mars:       " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::JUPITER, dat);
    cout << "Jupiter:    " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::SATURN, dat);
    cout << "Saturn:     " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::URANUS, dat);
    cout << "Uranus:     " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::NEPTUNE, dat);
    cout << "Neptune:    " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::PLUTO, dat);
    cout << "Pluto:      " << val << endl;
    MeasIERS::closeTables();
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MOON, dat);
    cout << "Moon:       " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::SUN, dat);
    cout << "SUN:        " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::BARYSOLAR, dat);
    cout << "Barycentre: " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::BARYEARTH, dat);
    cout << "Earth/Moon: " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::NUTATION, dat);
    cout << "Nutation:   " << val << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::LIBRATION, dat);
    cout << "Libration:  " << val << endl;

    cout << "DE405: " << dat << endl;
    cout << "---------------------------" << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MERCURY, dat);
    cout << "Mercury:    " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::VENUS, dat);
    cout << "Venus:      " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::EARTH, dat);
    cout << "Earth:      " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MARS, dat);
    cout << "Mars:       " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::JUPITER, dat);
    cout << "Jupiter:    " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::SATURN, dat);
    cout << "Saturn:     " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::URANUS, dat);
    cout << "Uranus:     " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::NEPTUNE, dat);
    cout << "Neptune:    " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::PLUTO, dat);
    cout << "Pluto:      " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MOON, dat);
    cout << "Moon:       " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::SUN, dat);
    cout << "SUN:        " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::BARYSOLAR, dat);
    cout << "Barycentre: " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::BARYEARTH, dat);
    cout << "Earth/Moon: " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::NUTATION, dat);
    cout << "Nutation:   " << val << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::LIBRATION, dat);
    cout << "Libration:  " << val << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  try {
    const MEpoch mdat(dat, MEpoch::Ref(MEpoch::TDB));
    MeasFrame frame(mdat);
    MDirection::Ref venr(MDirection::VENUS, frame);
    MDirection::Ref sunr(MDirection::SUN, frame);
    MDirection::Ref moonr(MDirection::MOON, frame);
    MDirection ven(venr);
    MDirection sn(sunr);
    MDirection mon(moonr);
    MDirection::Convert vc1(ven, MDirection::Ref(MDirection::JNAT));
    MDirection::Convert vc2(ven, MDirection::Ref(MDirection::APP));
    MDirection::Convert sc1(sn, MDirection::Ref(MDirection::JNAT));
    MDirection::Convert sc2(sn, MDirection::Ref(MDirection::APP));
    MDirection::Convert mc1(mon, MDirection::Ref(MDirection::JNAT));
    MDirection::Convert mc2(mon, MDirection::Ref(MDirection::APP));

    cout << "Venus JNAT: " << vc1() << endl;
    cout << "Venus APP:  " << vc2() << endl;
    cout << "Sun   JNAT: " << sc1() << endl;
    cout << "Sun   APP:  " << sc2() << endl;
    cout << "Sun   APP:  " << sc2().getValue().getAngle("deg") << endl;
    cout << "Moon  JNAT: " << mc1() << endl;
    cout << "Moon  APP:  " << mc2() << endl;
    cout << "Moon  APP:  " << mc2().getValue().getAngle("deg") << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  return 0;
}
