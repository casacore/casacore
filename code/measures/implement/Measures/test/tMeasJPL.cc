//# tMeasJPL.cc: This program test JPL DE functions
//# Copyright (C) 1997,1998
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
#include <aips/aips.h>
#include <iomanip.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Measures.h>
#include <aips/Measures/MeasJPL.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Measures/MEpoch.h>

main() {
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
      MeasTable::Planetary(MeasTable::MERCURY, dat.get()).ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MERCURY, dat);
    cout << "Mercury:    " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::VENUS, dat);
    cout << "Venus:      " << val.ac() << endl;
    for (uInt i=0; i<3; i++) mvd1(i) = val(i); mvd1.adjust();
    cout << "Venus:      " << mvd1 << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::EARTH, dat);
    cout << "Earth:      " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MARS, dat);
    cout << "Mars:       " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::JUPITER, dat);
    cout << "Jupiter:    " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::SATURN, dat);
    cout << "Saturn:     " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::URANUS, dat);
    cout << "Uranus:     " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::NEPTUNE, dat);
    cout << "Neptune:    " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::PLUTO, dat);
    cout << "Pluto:      " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MOON, dat);
    cout << "Moon:       " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::SUN, dat);
    cout << "SUN:        " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::BARYSOLAR, dat);
    cout << "Barycentre: " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::BARYEARTH, dat);
    cout << "Earth/Moon: " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::NUTATION, dat);
    cout << "Nutation:   " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE200, MeasJPL::LIBRATION, dat);
    cout << "Libration:  " << val.ac() << endl;

    cout << "DE405: " << dat << endl;
    cout << "---------------------------" << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MERCURY, dat);
    cout << "Mercury:    " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::VENUS, dat);
    cout << "Venus:      " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::EARTH, dat);
    cout << "Earth:      " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MARS, dat);
    cout << "Mars:       " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::JUPITER, dat);
    cout << "Jupiter:    " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::SATURN, dat);
    cout << "Saturn:     " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::URANUS, dat);
    cout << "Uranus:     " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::NEPTUNE, dat);
    cout << "Neptune:    " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::PLUTO, dat);
    cout << "Pluto:      " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MOON, dat);
    cout << "Moon:       " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::SUN, dat);
    cout << "SUN:        " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::BARYSOLAR, dat);
    cout << "Barycentre: " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::BARYEARTH, dat);
    cout << "Earth/Moon: " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::NUTATION, dat);
    cout << "Nutation:   " << val.ac() << endl;
    MeasJPL::get(val, MeasJPL::DE405, MeasJPL::LIBRATION, dat);
    cout << "Libration:  " << val.ac() << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;
  
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
  } end_try;
  
  exit(0);
}
