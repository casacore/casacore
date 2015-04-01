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
#include <casacore/casa/aips.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/measures/Measures.h>
#include <casacore/measures/Measures/MeasJPL.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MeasIERS.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/fstream.h>

#include <casacore/casa/namespace.h>
int main()
{
  try {
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (int dd=0; dd<4; ++dd) {
      const MVEpoch dat = 51116 + dd*33;
      ostringstream ostr;
      ostr << dd;
      ofstream os(("tMeasJPL_tmp.out_a" + ostr.str()).c_str());
      
      MVDirection mvd1;

      os << "Test measure class MeasJPL" << endl;
      os << "---------------------------" << endl;

      os << setprecision(9);
      Vector<Double> val(6);

      os << "DE200: " << dat << endl;
      os << "---------------------------" << endl;
      os << "Mercury0:   " <<
        MeasTable::Planetary(MeasTable::MERCURY, dat.get()) << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MERCURY, dat);
      os << "Mercury:    " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::VENUS, dat);
      os << "Venus:      " << val << endl;
      for (uInt i=0; i<3; i++) mvd1(i) = val(i); mvd1.adjust();
      os << "Venus:      " << mvd1 << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::EARTH, dat);
      os << "Earth:      " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MARS, dat);
      os << "Mars:       " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::JUPITER, dat);
      os << "Jupiter:    " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::SATURN, dat);
      os << "Saturn:     " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::URANUS, dat);
      os << "Uranus:     " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::NEPTUNE, dat);
      os << "Neptune:    " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::PLUTO, dat);
      os << "Pluto:      " << val << endl;
    }
    MeasIERS::closeTables();
    const TableCache& cache = PlainTable::tableCache();
    if(cache.ntable()>0){
      cout << "ERROR: cache not empty!" << endl;
      for (uInt i=0; i<cache.ntable(); ++i) {
	cout << "    " << i << ": \"" <<  cache(i)->tableName() << "\"" << endl;
      }
    }

    // Use ifdef to avoid compiler warning.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (int dd=0; dd<4; ++dd) {
      const MVEpoch dat = 51116 + dd*33;
      ostringstream ostr;
      ostr << dd;
      ofstream os(("tMeasJPL_tmp.out_b" + ostr.str()).c_str());
      os << setprecision(9);
      Vector<Double> val(6);

      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::MOON, dat);
      os << "Moon:       " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::SUN, dat);
      os << "SUN:        " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::BARYSOLAR, dat);
      os << "Barycentre: " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::BARYEARTH, dat);
      os << "Earth/Moon: " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::NUTATION, dat);
      os << "Nutation:   " << val << endl;
      MeasJPL::get(val, MeasJPL::DE200, MeasJPL::LIBRATION, dat);
      os << "Libration:  " << val << endl;
      
      os << "DE405: " << dat << endl;
      os << "---------------------------" << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MERCURY, dat);
      os << "Mercury:    " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::VENUS, dat);
      os << "Venus:      " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::EARTH, dat);
      os << "Earth:      " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MARS, dat);
      os << "Mars:       " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::JUPITER, dat);
      os << "Jupiter:    " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::SATURN, dat);
      os << "Saturn:     " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::URANUS, dat);
      os << "Uranus:     " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::NEPTUNE, dat);
      os << "Neptune:    " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::PLUTO, dat);
      os << "Pluto:      " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::MOON, dat);
      os << "Moon:       " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::SUN, dat);
      os << "SUN:        " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::BARYSOLAR, dat);
      os << "Barycentre: " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::BARYEARTH, dat);
      os << "Earth/Moon: " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::NUTATION, dat);
      os << "Nutation:   " << val << endl;
      MeasJPL::get(val, MeasJPL::DE405, MeasJPL::LIBRATION, dat);
      os << "Libration:  " << val << endl;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
  } 
  
  try {
    const MVEpoch dat = 51116;
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

  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
  } 
  
  return 0;
}
