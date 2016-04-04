//# Register.cc: Register Measure UDFs
//# Copyright (C) 2011
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
//# $Id$

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/meas/MeasUDF/PositionUDF.h>
#include <casacore/meas/MeasUDF/EpochUDF.h>
#include <casacore/meas/MeasUDF/DirectionUDF.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/tables/TaQL/TaQLShow.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <ostream>
#include <sstream>
#include <iomanip>

using namespace casacore;
using namespace std;

void register_meas()
{
  // Register the TaQL Meas UDFs.
  // All of them should be shown in the showFuncs functions below.
  UDFBase::registerUDF ("meas.HELP",          HelpMeasUDF::makeHELP);
  UDFBase::registerUDF ("meas.POS",           PositionUDF::makePOS);
  UDFBase::registerUDF ("meas.POSITION",      PositionUDF::makePOS);
  UDFBase::registerUDF ("meas.ITRFXYZ",       PositionUDF::makeITRFXYZ);
  UDFBase::registerUDF ("meas.ITRFLL",        PositionUDF::makeITRFLL);
  UDFBase::registerUDF ("meas.ITRFLONLAT",    PositionUDF::makeITRFLL);
  UDFBase::registerUDF ("meas.ITRFH",         PositionUDF::makeITRFH);
  UDFBase::registerUDF ("meas.ITRFHEIGHT",    PositionUDF::makeITRFH);
  UDFBase::registerUDF ("meas.WGS",           PositionUDF::makeWGSXYZ);
  UDFBase::registerUDF ("meas.WGSXYZ",        PositionUDF::makeWGSXYZ);
  UDFBase::registerUDF ("meas.WGSLL",         PositionUDF::makeWGSLL);
  UDFBase::registerUDF ("meas.WGSLONLAT",     PositionUDF::makeWGSLL);
  UDFBase::registerUDF ("meas.WGSH",          PositionUDF::makeWGSH);
  UDFBase::registerUDF ("meas.WGSHEIGHT",     PositionUDF::makeWGSH);
  UDFBase::registerUDF ("meas.EPOCH",         EpochUDF::makeEPOCH);
  UDFBase::registerUDF ("meas.LAST",          EpochUDF::makeLAST);
  UDFBase::registerUDF ("meas.LST",           EpochUDF::makeLAST);
  UDFBase::registerUDF ("meas.DIR",           DirectionUDF::makeDIR);
  UDFBase::registerUDF ("meas.DIRECTION",     DirectionUDF::makeDIR);
  UDFBase::registerUDF ("meas.HADEC",         DirectionUDF::makeHADEC);
  UDFBase::registerUDF ("meas.AZEL",          DirectionUDF::makeAZEL);
  UDFBase::registerUDF ("meas.APP",           DirectionUDF::makeAPP);
  UDFBase::registerUDF ("meas.APPARENT",      DirectionUDF::makeAPP);
  UDFBase::registerUDF ("meas.J2000",         DirectionUDF::makeJ2000);
  UDFBase::registerUDF ("meas.B1950",         DirectionUDF::makeB1950);
  UDFBase::registerUDF ("meas.ECL",           DirectionUDF::makeECL);
  UDFBase::registerUDF ("meas.ECLIPTIC",      DirectionUDF::makeECL);
  UDFBase::registerUDF ("meas.GAL",           DirectionUDF::makeGAL);
  UDFBase::registerUDF ("meas.GALACTIC",      DirectionUDF::makeGAL);
  UDFBase::registerUDF ("meas.SGAL",          DirectionUDF::makeSGAL);
  UDFBase::registerUDF ("meas.SUPERGAL",      DirectionUDF::makeSGAL);
  UDFBase::registerUDF ("meas.SUPERGALACTIC", DirectionUDF::makeSGAL);
  UDFBase::registerUDF ("meas.ITRFD",         DirectionUDF::makeITRF);
  UDFBase::registerUDF ("meas.ITRFDIR",       DirectionUDF::makeITRF);
  UDFBase::registerUDF ("meas.ITRFDIRECTION", DirectionUDF::makeITRF);
  UDFBase::registerUDF ("meas.RISET",         DirectionUDF::makeRISESET);
  UDFBase::registerUDF ("meas.RISESET",       DirectionUDF::makeRISESET);
}


namespace casacore {

  void HelpMeasUDF::showFuncsEpoch (ostream& os, Bool showTypes)
  {
    os << "Epoch conversion functions:" << endl;
    os << "  MEAS.EPOCH (type, epoch [,position])           convert to given type" << endl;
    os << "  MEAS.LAST (epoch, position)                    convert to local sidereal time" << endl;
    os << "       LST is a synonym for LAST" << endl;
    if (showTypes) {
      os << endl;
      os << TaQLShow::showMeasTypes ("epoch");
    }
  }

  void HelpMeasUDF::showFuncsPosition (ostream& os, Bool showTypes)
  {
    os << "Position conversion functions:" << endl;
    os << "  MEAS.POS (type, position)                      convert to given type" << endl;
    os << "       POSITION is a synonym for POS" << endl;
    os << "  MEAS.ITRFXYZ (position)                        convert to ITRF XYZ coord" << endl;
    os << "  MEAS.ITRFLL (position)                         convert to ITRF LonLat" << endl;
    os << "       ITRFLONLAT is a synonym for ITRFLL" << endl;
    os << "  MEAS.ITRFH (position)                          convert to ITRF height" << endl;
    os << "       ITRFHEIGHT is a synonym for ITRFH" << endl;
    os << "  MEAS.WGS (position)                            convert to WGS84 XYZ coord" << endl;
    os << "       WGSXYZ is a synonym for WGS" << endl;
    os << "  MEAS.WGSLL (position)                          convert to WGS84 LonLat" << endl;
    os << "       WGSLONLAT is a synonym for WGSLL" << endl;
    os << "  MEAS.WGSH (position)                           convert to WGS84 height" << endl;
    os << "       WGSHEIGHT is a synonym for WGSH" << endl;
    if (showTypes) {
      os << endl << "Known observatory positions (names are case-insenstive):" << endl;
      Vector<String> obs = MeasTable::Observatories().copy();
      genSort (obs);
      uInt maxLen = 0;
      for (uInt i=0; i<obs.size(); ++i) {
        if (obs[i].size() > maxLen) maxLen = obs[i].size();
      }
      uInt npl = 80 / (maxLen+1);
      uInt n = 0;
      for (uInt i=0; i<obs.size(); ++i) {
        os << setw(maxLen+1) << obs[i];
        if (++n == npl) {
          os << endl;
          n = 0;
        }
      }
      if (n > 0) os << endl;
      os << endl;
      os << TaQLShow::showMeasTypes ("position");
    }
  }

  void HelpMeasUDF::showFuncsDirection (ostream& os, Bool showTypes)
  {
    os << "Direction conversion functions:" << endl;
    os << "  MEAS.DIR (type, direction [,epoch, position])  convert to given type" << endl;
    os << "       DIRECTION is a synonym for DIR" << endl;
    os << "  MEAS.HADEC (direction, epoch, position)        convert to Hourangle/Decl" << endl;
    os << "  MEAS.AZEL (direction, epoch, position)         convert to Azimuth/Elevation" << endl;
    os << "  MEAS.APP (direction, epoch, position)          convert to apparent" << endl;
    os << "       APPARENT is a synonym for APP" << endl;
    os << "  MEAS.J2000 (direction [,epoch, position])      convert to J2000" << endl;
    os << "  MEAS.B1950 (direction [,epoch, position])      convert to B1950" << endl;
    os << "  MEAS.ECL (direction [,epoch, position])" << endl;
    os << "       ECLIPTIC is a synonym for ECL" << endl;
    os << "  MEAS.GAL (direction [,epoch, position])" << endl;
    os << "       GALACTIC is a synonym for GAL" << endl;
    os << "  MEAS.SGAL (direction [,epoch, position])" << endl;
    os << "       SUPERGAL is a synonym for SGAL" << endl;
    os << "       SUPERGALACTIC is a synonym for SGAL" << endl;
    os << "  MEAS.ITRFD (direction [,epoch, position])      convert to ITRF" << endl;
    os << "       ITRFDIR is a synonym for ITRFD" << endl;
    os << "       ITRFDIRECTION is a synonym for ITRFD" << endl;
    os << "  MEAS.RISET (direction, epoch, position)        get rise/set time" << endl;
    os << "       RISESET is a synonym for RISET" << endl;
    if (showTypes) {
      os << endl << "Known source directions (names are case-insenstive):" << endl;
      os << "  All sources in the Measures Sources table" << endl;
      os << "  SUN   MOON  MERCURY  VENUS  MARS  JUPITER  SATURN  URANUS  NEPTUNE  PLUTO" << endl;
      os << "  CasA  CygA  HerA     HydA   PerA  TauA     VirA" << endl;
      os << endl;
      os << TaQLShow::showMeasTypes ("direction");
    }
  }


  UDFBase* HelpMeasUDF::makeHELP (const String&)
    { return new HelpMeasUDF(); }

  void HelpMeasUDF::setup (const Table&, const TaQLStyle&)
  {
    AlwaysAssert (operands().size() <= 1, AipsError);
    if (operands().size() == 1) {
      AlwaysAssert (operands()[0]->dataType()  == TableExprNodeRep::NTString  &&
                    operands()[0]->valueType() == TableExprNodeRep::VTScalar,
                    AipsError);
    }
    // Set datatype, shape, unit, etc.
    setDataType (TableExprNodeRep::NTString);
    setNDim (0);                  // scalar
    setConstant (True);
  }

  String HelpMeasUDF::getString (const TableExprId& id)
  {
    ostringstream os;
    String type;
    if (operands().size() == 1) {
      type = operands()[0]->getString(id);
      type.downcase();
    }
    if (type.empty()) {
      showFuncsPosition (os, False);
      os << endl;
      showFuncsEpoch (os, False);
      os << endl;
      showFuncsDirection (os, False);
    } else if (type == "position"  ||  type == "pos") {
      showFuncsPosition (os, True);
    } else if (type == "epoch") {
      showFuncsEpoch (os, True);
    } else if (type == "direction"  ||  type == "dir") {
      showFuncsDirection (os, True);
    }
    if (os.str().empty()) {
      os << type
         << " is an unknown meas subtype; use pos(ition), epoch or dir(ection)"
         << endl;
    } else {
      os << endl << "See also section 'Special Measures functions'"
        " at http://casacore.github.io/casacore-notes/199.html"
         << endl;
    }
    return os.str();
  }

}  // end namespace
