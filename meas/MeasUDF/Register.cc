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

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/meas/MeasUDF/PositionUDF.h>
#include <casacore/meas/MeasUDF/EpochUDF.h>
#include <casacore/meas/MeasUDF/DirectionUDF.h>
#include <casacore/meas/MeasUDF/EarthMagneticUDF.h>
#include <casacore/meas/MeasUDF/FrequencyUDF.h>
#include <casacore/meas/MeasUDF/RadialVelocityUDF.h>
#include <casacore/meas/MeasUDF/DopplerUDF.h>
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
  UDFBase::registerUDF ("meas.ITRFLLH",       PositionUDF::makeITRFLLH);
  UDFBase::registerUDF ("meas.ITRFLL",        PositionUDF::makeITRFLL);
  UDFBase::registerUDF ("meas.ITRFLONLAT",    PositionUDF::makeITRFLL);
  UDFBase::registerUDF ("meas.ITRFH",         PositionUDF::makeITRFH);
  UDFBase::registerUDF ("meas.ITRFHEIGHT",    PositionUDF::makeITRFH);
  UDFBase::registerUDF ("meas.WGS",           PositionUDF::makeWGSXYZ);
  UDFBase::registerUDF ("meas.WGSXYZ",        PositionUDF::makeWGSXYZ);
  UDFBase::registerUDF ("meas.WGSLLH",        PositionUDF::makeWGSLLH);
  UDFBase::registerUDF ("meas.WGSLL",         PositionUDF::makeWGSLL);
  UDFBase::registerUDF ("meas.WGSLONLAT",     PositionUDF::makeWGSLL);
  UDFBase::registerUDF ("meas.WGSH",          PositionUDF::makeWGSH);
  UDFBase::registerUDF ("meas.WGSHEIGHT",     PositionUDF::makeWGSH);
  UDFBase::registerUDF ("meas.EPOCH",         EpochUDF::makeEPOCH);
  UDFBase::registerUDF ("meas.LAST",          EpochUDF::makeLAST);
  UDFBase::registerUDF ("meas.LST",           EpochUDF::makeLAST);
  UDFBase::registerUDF ("meas.DIR",           DirectionUDF::makeDIR);
  UDFBase::registerUDF ("meas.DIRECTION",     DirectionUDF::makeDIR);
  UDFBase::registerUDF ("meas.DIRCOS",        DirectionUDF::makeDIRCOS);
  UDFBase::registerUDF ("meas.DIRECTIONCOSINE", DirectionUDF::makeDIRCOS);
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
  UDFBase::registerUDF ("meas.EM",            EarthMagneticUDF::makeEMXYZ);
  UDFBase::registerUDF ("meas.EARTHMAGNETIC", EarthMagneticUDF::makeEMXYZ);
  UDFBase::registerUDF ("meas.EMXYZ",         EarthMagneticUDF::makeEMXYZ);
  UDFBase::registerUDF ("meas.EMANG",         EarthMagneticUDF::makeEMANG);
  UDFBase::registerUDF ("meas.EMANGLES",      EarthMagneticUDF::makeEMANG);
  UDFBase::registerUDF ("meas.EMLEN",         EarthMagneticUDF::makeEMLEN);
  UDFBase::registerUDF ("meas.EMLENGTH",      EarthMagneticUDF::makeEMLEN);
  UDFBase::registerUDF ("meas.IGRF",          EarthMagneticUDF::makeIGRFXYZ);
  UDFBase::registerUDF ("meas.IGRFXYZ",       EarthMagneticUDF::makeIGRFXYZ);
  UDFBase::registerUDF ("meas.IGRFANG",       EarthMagneticUDF::makeIGRFANG);
  UDFBase::registerUDF ("meas.IGRFANGLES",    EarthMagneticUDF::makeIGRFANG);
  UDFBase::registerUDF ("meas.IGRFLEN",       EarthMagneticUDF::makeIGRFLEN);
  UDFBase::registerUDF ("meas.IGRFLENGTH",    EarthMagneticUDF::makeIGRFLEN);
  UDFBase::registerUDF ("meas.IGRFLOS",       EarthMagneticUDF::makeIGRFLOS);
  UDFBase::registerUDF ("meas.IGRFLONG",      EarthMagneticUDF::makeIGRFLONG);
  UDFBase::registerUDF ("meas.FREQ",          FrequencyUDF::makeFREQ);
  UDFBase::registerUDF ("meas.FREQUENCY",     FrequencyUDF::makeFREQ);
  UDFBase::registerUDF ("meas.REST",          FrequencyUDF::makeREST);
  UDFBase::registerUDF ("meas.RESTFREQ",      FrequencyUDF::makeREST);
  UDFBase::registerUDF ("meas.RESTFREQUENCY", FrequencyUDF::makeREST);
  UDFBase::registerUDF ("meas.SHIFT",         FrequencyUDF::makeSHIFT);
  UDFBase::registerUDF ("meas.SHIFTFREQ",     FrequencyUDF::makeSHIFT);
  UDFBase::registerUDF ("meas.SHIFTFREQUENCY",FrequencyUDF::makeSHIFT);
  UDFBase::registerUDF ("meas.RV",            RadialVelocityUDF::makeRADVEL);
  UDFBase::registerUDF ("meas.RADVEL",        RadialVelocityUDF::makeRADVEL);
  UDFBase::registerUDF ("meas.RADIALVELOCITY",RadialVelocityUDF::makeRADVEL);
  UDFBase::registerUDF ("meas.DOPPLER",       DopplerUDF::makeDOPPLER);
  UDFBase::registerUDF ("meas.REDSHIFT",      DopplerUDF::makeDOPPLER);
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
    os << "  MEAS.DIRCOS (type, direction [,epoch, position])" << endl;
    os << "       as DIR returning 3 direction cosines instead of 2 angles" << endl;
    os << "       DIRECTIONCOSINE is a synonym for DIRCOS" << endl;
    if (showTypes) {
      os << endl << "Known source directions (names are case-insenstive):" << endl;
      os << "  All sources in the Measures Sources table" << endl;
      os << "  SUN   MOON  MERCURY  VENUS  MARS  JUPITER  SATURN  URANUS  NEPTUNE  PLUTO" << endl;
      os << "  CasA  CygA  HerA     HydA   PerA  TauA     VirA" << endl;
      os << " In function RISET type SUN can have a suffix -XX where XX can be (default -UR):" << endl;
      os << "   C    center touches horizon             CR  center with refraction" << endl;
      os << "   U    upper edge touches horizon         UR  upper edge with refraction" << endl;
      os << "   L    lower edge touches horizon         LR  lower edge with refraction" << endl;
      os << "   CT   civil twilight darkness (-6 deg)   NT  nautical twilight darkness (-12)" << endl;
      os << "   AT   amateur astronomy twilight (-15)   ST  scientific astronomy twilight (-18)" << endl;
      os << " The first 6 suffices can also be used with MOON." << endl;
      os << endl;
      os << TaQLShow::showMeasTypes ("direction");
    }
  }

  void HelpMeasUDF::showFuncsEarthMagnetic (ostream& os, Bool showTypes)
  {
    os << "EarthMagnetic conversion functions:" << endl;
    os << "  MEAS.EM (type, em, epoch, position)   convert em value to given type as xyz" << endl;
    os << "       EARTHMAGNETIC and EMXYZ are synonyms for EM" << endl;
    os << "  MEAS.EMANG (type, em, epoch, position)    convert and return as angles" << endl;
    os << "       EMANGLES is a synonym for EMANG" << endl;
    os << "  MEAS.EMLEN (type, em, epoch, position)    convert and return as flux density" << endl;
    os << "       EMLENGTH is a synonym for EMLEN" << endl;
    os << "  MEAS.IGRF (type, height, direction, epoch, position)    IGRF model value" << endl;
    os << "       IGRFXYZ is a synonym for IGRF" << endl;
    os << "  MEAS.IGRFANG (t, h, d, e, p)              IGRF model angles in ITRF" << endl;
    os << "       IGRFANGLES is a synonym for IGRFANG" << endl;
    os << "  MEAS.IGRFLEN (t, h, d, e, p)              IGRF model flux density " << endl;
    os << "       IGRFLENGTH is a synonym for IGRFLEN" << endl;
    os << "  MEAS.IGRFLOS (h, d, e, p)                 IGRF value along line-of-sight" << endl;
    os << "  MEAS.IGRFLONG (h, d, e, p)                longitude of calculation point" << endl;
    if (showTypes) {
      os << endl;
      os << TaQLShow::showMeasTypes ("earthmagnetic");
    }
  }

  void HelpMeasUDF::showFuncsFrequency (ostream& os, Bool showTypes)
  {
    os << "Frequency conversion functions:" << endl;
    os << "  MEAS.FREQ (type, freq, radvel, direction, epoch, position)   convert to given type" << endl;
    os << "           Instead of freq, a period or wavelength can be given (requires a unit)" << endl;
    os << "           radvel is only needed when converting to/from rest frequencies" << endl;
    os << "       FREQUENCY is a synonym for FREQ" << endl;
    os << "  MEAS.REST (freq, radvel, direction, epoch, position)         convert to rest freq" << endl;
    os << "  MEAS.REST (freq, doppler)                                    convert to rest freq" << endl;
    os << "       RESTFREQ and RESTFREQUENCY are synonyms for REST" << endl;
    os << "  MEAS.SHIFTFREQ (freq, doppler)                               shift frequencies" << endl;
    os << "       SHIFT and SHIFTFREQUENCY are synonyms for SHIFTFREQ" << endl;
    os << "       It can also be used to shift rest frequencies" << endl;
    if (showTypes) {
      os << endl;
      os << TaQLShow::showMeasTypes ("frequency");
    }
  }

  void HelpMeasUDF::showFuncsRadialVelocity (ostream& os, Bool showTypes)
  {
    os << "RadialVelocity conversion functions:" << endl;
    os << "  MEAS.RADVEL (type, radvel, direction, epoch, position)    convert to given type" << endl;
    os << "  MEAS.RADVEL (type, doppler)                               calc from doppler" << endl;
    os << "       RV and RADIALVELOCITY are synonyms for RADVEL" << endl;
    if (showTypes) {
      os << endl;
      os << TaQLShow::showMeasTypes ("radialvelocity");
    }
  }

  void HelpMeasUDF::showFuncsDoppler (ostream& os, Bool showTypes)
  {
    os << "Doppler conversion functions:" << endl;
    os << "  MEAS.DOPPLER (type, doppler)               convert to given type" << endl;
    os << "  MEAS.DOPPLER (type, radvel)                calc from radial velocity" << endl;
    os << "  MEAS.DOPPLER (type, freq, restfreq)        calc from frequency" << endl;
    os << "       REDSHIFT is a synonym for DOPPLER" << endl;
    if (showTypes) {
      os << endl;
      os << TaQLShow::showMeasTypes ("doppler");
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
      os << endl;
      showFuncsEarthMagnetic (os, False);
      os << endl;
      showFuncsFrequency (os, False);
      os << endl;
      showFuncsRadialVelocity (os, False);
      os << endl;
      showFuncsDoppler (os, False);
    } else if (type == "position"  ||  type == "pos") {
      showFuncsPosition (os, True);
    } else if (type == "epoch") {
      showFuncsEpoch (os, True);
    } else if (type == "direction"  ||  type == "dir") {
      showFuncsDirection (os, True);
    } else if (type == "earthmagnetic"  ||  type == "em") {
      showFuncsEarthMagnetic (os, True);
    } else if (type == "frequency"  ||  type == "freq") {
      showFuncsFrequency (os, True);
    } else if (type == "radialvelocity"  ||  type == "radvel"  ||  type == "rv") {
      showFuncsRadialVelocity (os, True);
    } else if (type == "doppler") {
      showFuncsDoppler (os, True);
    }
    if (os.str().empty()) {
      os << type
         << " is an unknown meas subtype; use pos(ition), epoch, dir(ection),"
         << " earthmagnetic (em), freq(uency) or radialvelocity (radvel)"
         << endl;
    } else {
      os << endl << "See also section 'Special Measures functions'"
        " at http://casacore.github.io/casacore-notes/199.html"
         << endl;
    }
    return os.str();
  }

}  // end namespace
