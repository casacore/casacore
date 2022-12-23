//# Register.cc: Register virtual column engine to return derived MS values
//# Copyright (C) 2010
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

#include <casacore/derivedmscal/DerivedMC/Register.h>
#include <casacore/derivedmscal/DerivedMC/DerivedMSCal.h>
#include <casacore/derivedmscal/DerivedMC/UDFMSCal.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <ostream>
#include <sstream>

using namespace casacore;
using namespace std;

void register_derivedmscal()
{
  // Register the table virtual column engine.
  DerivedMSCal::registerClass();

  // Register the TaQL UDFs.
  // Derived quantities.
  UDFBase::registerUDF ("derivedmscal.HELP",      HelpMsCalUDF::makeHELP);
  UDFBase::registerUDF ("derivedmscal.HA",        UDFMSCal::makeHA);
  UDFBase::registerUDF ("derivedmscal.HA1",       UDFMSCal::makeHA1);
  UDFBase::registerUDF ("derivedmscal.HA2",       UDFMSCal::makeHA2);
  UDFBase::registerUDF ("derivedmscal.HADEC",     UDFMSCal::makeHADEC);
  UDFBase::registerUDF ("derivedmscal.HADEC1",    UDFMSCal::makeHADEC1);
  UDFBase::registerUDF ("derivedmscal.HADEC2",    UDFMSCal::makeHADEC2);
  UDFBase::registerUDF ("derivedmscal.PA1",       UDFMSCal::makePA1);
  UDFBase::registerUDF ("derivedmscal.PA2",       UDFMSCal::makePA2);
  UDFBase::registerUDF ("derivedmscal.LAST",      UDFMSCal::makeLAST);
  UDFBase::registerUDF ("derivedmscal.LAST1",     UDFMSCal::makeLAST1);
  UDFBase::registerUDF ("derivedmscal.LAST2",     UDFMSCal::makeLAST2);
  UDFBase::registerUDF ("derivedmscal.AZEL",      UDFMSCal::makeAZEL);
  UDFBase::registerUDF ("derivedmscal.AZEL1",     UDFMSCal::makeAZEL1);
  UDFBase::registerUDF ("derivedmscal.AZEL2",     UDFMSCal::makeAZEL2);
  UDFBase::registerUDF ("derivedmscal.ITRF",      UDFMSCal::makeITRF);
  UDFBase::registerUDF ("derivedmscal.UVWWVL",    UDFMSCal::makeUvwWvl);
  UDFBase::registerUDF ("derivedmscal.UVWWVLS",   UDFMSCal::makeUvwWvls);
  UDFBase::registerUDF ("derivedmscal.UVWJ2000",  UDFMSCal::makeUvwJ2000);
  UDFBase::registerUDF ("derivedmscal.UVWJ2000WVL", UDFMSCal::makeWvlJ2000);
  UDFBase::registerUDF ("derivedmscal.UVWJ2000WVLS",UDFMSCal::makeWvlsJ2000);
  UDFBase::registerUDF ("derivedmscal.UVWAPP",    UDFMSCal::makeUvwAPP);
  UDFBase::registerUDF ("derivedmscal.UVWAPPWVL", UDFMSCal::makeWvlAPP);
  UDFBase::registerUDF ("derivedmscal.UVWAPPWVLS",UDFMSCal::makeWvlsAPP);
  UDFBase::registerUDF ("derivedmscal.DELAY1",    UDFMSCal::makeDelay1);
  UDFBase::registerUDF ("derivedmscal.DELAY2",    UDFMSCal::makeDelay2);
  UDFBase::registerUDF ("derivedmscal.DELAY",     UDFMSCal::makeDelay);
  UDFBase::registerUDF ("derivedmscal.STOKES",    UDFMSCal::makeStokes);
  // CASA selection.
  UDFBase::registerUDF ("derivedmscal.BASELINE",  UDFMSCal::makeBaseline);
  UDFBase::registerUDF ("derivedmscal.TIME",      UDFMSCal::makeTime);
  UDFBase::registerUDF ("derivedmscal.SPW",       UDFMSCal::makeSpw);
  UDFBase::registerUDF ("derivedmscal.UVDIST",    UDFMSCal::makeUVDist);
  UDFBase::registerUDF ("derivedmscal.FIELD",     UDFMSCal::makeField);
  UDFBase::registerUDF ("derivedmscal.ARRAY",     UDFMSCal::makeArray);
  UDFBase::registerUDF ("derivedmscal.SCAN",      UDFMSCal::makeScan);
  UDFBase::registerUDF ("derivedmscal.STATE",     UDFMSCal::makeState);
  UDFBase::registerUDF ("derivedmscal.OBS",       UDFMSCal::makeObs);
  // Data from subtables.
  UDFBase::registerUDF ("derivedmscal.ANT1NAME",  UDFMSCal::makeAnt1Name);
  UDFBase::registerUDF ("derivedmscal.ANT2NAME",  UDFMSCal::makeAnt2Name);
  UDFBase::registerUDF ("derivedmscal.ANT1COL",   UDFMSCal::makeAnt1Col);
  UDFBase::registerUDF ("derivedmscal.ANT2COL",   UDFMSCal::makeAnt2Col);
  UDFBase::registerUDF ("derivedmscal.STATECOL",  UDFMSCal::makeStateCol);
  UDFBase::registerUDF ("derivedmscal.OBSCOL",    UDFMSCal::makeObsCol);
  UDFBase::registerUDF ("derivedmscal.SPWCOL",    UDFMSCal::makeSpwCol);
  UDFBase::registerUDF ("derivedmscal.POLCOL",    UDFMSCal::makePolCol);
  UDFBase::registerUDF ("derivedmscal.FIELDCOL",  UDFMSCal::makeFieldCol);
  UDFBase::registerUDF ("derivedmscal.PROCCOL",   UDFMSCal::makeProcCol);
  UDFBase::registerUDF ("derivedmscal.SUBCOL",    UDFMSCal::makeSubCol);
}


namespace casacore {

  void HelpMsCalUDF::showFuncsDerived (ostream& os)
  {
    os << "Derived direction coordinates functions" << endl;
    os << " A direction parameter can be given to the functions." << endl;
    os << " It can be the name of a FIELD subtable column (e.g., 'DELAY_DIR')," << endl;
    os << " the name of a source (e.g., 'SUN'), or a RA,DEC pair defining" << endl;
    os << " the J2000 source direction (e.g., [10h42m31.3, 45d51m16])." << endl;
    os << " If no direction argument is given, column DELAY_DIR is used for the" << endl;
    os << " delay functions, otherwise column PHASE_DIR." << endl;
    os << "  double MSCAL.HA()             "
      " hourangle of array center" << endl;
    os << "  double MSCAL.HA1()            "
      " hourangle of ANTENNA1" << endl;
    os << "  double MSCAL.HA2()            "
      " hourangle of ANTENNA2" << endl;
    os << "  doublearray MSCAL.HADEC()     "
      " hourangle/declination of array center" << endl;
    os << "  doublearray MSCAL.HADEC1()    "
      " hourangle/declination of ANTENNA1" << endl;
    os << "  doublearray MSCAL.HADEC2()    "
      " hourangle/declination of ANTENNA2" << endl;
    os << "  doublearray MSCAL.AZEL()      "
      " azimuth/elevation of array center" << endl;
    os << "  doublearray MSCAL.AZEL1()     "
      " azimuth/elevation of ANTENNA1" << endl;
    os << "  doublearray MSCAL.AZEL2()     "
      " azimuth/elevation of ANTENNA2" << endl;
    os << "  doublearray MSCAL.ITRF()      "
      " direction in ITRF coordinates" << endl;
    os << "  double MSCAL.LAST()           "
      " local sidereal time of array center" << endl;
    os << "  double MSCAL.LAST1()          "
      " local sidereal time of ANTENNA1" << endl;
    os << "  double MSCAL.LAST2()          "
      " local sidereal time of ANTENNA2" << endl;
    os << "  double MSCAL.PA1()            "
      " parallactic angle of ANTENNA1" << endl;
    os << "  double MSCAL.PA2()            "
      " parallactic angle of ANTENNA2" << endl;
    os << "  doublearray MSCAL.UVWWVL()    "
      " stored UVW coordinates in wvl for reffreq" << endl;
    os << "  doublearray MSCAL.UVWWVLS()   "
      " stored UVW coordinates in wvl per channel" << endl;
    os << "  doublearray MSCAL.UVWJ2000()    "
      " calc J2000 UVW coordinates in meters" << endl;
    os << "  doublearray MSCAL.UVWJ2000WVL() "
      " calc J2000 UVW coordinates in wvl for reffreq" << endl;
    os << "  doublearray MSCAL.UVWJ2000WVLS()"
      " calc J2000 UVW coordinates in wvl per channel" << endl;
    os << "  doublearray MSCAL.UVWAPP()    "
      " calc Apparent UVW coordinates in meters" << endl;
    os << "  doublearray MSCAL.UVWAPPWVL() "
      " calc Apparent UVW coordinates in wvl for reffreq" << endl;
    os << "  doublearray MSCAL.UVWAPPWVLS()"
      " calc Apparent UVW coordinates in wvl per channel" << endl;
    os << "  double MSCAL.DELAY1()  "
      " calc delay (seconds) of ANTENNA1 w.r.t. array center" << endl;
    os << "  double MSCAL.DELAY2()  "
      " calc delay (seconds) of ANTENNA2 w.r.t. array center" << endl;
    os << "  double MSCAL.DELAY1()  "
      " calc delay (seconds) of ANTENNA1 w.r.t. ANTENNA2" << endl;
  }

  void HelpMsCalUDF::showFuncsStokes (ostream& os, Bool showStokes)
  {
    os << "Stokes conversion functions:" << endl;
    os << "  complexarray MSCAL.STOKES(complexarray, string)  "
      " convert the data" << endl;
    os << "  doublearray  MSCAL.STOKES(doublearray,  string)  "
      " combine the weights" << endl;
    os << "  boolarray    MSCAL.STOKES(boolarray,    string)  "
      " combine the flags" << endl;
    if (showStokes) {
      os << endl;
      os << "The case-insensitive string argument defines the output Stokes axes." << endl;
      os << "It must be a comma separated list of Stokes names. All values" << endl;
      os<< "defined in the Casacore class Stokes are possible. Most important are:" << endl;
      os << "   XX, XY, YX, and/or YY.  LINEAR or LIN means XX,XY,YX,YY." << endl;
      os << "   RR, RL, LR, and/or LL.  CIRCULAR or CIRC means RR,RL,LR,LL." << endl;
      os << "   I, Q, U, and/or V.      IQUV or STOKES means I,Q,U,V." << endl;
      os << "   PTOTAL is the polarized intensity (sqrt(Q**2+U**2+V**2))" << endl;
      os << "   PLINEAR is the linearly polarized intensity (sqrt(Q**2+U**2))" << endl;
      os << "   PFTOTAL is the polarization fraction (Ptotal/I)" << endl;
      os << "   PFLINEAR is the linear polarization fraction (Plinear/I)" << endl;
      os << "   PANGLE is the linear polarization angle (0.5*arctan(U/Q)) (in radians)" << endl;
      os << "If not given, the string argument defaults to 'IQUV'." << endl;
    }
  }

  void HelpMsCalUDF::showFuncsSelection (ostream& os)
  {
    os << "CASA selection functions:" << endl;
    os << "  bool MSCAL.BASELINE (string) "
      " select using a baseline string" << endl;
    os << "  bool MSCAL.TIME (string)     "
      " select using a time string" << endl;
    os << "  bool MSCAL.FIELD (string)    "
      " select using a field string" << endl;
    os << "  bool MSCAL.FEED (string)     "
      " select using a feed string" << endl;
    os << "  bool MSCAL.SCAN (string)     "
      " select using a scan string" << endl;
    os << "  bool MSCAL.SPW (string)      "
      " select using a spectral-window string" << endl;
    os << "  bool MSCAL.UVDIST (string)   "
      " select using a uv-distance string" << endl;
    os << "  bool MSCAL.STATE (string)    "
      " select using a state string" << endl;
    os << "  bool MSCAL.OBS (string)      "
      " select using an observation string" << endl;
    os << "  bool MSCAL.ARRAY (string)    "
      " select using an array string" << endl;
    os << " More information about the CASA selection syntax can be found at"
       << endl << " http://casacore.github.io/casacore-notes/263.html" << endl;
  }

  void HelpMsCalUDF::showFuncsSubtable (ostream& os)
  {
    os << "Subtable information functions:" << endl;
    os << "  MSCAL.ANT1NAME()         "
      " the name of ANTENNA1" << endl;
    os << "  MSCAL.ANT2NAME()         "
      " the name of ANTENNA2" << endl;
    os << "  MSCAL.ANT1COL('column')  "
      " for ANTENNA1 the value in given column in ANTENNA subtable" << endl;
    os << "  MSCAL.ANT2COL('column')  "
      " for ANTENNA2 the value in given column in ANTENNA subtable" << endl;
    os << "  MSCAL.STATECOL('column') "
      " for STATE_ID the value in given column in STATE subtable" << endl;
    os << "  MSCAL.OBSCOL('column')   "
      " for OBSERVATION_ID the value in given column in OBSERVATION subtable" << endl;
    os << "  MSCAL.SPWCOL('column')   "
      " for DATA_DESC_ID the value in given column in SPECTRAL_WINDOW subtable" << endl;
    os << "  MSCAL.POLCOL('column')   "
      " for DATA_DESC_ID the value in given column in POLARIZATION subtable" << endl;
    os << "  MSCAL.FIELDCOL('column') "
      " for FIELD_ID the value in given column in FIELD subtable" << endl;
    os << "  MSCAL.PROCCOL('column')  "
      " for PROCESSOR_ID the value in given column in PROCESSOR subtable" << endl;
    os << "  MSCAL.SUBCOL('subtable', 'column', 'idcolumn') " << endl
       << "    for the id-column the value in given column in given subtable"
       << endl;
  }


  UDFBase* HelpMsCalUDF::makeHELP (const String&)
    { return new HelpMsCalUDF(); }

  void HelpMsCalUDF::setup (const Table&, const TaQLStyle&)
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

  String HelpMsCalUDF::getString (const TableExprId& id)
  {
    ostringstream os;
    String type;
    if (operands().size() == 1) {
      type = operands()[0]->getString(id);
      type.downcase();
    }
    if (type.empty()) {
      showFuncsDerived (os);
      os << endl;
      showFuncsStokes (os, False);
      os << endl;
      showFuncsSelection (os);
      os << endl;
      showFuncsSubtable (os);
      os << endl
         << "Use 'show function mscal <type> for more information"
         << endl;
    } else if (type == "derived") {
      showFuncsDerived (os);
    } else if (type == "stokes") {
      showFuncsStokes (os, True);
    } else if (type == "selection") {
      showFuncsSelection (os);
    } else if (type == "subtable"  ||  type == "subtables") {
      showFuncsSubtable (os);
    }
    if (os.str().empty()) {
      os << type
         << " is an unknown mscal subtype; use derived, stokes, selection or subtable"
         << endl;
    } else {
      os << endl << "See also section 'Special MeasurementSet functions' at"
         << endl << "http://casacore.github.io/casacore-notes/199.html"
         << endl;
    }
    return os.str();
  }

}  // end namespace
