//# MeasIERS.cc: Interface to IERS tables
//# Copyright (C) 1996
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

//# Includes
#ifdef __GNUG__
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_measiers_bug1;
#endif
#include <aips/Measures/MeasIERS.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/OS/Time.h>
///#include <aips/Logging.h>
#include <aips/Measures/MRefData.h>

// MeasIERS_init class

MeasIERS_init::MeasIERS_init() {
  if (count++ == 0) {
    MeasIERS::measured = 0;
    MeasIERS::predicted = 0;
    MeasIERS::measFlag = True;
    MeasIERS::predictFlag = True;
    Time now;
    MeasIERS::dateNow = now.modifiedJulianDay();
  };
}

uShort MeasIERS_init::count = 0; 

MeasIERS_init::~MeasIERS_init() {
  if (--count == 0) {
    delete MeasIERS::measured;
    delete MeasIERS::predicted;
  };
}

// MeasIERS class

//# Constants
const Double MeasIERS::INTV = 5;

//# Constructors
MeasIERS::MeasIERS() {}

MeasIERS::MeasIERS(const MeasIERS &other) {}

MeasIERS &MeasIERS::operator=(const MeasIERS &other) {
    if (this != &other) {
    }
    return *this;
}

//# Destructor
MeasIERS::~MeasIERS() {}

//# Operators

//# Member functions
Bool MeasIERS::get(MeasIERS::Files file, 
		   MeasIERS::Types type, 
		   Double date,
		   Double &returnValue) {
  static const String names[MeasIERS::N_Types] = {
    "X",
    "Y",
    "dUT1",
    "dUT1R",
    "D",
    "DR",
    "dPsi",
    "dEpsilon",
    "OmegaR",
    "LeapSecond",
    "dUT1R_TAI"};
// The next line is for the gnu compiler
  static MeasureReferenceData::SelectionCriteria key 
    ///    = MeasureReferenceData::defaultKey;
    = MeasureReferenceData::threeKeys;

  returnValue = 0.0;
  if (MeasDetail::get(B_NoTable)) return True;
  Bool res = True;
  Double intv;
  if (file == PREDICTED || MeasDetail::get(B_ForcePredict) ||
      (dateNow - date) <= (MeasDetail::get(MeasIERS::D_PredictTime, intv) ? 
		 intv : MeasIERS::INTV)) {
    res = False;			// do predict
  };
  if (res) {
    res = initMeas();
    if (res) {
      if (type == dUTC_TAI) {
	res = measured->get(names[type], key,
			    date, returnValue);
      } else {
	res = measured->get(names[type], date, returnValue);
      };
    };
  };
  if (!res) {				// Retry for predicted
    if (!initPredict()) return False;
    if (type == dUTC_TAI) {
      res = predicted->get(names[type], key,
			   date, returnValue);
    } else {
      res = predicted->get(names[type], date, returnValue);
    };
  };
  return res;
}

Bool MeasIERS::initMeas() {
  if (!measured && measFlag) {
    try {
      if (MeasDetail::get(B_UseNEOS)) {
	measured = new MeasureReferenceData(String("IERSMeasured"),
					    MeasureReferenceData::threeKeys,
					    MeasureReferenceData::NEOS);
      } else {
	measured = new MeasureReferenceData(String("IERSMeasured"),
					    MeasureReferenceData::threeKeys,
					    MeasureReferenceData::IERS_CB);
      };
    } catch(AipsError x) {
      ///      Log(LogMessage(LogMessage::HIGH, LogMessage::ERROR, x.getMesg()));
      measured = 0;
    } end_try;
    measFlag = False;
  };
  return ToBool(measured);
}

Bool MeasIERS::initPredict() {
  if (!predicted && predictFlag) {
    try {
      if (MeasDetail::get(B_UseNEOS)) {
	predicted = new MeasureReferenceData(String("IERSPredicted"),
					     MeasureReferenceData::threeKeys,
					     MeasureReferenceData::NEOS);
      } else {
	predicted = new MeasureReferenceData(String("IERSPredicted"),
					     MeasureReferenceData::threeKeys,
					     MeasureReferenceData::IERS_CB);
      };
    } catch(AipsError x) {
      ///      Log(LogMessage(LogMessage::HIGH, LogMessage::ERROR, x.getMesg()));
      predicted = 0;
    } end_try;
    predictFlag = False;
  };
  return ToBool(predicted);
}

MeasureReferenceData *MeasIERS::measured;
MeasureReferenceData *MeasIERS::predicted;
Bool MeasIERS::measFlag;
Bool MeasIERS::predictFlag;
Double MeasIERS::dateNow;
