//# MSSelectionError.cc: Error classes for the MSSelection classes
//# Copyright (C) 1994,1995,1996,1997,2000
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/ms/MSSel/MSSelectionError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


MSSelectionError::MSSelectionError (Category c)
  : AipsError("MSSelection Error",c), hasMessage(False)
{}
MSSelectionError::MSSelectionError (const String& str,Category c)
  : AipsError(str,c), hasMessage(False)
{}
MSSelectionError::~MSSelectionError () noexcept
{}

void MSSelectionError::addMessage(String& mesg)
{
  message = message+mesg;
  hasMessage = True;
}

void MSSelectionError::changeMessage(String& mesg)
{
  message = mesg;
}
//
//-----------------------------------------------------------------------------------
//
String constructMessage(const Int pos, const String& command)
{
  ostringstream newMesg;
  newMesg << endl << "(near char. " << pos << " in string \"" << command << "\")";
  //
  // Make a few guess about user errors and help them out 
  // (did someone say I don't do user support? ;-))
  //
  if ((pos > 0) && (pos < (Int)command.length()) && (command[pos-1] == '-'))
    newMesg << endl << "[TIP: Did you know we use \"~\" as the range operator (for a good reason)?]";
  return String(newMesg.str().c_str());
}
//
//------------------------Time selection expression parser exceptions----------------
//
MSSelectionNullSelection::MSSelectionNullSelection (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionNullSelection::~MSSelectionNullSelection () noexcept
{}

MSSelectionNullExpr::MSSelectionNullExpr (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionNullExpr::~MSSelectionNullExpr () noexcept
{}

MSSelectionNullTEN::MSSelectionNullTEN (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionNullTEN::~MSSelectionNullTEN () noexcept
{}
//
//------------------------Time selection expression parser exceptions----------------
//
MSSelectionTimeError::MSSelectionTimeError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionTimeError::~MSSelectionTimeError () noexcept
{}

MSSelectionTimeParseError::MSSelectionTimeParseError (const String& str,Category c)
: MSSelectionTimeError(str,c)
{}
MSSelectionTimeParseError::~MSSelectionTimeParseError () noexcept
{}
//
//------------------------Baseline selection expression parser exceptions------------
//
MSSelectionAntennaError::MSSelectionAntennaError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionAntennaError::~MSSelectionAntennaError () noexcept
{}

MSSelectionAntennaParseError::MSSelectionAntennaParseError (const String& str,Category c)
: MSSelectionAntennaError(str,c)
{}
MSSelectionAntennaParseError::~MSSelectionAntennaParseError () noexcept
{}

//
//------------------------Field selection expression parser exceptions---------------
//
MSSelectionFieldError::MSSelectionFieldError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionFieldError::~MSSelectionFieldError () noexcept
{}

MSSelectionFieldParseError::MSSelectionFieldParseError (const String& str,Category c)
: MSSelectionFieldError(str,c)
{}
MSSelectionFieldParseError::~MSSelectionFieldParseError () noexcept
{}

MSSelectionFieldWarning::MSSelectionFieldWarning (const String& str,Category c)
: MSSelectionFieldError(str,c)
{}

MSSelectionFieldWarning::~MSSelectionFieldWarning () noexcept
{}
//
//------------------------UVDist selection expression parser exceptions--------------
//
MSSelectionUvDistError::MSSelectionUvDistError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionUvDistError::~MSSelectionUvDistError () noexcept
{}

MSSelectionUvDistParseError::MSSelectionUvDistParseError (const String& str,Category c)
: MSSelectionUvDistError(str,c)
{}
MSSelectionUvDistParseError::~MSSelectionUvDistParseError () noexcept
{}
//
//------------------------SPW selection expression parser exceptions-----------------
//
MSSelectionSpwError::MSSelectionSpwError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionSpwError::~MSSelectionSpwError () noexcept
{}

MSSelectionSpwParseError::MSSelectionSpwParseError (const String& str,Category c)
: MSSelectionSpwError(str,c)
{}
MSSelectionSpwParseError::~MSSelectionSpwParseError () noexcept
{}
MSSelectionSpwWarning::MSSelectionSpwWarning (const String& str,Category c)
: MSSelectionSpwError(str,c)
{}

MSSelectionSpwWarning::~MSSelectionSpwWarning () noexcept
{}
//
//------------------------Scan selection expression parser exceptions----------------
//
MSSelectionScanError::MSSelectionScanError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionScanError::~MSSelectionScanError () noexcept
{}

MSSelectionScanParseError::MSSelectionScanParseError (const String& str,Category c)
: MSSelectionScanError(str,c)
{}
MSSelectionScanParseError::~MSSelectionScanParseError () noexcept
{}
//
//------------------------Sub-array selection expression parser exceptions-----------
//
MSSelectionArrayError::MSSelectionArrayError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionArrayError::~MSSelectionArrayError () noexcept
{}

MSSelectionArrayParseError::MSSelectionArrayParseError (const String& str,Category c)
: MSSelectionArrayError(str,c)
{}
MSSelectionArrayParseError::~MSSelectionArrayParseError () noexcept
{}
//
//-----------------------------------------------------------------------------------
//
MSSelectionPolnError::MSSelectionPolnError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionPolnError::~MSSelectionPolnError () noexcept
{}

MSSelectionPolnParseError::MSSelectionPolnParseError (const String& str,Category c)
: MSSelectionPolnError(str,c)
{}
MSSelectionPolnParseError::~MSSelectionPolnParseError () noexcept
{}

//
//-----------------------------------------------------------------------------------
//
MSSelectionStateError::MSSelectionStateError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionStateError::~MSSelectionStateError () noexcept
{}

MSSelectionStateParseError::MSSelectionStateParseError (const String& str,Category c)
: MSSelectionStateError(str,c)
{}
MSSelectionStateParseError::~MSSelectionStateParseError () noexcept
{}

//
//-----------------------------------------------------------------------------------
//
MSSelectionObservationError::MSSelectionObservationError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionObservationError::~MSSelectionObservationError () noexcept
{}

MSSelectionObservationParseError::MSSelectionObservationParseError (const String& str,Category c)
: MSSelectionObservationError(str,c)
{}
MSSelectionObservationParseError::~MSSelectionObservationParseError () noexcept
{}

//
//-----------------------------------------------------------------------------------
//
MSSelectionFeedError::MSSelectionFeedError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionFeedError::~MSSelectionFeedError () noexcept
{}

MSSelectionFeedParseError::MSSelectionFeedParseError (const String& str,Category c)
: MSSelectionFeedError(str,c)
{}
MSSelectionFeedParseError::~MSSelectionFeedParseError () noexcept
{}

} //# NAMESPACE CASACORE - END

