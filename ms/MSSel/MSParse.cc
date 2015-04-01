//# MSParse.cc: Classes to hold results from an ms grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#include <casacore/ms/MSSel/MSParse.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/ostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  MeasurementSet *MSParse::ms_p = 0;
  MSSelectableTable *MSParse::msInterface_p = 0;
  
  //# Default constructor.
  MSParse::MSParse():tempMSInterface_p(NULL)
  {
    tempMSInterface_p = new MSInterface();
  }
  
  //# Constructor with given ms name.
  MSParse::MSParse (const MeasurementSet* ms, const String& shorthand)
    : shorthand_p (shorthand), tempMSInterface_p(NULL)
  {
    ms_p = const_cast<MeasurementSet *>(ms);
    tempMSInterface_p = new MSInterface(*ms);
  }
  
  MSParse::MSParse (const MSSelectableTable* msLike, const String& shorthand)
    :shorthand_p (shorthand), tempMSInterface_p(NULL)
  {
    msInterface_p = const_cast<MSSelectableTable *>(msLike);
  }

  MSParse::~MSParse()
  {
    if (tempMSInterface_p != NULL) delete tempMSInterface_p;
  }

  MSParse::MSParse (const MSParse& that)
    : shorthand_p (that.shorthand_p)
  {}

  MSParse& MSParse::operator= (const MSParse& that)
  {
    if (this != &that) 
      shorthand_p = that.shorthand_p;

    return *this;
  }

  Bool MSParse::test (const String& str) const
  {
    return (shorthand_p == str  ?  True : False);
  }

  String& MSParse::shorthand()
  {
    return shorthand_p;
  }

  MeasurementSet* MSParse::ms()
  {
    if (msInterface_p != NULL) return (MeasurementSet *)msInterface()->asMS();
    else return ms_p;
  }

  MSSelectableTable* MSParse::msInterface()
  {
    if (msInterface_p != NULL) return msInterface_p; // If constructed with MSInterface
    else if (tempMSInterface_p != NULL) return tempMSInterface_p; // If constructed with MS
    else throw(AipsError("Internal error in MSParse::msInterface()"));
  }

  void MSParse::addCondition(TableExprNode& target, TableExprNode& source)
  {
    if(target.isNull()) target = source;
    else                target = target || source;
  }
  //# The AipsIO functions are needed for the list of MSParse, but
  //# we do not support it actually.
  AipsIO& operator<< (AipsIO& ios, const MSParse&)
  {
    throw (AipsError ("AipsIO << MSParse& not possible"));
    return ios;
  }
  AipsIO& operator>> (AipsIO& ios, MSParse&)
  {
    throw (AipsError ("AipsIO >> MSParse& not possible"));
    return ios;
  }

} //# NAMESPACE CASACORE - END

