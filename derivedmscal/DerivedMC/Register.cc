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
//#
//# $Id$

#include <derivedmscal/DerivedMC/Register.h>
#include <derivedmscal/DerivedMC/DerivedMSCal.h>
#include <derivedmscal/DerivedMC/UDFMSCal.h>

#include <casa/iostream.h>
using namespace casa;

void register_derivedmscal()
{
  // Register the table virtual column engine.
  DerivedMSCal::registerClass();

  // Register the TaQL UDFs.
  UDFBase::registerUDF ("derivedmscal.HA",    UDFMSCal::makeHA);
  UDFBase::registerUDF ("derivedmscal.HA1",   UDFMSCal::makeHA1);
  UDFBase::registerUDF ("derivedmscal.HA2",   UDFMSCal::makeHA2);
  UDFBase::registerUDF ("derivedmscal.PA1",   UDFMSCal::makePA1);
  UDFBase::registerUDF ("derivedmscal.PA2",   UDFMSCal::makePA2);
  UDFBase::registerUDF ("derivedmscal.LAST",  UDFMSCal::makeLAST);
  UDFBase::registerUDF ("derivedmscal.LAST1", UDFMSCal::makeLAST1);
  UDFBase::registerUDF ("derivedmscal.LAST2", UDFMSCal::makeLAST2);
  UDFBase::registerUDF ("derivedmscal.AZEL1", UDFMSCal::makeAZEL1);
  UDFBase::registerUDF ("derivedmscal.AZEL2", UDFMSCal::makeAZEL2);
  UDFBase::registerUDF ("derivedmscal.UVW",   UDFMSCal::makeUVW);
}
