//# QBase.cc: base class for Quantum
//# Copyright (C) 1994,1995,1996,1998,2001
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
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Quanta/QBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

QBase::QBase() 
: qUnit() {}

QBase::QBase(const QBase &other) 
: qUnit(other.qUnit) {}

QBase::QBase(const Unit &s) 
: qUnit(s) {}

QBase::~QBase() {}

//# QBase general member functions

const String &QBase::getUnit() const {
    return qUnit.getName();
}

void QBase::setUnit(const Unit &s) {
    qUnit = s;
}

void QBase::setUnit(const QBase &other) {
    qUnit = other.qUnit;
}

Bool QBase::isConform(const Unit &s) const {
    return (qUnit.getValue() == s.getValue());
}

Bool QBase::isConform(const QBase &other) const {
    return (qUnit.getValue() == other.qUnit.getValue());
}


//# Global functions
ostream &operator<<(ostream &os, const QBase &meas) {
  meas.print(os);
  return os;
}

LogIO &operator<<(LogIO& os, const QBase &meas) {
  os.output() << meas;
  return os;
}

} //# NAMESPACE CASACORE - END

