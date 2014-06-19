//# ConstantSpectrum.cc.cc:
//# Copyright (C) 1998,1999,2000,2003
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

#include <components/ComponentModels/ConstantSpectrum.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

ConstantSpectrum::ConstantSpectrum()
  :SpectralModel()
{
  DebugAssert(ok(), AipsError);
}

ConstantSpectrum::ConstantSpectrum(const ConstantSpectrum& other) 
  :SpectralModel(other)
{
  DebugAssert(ok(), AipsError);
}

ConstantSpectrum::~ConstantSpectrum() {
  DebugAssert(ok(), AipsError);
}

ConstantSpectrum& ConstantSpectrum::operator=(const ConstantSpectrum& other) {
  SpectralModel::operator=(other);
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::SpectralShape ConstantSpectrum::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::CONSTANT_SPECTRUM;
}

Double ConstantSpectrum::sample(const MFrequency&) const {
  DebugAssert(ok(), AipsError);
  return 1.0;
}

void ConstantSpectrum::sampleStokes(const MFrequency&, Vector<Double>& iquv) const {
  DebugAssert(ok(), AipsError);
  if(iquv.nelements() != 4 ){//keeps compiler happy
    };
    
    
}

void ConstantSpectrum::sample(Vector<Double>& scale, 
			      const Vector<MFrequency::MVType>&, 
			      const MFrequency::Ref&) const {
  DebugAssert(ok(), AipsError);
  scale = 1.0;
}

  void ConstantSpectrum::sampleStokes(Vector<Vector<Double> >& iquv, 
			      const Vector<MFrequency::MVType>& freq, 
			      const MFrequency::Ref&) const {
  DebugAssert(ok(), AipsError);
  if(freq.nelements() != iquv.nelements()){
    throw(AipsError("ConstSpectrum: frequency length does not match stokes val"));
  }
  
}
SpectralModel* ConstantSpectrum::clone() const {
  DebugAssert(ok(), AipsError);
  SpectralModel* tmpPtr = new ConstantSpectrum(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

uInt ConstantSpectrum::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 0;
}

void ConstantSpectrum::setParameters(const Vector<Double>& newSpectralParms) {
  DebugAssert(newSpectralParms.nelements() == nParameters(), AipsError);
  DebugAssert(ok(), AipsError);
  // Suppress compiler warning about unused variable
  if (&newSpectralParms == 0) {}; 
}

Vector<Double> ConstantSpectrum::parameters() const {
  DebugAssert(ok(), AipsError);
  return Vector<Double>(0);
}

void ConstantSpectrum::setErrors(const Vector<Double>& newSpectralErrs) {
  DebugAssert(ok(), AipsError);
  // Suppress compiler warning about unused variable
  if (&newSpectralErrs == 0) {}; 
}

Vector<Double> ConstantSpectrum::errors() const {
  DebugAssert(ok(), AipsError);
  Vector<Double> tmp(1,0.0);
  return tmp;
}

Bool ConstantSpectrum::fromRecord(String& errorMessage, 
				  const RecordInterface& record) {
  const Bool retVal = SpectralModel::fromRecord(errorMessage, record);
  DebugAssert(ok(), AipsError);
  return retVal;
}

Bool ConstantSpectrum::toRecord(String& errorMessage,
				RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  return SpectralModel::toRecord(errorMessage, record);
}

Bool ConstantSpectrum::convertUnit(String&,
				   const RecordInterface&) {
  DebugAssert(ok(), AipsError);
  return True;
}
 
Bool ConstantSpectrum::ok() const {
  return SpectralModel::ok();
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ConstantSpectrum"
// End:

} //# NAMESPACE CASA - END

