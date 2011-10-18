//# FluxCalcLogFreqPolynomial.cc: Implementation of FluxCalcLogFreqPolynomial.h
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
#include <components/ComponentModels/FluxCalcLogFreqPolynomial.h>
#include <casa/BasicSL/String.h>
#include <measures/Measures/MFrequency.h>

// Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>

#include <map>

namespace casa { //# NAMESPACE CASA - BEGIN

Bool FluxCalcLogFreqPolynomial::operator()(Flux<Double>& value,
                                           Flux<Double>& error,
                                           const MFrequency& mfreq)
{
  Double dt = log10(mfreq.get(freqUnit_p).getValue());

  Double fluxCoeff = coeffs_p(0)[coeffs_p(0).nelements() - 1];
  for(Int order = coeffs_p(0).nelements() - 2; order >= 0; --order)
    fluxCoeff = fluxCoeff * dt + coeffs_p(0)[order];
  
  Double coeffErr = 0.0;
  Double dtpow = 1.0;
  for(uInt order = 0; order < coeffs_p(1).nelements(); ++order){
    coeffErr += square(coeffs_p(1)[order] * dtpow);
    dtpow *= dt;
  }

  Double fluxDensity = pow(10.0, fluxCoeff);
  Double fluxError = coeffErr > 0.0 ? C::ln10 * fluxDensity * sqrt(coeffErr) : 0.0;

  value.setValue(fluxDensity);
  error.setValue(fluxError);

  // In this case the hard part of matching the std and src has already been done.
  return true;
}

Bool FluxCalcLogFreqPolynomial::setSource(const String& sourceName)
{
  Bool success = FluxCalcQS::setSource(sourceName);

  if(success)
    success = setSourceCoeffs();
  return success;
}

void FluxCalcLogFreqPolynomial::setFreqUnit(const String& freqUnit)
{
  freqUnit_p = freqUnit;
}

void FluxCalcLogFreqPolynomial::fill_coeffs(const Vector<Float>& lfv)
{
  coeffs_p(0) = lfv;
}


FluxCalcLogFreqBrokenPolynomial::FluxCalcLogFreqBrokenPolynomial() :
  FluxCalcLogFreqPolynomial::FluxCalcLogFreqPolynomial(),
  break_freq_p(Quantity(0.0, "Hz"))
{
}

Bool FluxCalcLogFreqBrokenPolynomial::operator()(Flux<Double>& value,
                                                 Flux<Double>& error,
                                                 const MFrequency& mfreq)
{
  // Make sure coeffs_p has the right values for mfreq.
  Double break_freq_in_Hz = break_freq_p.get("Hz").getValue();
  
  if(break_freq_in_Hz > 0.0){
    if(mfreq.get("Hz").getValue() > break_freq_in_Hz){
      if(in_low_state_p)
        fill_coeffs(high_coeffs_p);
    }
    else if(!in_low_state_p)
      fill_coeffs(low_coeffs_p);
  }

  // Proceed...
  return FluxCalcLogFreqPolynomial::operator()(value, error, mfreq);
}

} //# NAMESPACE CASA - END
