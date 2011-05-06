//# FluxCalcLogFreqPolynomial.h: Implementation base classes for flux standards
//# which are (possibly broken) polynomials of log10(frequency).
//# Copyright (C) 2010 Associated Universities, Inc. Washington DC, USA.
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
#ifndef COMPONENTS_FLUXCALCLOGFREQPOLYNOMIAL_H
#define COMPONENTS_FLUXCALCLOGFREQPOLYNOMIAL_H

#include <components/ComponentModels/FluxStandard.h>
#include <components/ComponentModels/FluxCalcQS.h>
#include <casa/BasicSL/String.h>
#include <measures/Measures/MFrequency.h>

// Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// FluxCalcLogFreqPolynomial: Implementation base class for flux standards
// which are polynomials of log10(frequency).
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcQS">FluxCalcQS</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "calculator", "log10(frequency)", and "polynomial".
// </etymology>
//
// <synopsis>
// The FluxCalcLogFreqPolynomial class provides machinery to compute total flux
// densities for specified non-variable sources where the flux density is well
// described by a low order polynomial of log(frequency).
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Encapsulate the machinery for most of the flux density standards in one class.
//
// The flux standards for cm astronomy are deliberately chosen to be distant,
// non-varying, and bright around 1 GHz.  Since such objects tend to be
// dominated by synchrotron radiation their flux density is usually described
// by a polynomial of log(frequency).
// </motivation>
//

class FluxCalcLogFreqPolynomial : public virtual FluxCalcQS {
public:
  // Some abbreviations, since the classes derived from this have to
  // define many polynomial coefficients.
  typedef RigidVector<Float, 3> RVF3;
  typedef RigidVector<Float, 4> RVF4;
  
  // Set the log10(frequency) polynomial coefficients for calculating the flux
  // density and its uncertainty, and the unit (typically "MHz" or "GHz") that
  // the coefficients assume.  Note that errcoeffs does not have to have the
  // same number of terms as lfcoeffs, or any terms at all, and that each term
  // in its polynomial is (errcoeff[order] * pow(log10(freq), order))**2.
  //FluxCalcLogFreqPolynomial(const String& freqUnit, const Vector<Double>& lfcoeffs,
  //                          const Vector<Double>& errcoeffs);

  // Set value and error with the expected flux density and its uncertainty
  // (0.0 if unknown) at mfreq.
  virtual Bool operator()(Flux<Double>& value, Flux<Double>& error,
                          const MFrequency& mfreq);

  virtual Bool setSource(const String& sourceName);
  void setFreqUnit(const String& freqUnit);

  // Functions for setting up coeffs_p by taking a bunch of numbers
  // (packaged in RigidVectors) and formatting them into coeffs_p.

  // Takes a RigidVector for the flux density coefficients and
  // second one for the uncertainty coefficients, and fills coeffs_p with them.
  template<Int lford, Int errord>
  void fill_coeffs(const RigidVector<Float, lford>& lfrv,
                   const RigidVector<Float, errord>& errrv);

  // Like fill_coeffs(lfrv, errrv), but it only takes the flux density
  // coefficients, and substitutes an empty Vector for the error coefficients.
  template<Int lford>
  void fill_coeffs(const RigidVector<Float, lford>& lfrv);

  void fill_coeffs(const Vector<Float>& lfv);

private:
  virtual Bool setSourceCoeffs() = 0;

  // The first element of this pair of Vectors is a Vector of coefficients for
  // the flux density polynomial (of log10(frequency)).  The second element is
  // for estimating the flux density's uncertainty with a similar polynomial,
  // but each term is (coeff * log10(freq))**2.  It does not need to have the
  // same number of coefficients as the first element, or even any
  // coefficients.  Both Vectors start with the 0th order term.
  RigidVector<Vector<Float>, 2> coeffs_p;

  // The frequency unit (e.g. "MHz" or "GHz") assumed by coeffs_p.
  String freqUnit_p;
};

// <summary> 
// FluxCalcLogFreqBrokenPolynomial: Implementation base class for flux standards
// which are broken polynomials of log10(frequency).
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="FluxCalcLogFreqPolynomial">FluxCalcLogFreqPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From FluxCalcLogFreqPolynomial  and "broken".
// </etymology>
//
// <synopsis>
// The FluxCalcLogFreqBrokenPolynomial class extends FluxCalcLogFreqPolynomial
// to allow one set of coefficients to be used below a certain frequency (the
// break frequency) and another above it.  Ideally the sets should mesh well
// enough to make the resulting function at least roughly continuous.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Some of the flux classes use a broken polynomial for 1934-638, and some do not.
// </motivation>
//
// <todo asof="2010/07/26">
// <li> Handle an arbitrary number of breaks.
// </todo>
class FluxCalcLogFreqBrokenPolynomial : public virtual FluxCalcLogFreqPolynomial {
public:
  FluxCalcLogFreqBrokenPolynomial();
  
  template <Int lford>
  void fill_lohi_coeffs(const RigidVector<Float, lford>& lorv,
                        const MFrequency& break_freq,
                        const RigidVector<Float, lford>& hirv);

  virtual Bool operator()(Flux<Double>& value, Flux<Double>& error,
                          const MFrequency& mfreq);
private:
  MFrequency    break_freq_p;
  Bool          in_low_state_p;
  Vector<Float> low_coeffs_p;
  Vector<Float> high_coeffs_p;
};

} //# NAMESPACE CASA - END

#ifndef AIPS_NO_TEMPLATE_SRC
#include <components/ComponentModels/FluxCalcLogFreqPolynomial.tcc>
#endif //# AIPS_NO_TEMPLATE_SRC

#endif
