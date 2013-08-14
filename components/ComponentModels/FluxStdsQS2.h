//# FluxStdsQS2.h: Client class declarations for flux standards 
//# some of which support time-variable sources.
//
//# Copyright (C) 2013
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
#ifndef COMPONENTS_FLUXSTDSQS2_H
#define COMPONENTS_FLUXSTDSQS2_H

#include <components/ComponentModels/FluxCalcLogFreqPolynomial.h>
#include <components/ComponentModels/FluxCalcVQS.h>

namespace casa { //# NAMESPACE CASA - BEGIN
namespace NSTDS { //# NAMESPACE NSTDS "new standards"  - BEGIN
                  //#   once old code (FluxCalcQS etc) deleted this can be
                  //#   removed.
// <summary> 
// FluxStdBaars: The Baars flux standard.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqPolynomial">FluxCalcLogFreqPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", and "Baars".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqPolynomial with the Baars coefficients and
// list of recognized sources.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>

class FluxStdBaars : public virtual FluxCalcVQS,
	private FluxCalcLogFreqPolynomial 
{
private:
  virtual Bool setSourceCoeffs();
};

// <summary> 
// FluxStdPerley90: The Perley90 flux standard.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
//
// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqPolynomial">FluxCalcLogFreqPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", "Perley", and "1990".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqPolynomial with the Perley_90 coefficients and
// list of recognized sources.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>
class FluxStdPerley90 : public virtual FluxCalcVQS,
	private FluxCalcLogFreqPolynomial 
{
private:
  virtual Bool setSourceCoeffs();
};

// <summary> 
// FluxStdPerleyTaylor95: The PerleyTaylor95 flux standard.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
//
// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqPolynomial">FluxCalcLogFreqPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", "Perley", "Taylor", and "1995".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqPolynomial with the PerleyTaylor95 coefficients and
// list of recognized sources.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>
class FluxStdPerleyTaylor95 : public virtual FluxCalcVQS,
	private FluxCalcLogFreqPolynomial 
{
private:
  virtual Bool setSourceCoeffs();
};

// <summary> 
// FluxStdPerleyTaylor99: The PerleyTaylor99 flux standard.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
//
// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqBrokenPolynomial">FluxCalcLogFreqBrokenPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", "Perley", "Taylor", and "1999".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqBrokenPolynomial with the PerleyTaylor99 coefficients and
// list of recognized sources.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>
class FluxStdPerleyTaylor99 : public virtual FluxCalcVQS,
	private FluxCalcLogFreqBrokenPolynomial 
{
private:
  virtual Bool setSourceCoeffs();
};

// <summary> 
// FluxStdPerleyButler2010: The PerleyButler2010 flux standard.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
//
// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqBrokenPolynomial">FluxCalcLogFreqBrokenPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", "Perley", "Butler", and "2010".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqBrokenPolynomial with the PerleyButler2010 coefficients and
// list of recognized sources.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>
class FluxStdPerleyButler2010 : public virtual FluxCalcVQS,
                                private FluxCalcLogFreqBrokenPolynomial 
{
private:
  virtual Bool setSourceCoeffs();
};
// <summary>
// FluxStdPerleyButler2013: The PerleyButler2013 flux standard.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
//
// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqBrokenPolynomial">FluxCalcLogFreqBrokenPolynomial</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", "Perley", "Butler", and "2013".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqBrokenPolynomial with the PerleyButler2013 coefficients and
// list of recognized sources. The coeffients are stored in a table along with epoch information as
// some of the calibrators are varible enough to be interpolated in time. 
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>
class FluxStdPerleyButler2013 : public virtual FluxCalcVQS,
                                private FluxCalcLogFreqPolynomial
{
/***
public:
Bool operator()(Vector<Flux<Double> >& values,
                  Vector<Flux<Double> >& errors,
                  const Vector<MFrequency>& mfreqs,
                  const MEpoch& mtime);
**/ 
private:
  virtual Bool setSourceCoeffs();
};

// <summary>
// FluxStdScaifeHeald2012: The Scaife & Heald (2012) flux standard.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
//
// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// <li><linkto class="FluxCalcLogFreqPolynomialSH">FluxCalcLogFreqPolynomialSH</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "standard", "Scaife", "Heald", and "2012".
// </etymology>
//
// <synopsis>
// This specializes FluxCalcLogFreqPolynomialSH with the Scaife & Heald (2012)
// coefficients and list of recognized sources.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Support flux density calibration.
// </motivation>
class FluxStdScaifeHeald2012 : public virtual FluxCalcVQS,
                               private FluxCalcLogFreqPolynomialSH
{
private:
  virtual Bool setSourceCoeffs();
};


} //# NAMESPACE NSTDS -END
} //# NAMESPACE CASA - END

#endif /* COMPONENTS_FLUXSTDSQS2_H */
