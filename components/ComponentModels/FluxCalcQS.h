//# FluxCalcQS.h: Base class for flux standard calculations which do not
//# explicitly depend on time.
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
#ifndef COMPONENTS_FLUXCALCQS_H
#define COMPONENTS_FLUXCALCQS_H

#include <components/ComponentModels/FluxStandard.h>
#include <casa/BasicSL/String.h>

// Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>

#include <map>


namespace casa { //# NAMESPACE CASA - BEGIN

//class Flux;
class MFrequency;
//class Vector;

// <summary> 
// FluxCalcQS: Base class for flux standard calculations which do not
// explicitly depend on time.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "calculator", and "quasistatic".
// </etymology>
//
// <synopsis>
// The FluxCalcQS class provides an interface and a small amount of machinery
// for computing total flux densities of nominally non-variable sources.
// "Nominally non-variable" means that the sources do not vary quickly, but
// they are remeasured, and new standards published, every few years.  See
// FluxStdsQS for actual definitions of the standards.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Provide a base time-independent interface for calculating standard flux
// densities, and include any common functions.
// </motivation>

class FluxCalcQS
{
public:
  typedef FluxCalcQS FCQS;
  typedef RigidVector<String, 4> RVS4;

  // Source identifiers.
  enum Source {
    THREEC286 = 0,      // 3C286
    THREEC48,
    THREEC147,
    THREEC138,
    NINETEEN34M638,   // 1934-638
    THREEC295,
    THREEC196,
    // The number of standards in this enumerator.
    NUMBER_SOURCES,
    UNKNOWN_SOURCE = NUMBER_SOURCES
  };
  
  virtual ~FluxCalcQS();

  virtual Bool operator()(Flux<Double>& value, Flux<Double>& error,
                          const MFrequency& mfreq) = 0;
  Bool operator()(Vector<Flux<Double> >& values,
                  Vector<Flux<Double> >& errors,
                  const Vector<MFrequency>& mfreqs);

  // If a FS::Source enum matches srcName, returns the enum.
  // Otherwise, FCQS::UNKNOWN_SOURCE.
  FCQS::Source srcNameToEnum(const String& srcName) const;

  // Sets srcEnum_p = srcNameToEnum(sourceName), and returns
  // srcEnum_p != FCQS::UNKNOWN_SOURCE
  virtual Bool setSource(const String& sourceName);

  FCQS::Source getSrcEnum();

protected:
  FluxCalcQS();   // Initializes names_p.

private:
  FCQS::Source srcEnum_p;       // The source identifier.

  // A map from an FS::Source enum to a list of recognized names for it.
  std::map<FCQS::Source, Vector<String> > names_p;
};

} //# NAMESPACE CASA - END

#endif /* COMPONENTS_FLUXCALCQS_H */
