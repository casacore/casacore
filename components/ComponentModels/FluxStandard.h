//# FluxStandard.h: Compute flux densities for standard reference sources
//# Copyright (C) 1996,1997,1999,2001
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
//# $Id$

#ifndef COMPONENTS_FLUXSTANDARD_H
#define COMPONENTS_FLUXSTANDARD_H

#include <casa/aips.h>
#include <components/ComponentModels/Flux.h>
#include <measures/Measures/MDirection.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// Forward declarations
class String;        //#include <casa/BasicSL/String.h>
class MEpoch;        //#include <measures/Measures/MEpoch.h>
class MFrequency;    //#include <measures/Measures/MFrequency.h>
class SpectralModel; //#include <components/ComponentModels/SpectralModel.h>

// <summary> 
// FluxStandard: Compute flux densities for standard reference sources
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="Flux">Flux</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density" and "standard".
// </etymology>
//
// <synopsis>
// The FluxStandard class provides a means to compute total flux
// densities for specified non-variable sources on a standard
// flux density scale, such as that established by Baars or
// Perley and Taylor.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Encapsulate information on standard flux density computation in one class.
// </motivation>
//
// <todo asof="99/06/01">
// <li> closer integration into component models.
// </todo>

class FluxStandard
{
 public:
  // Flux scale types.
  // Standards which do not include resolution info must come before
  // HAS_RESOLUTION_INFO, and those with it must come after.
  enum FluxScale {
    // Perley (1990); plus Reynolds (1934-638; 7/94); Baars (3C138)
    PERLEY_90 = 0,

    // Perley and Taylor (1995.2); plus Reynolds (1934-638; 7/94)
    PERLEY_TAYLOR_95,

    // Perley and Taylor (1999.2); plus Reynolds (1934-638; 7/94)
    PERLEY_TAYLOR_99,

    // Baars scale
    // Baars J. W. M., Genzel R., Pauliny-Toth I. I. K., et al., 1977,
    // A&A, 61, 99
    // http://cdsads.u-strasbg.fr/abs/1977A%26A....61...99B
    BAARS,

    // Perley-Butler 2010 Scale (using VLA [not EVLA!] data)
    PERLEY_BUTLER_2010,

    // Perley-Butler 2013 
    PERLEY_BUTLER_2013,

    HAS_RESOLUTION_INFO,

    // Estimate the flux density for a Solar System object using a JPL Horizons
    // ephemeris/data page and model provided by Bryan Butler.
    SS_JPL_BUTLER = HAS_RESOLUTION_INFO,

    // The number of standards in this enumerator.
    NUMBER_STANDARDS
  };

  // Default constructor, and destructor
  FluxStandard(const FluxStandard::FluxScale scale =
               FluxStandard::PERLEY_TAYLOR_99);
  ~FluxStandard();

  // Compute the flux density for a specified source at a specified frequency
  Bool compute (const String& sourceName, const MFrequency& mfreq,
		Flux<Double>& value, Flux<Double>& error);

  // Compute the flux densities and their uncertainties for a specified source
  // at a set of specified frequencies.
  Bool compute(const String& sourceName, const Vector<MFrequency>& mfreqs,
	       Vector<Flux<Double> >& values,
               Vector<Flux<Double> >& errors,
               const Bool verbose=True);

  // Compute the flux densities and their uncertainties for a specified source
  // for a set of sets of specified frequencies, i.e. mfreqs[spw] is a set of
  // frequencies for channels in spectral window spw, and values and errors are
  // arranged the same way.
  Bool compute(const String& sourceName,
               const Vector<Vector<MFrequency> >& mfreqs,
               Vector<Vector<Flux<Double> > >& values,
               Vector<Vector<Flux<Double> > >& errors);

  // Like compute, but it also saves a set of ComponentLists for the source to
  // disk and puts the paths (sourceName_mfreq_mtime.cl) in clnames, making it
  // suitable for resolved sources.
  // mtime is ignored for nonvariable objects.
  // Solar System objects are typically resolved and variable!
  // The ComponentList names are formed from prefix, sourceName, the
  // frequencies, and times.
  Bool computeCL(const String& sourceName, const Vector<Vector<MFrequency> >& mfreqs,
                 const MEpoch& mtime, const MDirection& position,
                 Vector<Vector<Flux<Double> > >& values,
                 Vector<Vector<Flux<Double> > >& errors,
                 Vector<String>& clnames, const String& prefix="");

  // Take a component cmp and save it to a ComponentList on disk, returning the
  // pathname.  ("" if unsuccessful, sourceName_mfreqGHzDateTime.cl otherwise)
  //
  // This is also used outside of FluxStandard, but it is declared here instead
  // of in ComponentList because it is somewhat specialized, mainly in setting
  // up the pathname.  The ComponentList name is formed from prefix, sourceName,
  // mfreq, and mtime.
  //
  static String makeComponentList(const String& sourceName, const MFrequency& mfreq,
                                  const MEpoch& mtime, const Flux<Double>& fluxval,
                                  const ComponentShape& cmp,
                                  const SpectralModel& spectrum,
				  const String& prefix="");

  // Variation of the above that will fill a TabularSpectrum with mfreqs and
  // values if appropriate.
  static String makeComponentList(const String& sourceName,
                                  const Vector<MFrequency>& mfreqs,
                                  const MEpoch& mtime,
                                  const Vector<Flux<Double> >& values,
                                  const ComponentShape& cmp,
                                  const String& prefix="");

  // Decode a string representation of the standard or catalog name
  static Bool matchStandard(const String& name, 
			    FluxStandard::FluxScale& stdEnum,
			    String& stdName);

  // Return a standard string description for each scale or catalog
  static String standardName(const FluxStandard::FluxScale& stdEnum);

 private:
  // Flux scale in use
  FluxStandard::FluxScale itsFluxScale;

  Bool has_direction_p;

  MDirection direction_p;
};

} //# NAMESPACE CASA - END

#endif
