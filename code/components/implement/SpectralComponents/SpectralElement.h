//# SpectralElement.h: Describes (a set of related) spectral lines
//# Copyright (C) 2001
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
//#
//# $Id$

#if !defined(TRIAL_SPECTRALELEMENT_H)
#define TRIAL_SPECTRALELEMENT_H

//# Includes
#include <aips/aips.h>

//# Forward Declarations
class String;
#include <aips/iosfwd.h>

// <summary>
// Describes (a set of related) spectral lines
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <ahlink module=Functionals>Functionals</ahlink> module
// </prerequisite>
//
// <etymology>
// From spectral line and element
// </etymology>
//
// <synopsis>
// The SpectralElement class is a container for a spectral line descriptor.
// It can contain a single line (like a Gaussian profile), or a set of
// related lines (like a doublet or so).
//
// The element can be used in the
// <ahlink class=SpectralFit>SpectralFit</ahlink> class and in the
// <ahlink class=SpectralEstimate>SpectralEstimate</ahlink> class.
//
// The default type is a Gaussian, defined as:
// <srcblock>
//	AMPL.exp[ -(x-CENTER)<sup>2</sup>/2 SIGMA<sup>2</sup>]
// </srcblock>
//
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a contained fitting of spectral profiles to an observed spectrum
// </motivation>
//
// <todo asof="2001/02/04">
//   <li> add more profile types
// </todo>

class SpectralElement {
 public:

  //# Enumerations
  // Types of spectral lines known
  enum Types {
    // A gaussian profile
    GAUSSIAN,
    // A polynomial baseline
    POLYNOMIAL,
    N_Types
  };

  //# Friends

  //# Constructors
  // Default constructor creates a default Gaussian element with an amplitude
  // of 1; a width (sigma) of 1; a central frequency of zero.
  SpectralElement();
  // Construct with given type and values
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  SpectralElement(SpectralElement::Types tp, const Double ampl,
		  const Double center, const Double sigma);
  // Construct an n-degree polynomial
  explicit SpectralElement(const uInt n);
  // Copy constructor (deep copy)
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  SpectralElement(const SpectralElement &other);

  //# Operators
  // Assignment (copy semantics)
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  SpectralElement &operator=(const SpectralElement &other);

  //# Member functions
  // Get all the types available as String and codes, and number available
  static const String *const allTypes(Int &nall,
				      const SpectralElement::Types *&typ);
  // Get a string from the type
  static const String &fromType(SpectralElement::Types tp);
  // Get a type from a (non-case sensitive; minimum match) String
  static Bool toType(SpectralElement::Types &tp,
		     const String &typName);

  // Get the data for this element
  // <group>
  // Get type of this element
  SpectralElement::Types getType() const { return tp_p; }
  // Get amplitude
  Double getAmpl() const { return ampl_p; }
  // Get center value
  Double getCenter() const { return center_p; }
  // Get the width
  // <group>
  Double getSigma() const { return sigma_p; }
  Double getFWHM() const;
  // </group>
  // Get the degree of e.g. polynomial
  uInt getDegree() const { return n_p; }
  // </group>

  // Set data for element
  // <group>
  // Set all data
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  void set(SpectralElement::Types tp, const Double ampl,
	   const Double center, const Double sigma);
  // Set type
  void setType(SpectralElement::Types tp);
  // Set amplitude
  void setAmpl(Double ampl);
  // Set center
  void setCenter(Double center);
  // Set width
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  void setSigma(Double sigma);
  // </group>
  // Set degree
  void setDegree(uInt n);

  //#Destructor
  // Destructor
  ~SpectralElement();

 private:
  //#Data
  // type of element
  SpectralElement::Types tp_p;
  // A number (like polynomial degree or number of doublet lines
  uInt n_p;
  // Amplitude
  Double ampl_p;
  // Center frequency
  Double center_p;
  // Width
  Double sigma_p;

  //# Member functions
  // Check for valid values: sigma should not be zero.
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  void SpectralElement::check();

};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
ostream &operator<<(ostream &os, const SpectralElement &elem);
// </group>

#endif


