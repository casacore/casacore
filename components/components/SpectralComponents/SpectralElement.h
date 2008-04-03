//# SpectralElement.h: Describes (a set of related) spectral lines
//# Copyright (C) 2001,2003,2004
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

#ifndef COMPONENTS_SPECTRALELEMENT_H
#define COMPONENTS_SPECTRALELEMENT_H

//# Includes
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <casa/Utilities/RecordTransformable.h>
#include <casa/BasicSL/String.h>
#include <casa/iosfwd.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Describes (a set of related) spectral lines
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Functionals>Functionals</linkto> module
//   <li> <linkto class=RecordInterface>RecordInterface</linkto> class
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
// <linkto class=SpectralFit>SpectralFit</linkto> class and in the
// <linkto class=SpectralEstimate>SpectralEstimate</linkto> class.
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
// To have a container for fitting of spectral profiles to an observed spectrum
// </motivation>
//
// <todo asof="2001/02/04">
//   <li> add more profile types
// </todo>

class SpectralElement : public RecordTransformable {
 public:

  //# Enumerations
  // Types of spectral lines known
  enum Types {
    // A gaussian profile
    GAUSSIAN,
    // A polynomial baseline
    POLYNOMIAL,
    // Any compiled string functional
    COMPILED,
    N_Types
  };

  //# Constants
  // Sigma to FWHM conversion factor
  static const Double SigmaToFWHM;

  //# Constructors
  // Default constructor creates a default Gaussian element with an amplitude
  // of 1; an integral <src>(sigma=2sqrt(ln2)/pi)</src> of 1;
  // a central frequency of zero.
  SpectralElement();
  // Construct with given type and values
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  //   <li> AipsError if type not GAUSSIAN
  // </thrown>
  SpectralElement(SpectralElement::Types tp, const Double ampl,
		  const Double center, const Double sigma);
  // Construct an n-degree polynomial
  explicit SpectralElement(const uInt n);
  // Construct a compiled string
  explicit SpectralElement(const String &str, const Vector<Double> &param);
  // Construct the given tp with the given param
  // <thrown>
  //   <li> AipsError if incorrect number of parameters (e.g. not 3 for GAUSSIAN)
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  SpectralElement(SpectralElement::Types tp, const Vector<Double> &param);
  // Copy constructor (deep copy)
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  SpectralElement(const SpectralElement &other);

  //#Destructor
  // Destructor
  ~SpectralElement();

  //# Operators
  // Assignment (copy semantics)
  // <thrown>
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  SpectralElement &operator=(const SpectralElement &other);
  // Evaluate the value of the element at x
  Double operator()(const Double x) const;
  // Get parameter n
  // <thrown>
  //  <li> AipsError if illegal n
  // </thrown>
  Double operator[](const uInt n) const;

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
  // <thrown>
  //  <li> AipsError if element does not have data
  //	   (e.g. amplitude for POLYNOMIAL)
  // </thrown>
  // <group>
  // Get type of this element
  SpectralElement::Types getType() const { return tp_p; }
  // Get amplitude
  Double getAmpl() const;
  // Get center value
  Double getCenter() const;
  // Get the width
  // <group>
  Double getSigma() const;
  Double getFWHM() const;
  // </group>
  // Get all parameters
  void get(Vector<Double> &param) const;
  // Get amplitude error estimate
  Double getAmplErr() const;
  // Get center value error estimate
  Double getCenterErr() const;
  // Get the width error estimate
  // <group>
  Double getSigmaErr() const;
  Double getFWHMErr() const;
  // </group>
  // Get error estimates of parameters
  void getError(Vector<Double> &err) const;
  // Get the degree of e.g. polynomial
  uInt getDegree() const;
  // Get the string of a compiled functional
  const String &getCompiled() const;
  // </group>
  // Get the order (i.e. the number of parameters)
  uInt getOrder() const { return par_p.nelements(); };

  // Set data for element
  // <group>
  // Set all data
  // <thrown>
  //   <li> AipsError if incorrect number of parameters (e.g. not 3 for GAUSSIAN)
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  // <group>
  // Reset a complete element
  // <group>
  template <class MT>
    void set(SpectralElement::Types tp, const Vector<MT> &param);
  // </group>
  // Reset the parameter values only (i.e zero errors and nothing fixed)
  // <group>
  template <class MT>
    void set(const Vector<MT> &param);
  // </group>
  // </group>
  // Set the error fields
  void setError(const Vector<Double> &err);
  // Set amplitude
  // <thrown>
  //   <li> AipsError if non GAUSSIAN
  // </thrown>
  void setAmpl(Double ampl);
  // Set center
  // <thrown>
  //   <li> AipsError if non GAUSSIAN
  // </thrown>
  void setCenter(Double center);
  // Set width
  // <thrown>
  //   <li> AipsError if non GAUSSIAN
  //   <li> AipsError if sigma == 0.0
  // </thrown>
  // <group>
  void setSigma(Double sigma);
  void setFWHM(Double fwhm);
  // </group>
  // </group>
  // Set degree
  // <thrown>
  //   <li> AipsError if non POLYNOMIAL
  // </thrown>
  void setDegree(uInt n);
  // Set a new compiled string
  // <thrown>
  //   <li> AipsError if non COMPILED and illegal string
  // </thrown>
  void setCompiled(const String &str);

  // Set fixed parameters (True) or unset them (False)
  // <thrown>
  //   <li> AipsError if incorrect number of parameters (e.g. not 3 for GAUSSIAN)
  // </thrown>
  // <group>
  void fixAmpl(const Bool fix=True);
  void fixCenter(const Bool fix=True);
  void fixSigma(const Bool fix=True);
  void fixFWHM(const Bool fix=True);
  // Fix/unfix all in one go
  void fix(const Vector<Bool> &fix);
  // </group>

  // Get the fix state[s]
  // <thrown>
  //   <li> AipsError if incorrect number of parameters (e.g. not 3 for GAUSSIAN)
  // </thrown>
  // <group>
  Bool fixedAmpl() const;
  Bool fixedCenter() const;
  Bool fixedSigma() const;
  Bool fixedFWHM() const;
  const Vector<Bool> &fixed() const;
  // </group>

  // Construct from record.  Must hold fields "type" (String) and 
  // "parameters" (Vector<Double>).  For type=GAUSSIAN, parameters
  // holds amplitude, center and sigma. For type=POLYNOMIAL,
  // parameters(0) holds the degree.
  static SpectralElement* fromRecord(const RecordInterface &container);

  // Create a SpectralElement from a record.
  // An error message is generated, and False
  // returned if an invalid record is given. A valid record will return True.
  // A valid record contains the following fields (any additional fields are
  // ignored):
  // <ul>
  // <li> type = TpString: type of element (gaussian etc; case
  //	 insensitive)
  // <li> parameters = TpVector(Double): one or more values giving the parameters
  //		for the type
  // </ul>
  // A SpectralElement can be created from a string. In that case the string
  // will only indicate the type of element (like gaussian), and will
  // create a default element of that given type.
  // Error messages are postfixed to error.
  // <group>
  virtual Bool fromRecord(String &error, const RecordInterface &in);
  virtual Bool fromString(String &error, const String &in);
  // </group>

  // Save to a record.  The return will be False and an error
  // message generated only if the SpectralElement is illegal (could not happen)
  // Error messages are postfixed to error.   For Gaussian elements,
  // the width is defined as a FWHM in the record interface.
  virtual Bool toRecord(String &error, RecordInterface &out) const;

  // Get the identification of a record
  virtual const String &ident() const;

 private:
  //#Data
  // type of element
  SpectralElement::Types tp_p;
  // A number (like polynomial degree or number of doublet lines)
  uInt n_p;
  // The string value for compiled functional
  String str_p;
  // The parameters of the function. I.e. the polynomial coefficients;
  // amplitude, center and sigma of a Gaussian.
  Vector<Double> par_p;
  // The errors of the parameters
  Vector<Double> err_p;
  // The indication if the parameter has to be fixed (True) or solved (False).
  // Solved is the default.
  Vector<Bool> fix_p;

  //# Member functions
  // Check if GAUSSIAN type
  // <thrown>
  //   <li> AipsError if non-Gaussian
  // </thrown>
  void checkGauss() const;
  // Check if POLYNOMIAL type
  // <thrown>
  //   <li> AipsError if non-polynomial
  // </thrown>
  void checkPoly() const;
  // Check if COMPILED type
  // <thrown>
  //   <li> AipsError if non-compiled
  // </thrown>
  void checkCompiled() const;
  // Check if sigma non-equal to zero and positive if a GAUSSIAN;
  // if COMPILED check for correct string
  // <thrown>
  //   <li> AipsError if illegal sigm
  // </thrown>
  void check();

  // Sigma to FWHM
  // Convert from sigma to FWHM and vice versa
  // <group>
  static Double sigmaToFWHM (const Double sigma);
  static Double sigmaFromFWHM (const Double fwhm);
  // </group>


};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
ostream &operator<<(ostream &os, const SpectralElement &elem);
// </group>


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <components/SpectralComponents/Spectral4Element.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
