//# MDoppler.h: A Measure: Doppler shift
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2002,2003
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

#ifndef MEASURES_MDOPPLER_H
#define MEASURES_MDOPPLER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVDoppler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MDoppler;
class MCDoppler;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;
template <class T> class Vector;
template <class T> class Quantum;

//# Typedefs

// <summary>
// A Measure: Doppler shift
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// From Measure and Doppler
// </etymology>
//
// <synopsis>
// MDoppler forms the derived Measure class for Doppler shifts.<br>
// An MDoppler can be generated from a simple value (or an
// <linkto class=MVDoppler>MVDoppler</linkto>), which is then
// interpreted as a Doppler ratio, and a reference, with a RADIO type
// as default.<br>
// It can also be generated from a Quantity, where the interpretation
// depends on the dimensionality of the Quantity:
// <ul>
// <li> None: a Doppler ratio
// <li> Velocity: Doppler ratio calculated by dividing with c
// </ul>
// The different types of Doppler (with F = f/f0, the frequency ratio),
// are:
// <ul>
// <li> MDoppler::Z		(-1 + 1/F)
// <li> MDoppler::RATIO		(F)				*
// <li> MDoppler::RADIO		(1  - F)
// <li> MDoppler::OPTICAL == Z
// <li> MDoppler::BETA  	((1 - F<sup>2</sup>)/(1 + F<sup>2</sup>))
// <li> MDoppler::GAMMA  	((1 + F<sup>2</sup>)/2F)	*
// <li> MDoppler::RELATIVISTIC == BETA (== v/c)
// <li> MDoppler::DEFAULT == RADIO
// </ul>
// Note that the ones with an '*' have no real interpretation (although the
// calculation will proceed) if given as
// a velocity.<br>
// <p>
// Conversion between the different types is done with the standard
// <linkto class=MeasConvert>MeasConvert</linkto> class 
// (<src>MDoppler::Convert</src> in this case).<br>
// 
// Dopplers can be created from an <linkto class=MFrequency>MFrequency</linkto>
// object, or from an <linkto class=MRadialVelocity>MRadialVelocity</linkto>
// object.<br>
//
// A <em>shiftFrequency()</em> method can shift frequencies.
//
// Dopplers do not need a reference frame.
//
// </synopsis>
//
// <example>
// Conversion of a radio Doppler to an optical
// <srcblock>
//	MDoppler radio(0.01);		// A radio Doppler value
//	cout << "Doppler radio = " << radio << "; optical = " <<
//		MDoppler::Convert(radio, MDoppler::OPTICAL)() << // Convert 
//		endl;
// </srcblock>
// Setting up a conversion
// <srcblock>
//	MDoppler::Convert to_opt(MDoppler::RADIO, MDoppler::OPTICAL);
//	for (Double d=0; d<0.1; d += 0.005) {
//		cout << "radio = " << d << " to optical = " <<
//			to_opt(d) << endl;
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2000/06/15">
//	<li>
// </todo>

class MDoppler : public MeasBase<MVDoppler, MeasRef<MDoppler> > {

 public:
  //# Friends
  // Conversion of data
  friend class MeasConvert<MDoppler>;
  
  //# Enumerations
  // Types of known MDopplers
  // <note role=warning> The order defines the order in the translation
  // matrix FromTo
  // in the getConvert routine. Do not change the order without
  // changing the array. Additions should be made before N_types, and
  // an additional row and column should be coded in FromTo, and
  // in showType().</note>
  enum Types {
    RADIO, 
    Z,
    RATIO,
    BETA,
    GAMMA,
    N_Types,
    OPTICAL=Z,
    RELATIVISTIC=BETA,
    DEFAULT=RADIO };
  
  //# Typedefs
  // Measure value container for this class (i.e. MDoppler::MVType)
  typedef MVDoppler MVType;
  // Measure conversion routines for this class (i.e. MDoppler::MCType)
  typedef MCDoppler MCType;
  // Measure reference (i.e. MDoppler::Ref)
  typedef MeasRef<MDoppler> Ref;
  // Measure Convert (i.e. MDoppler::Convert)
  typedef MeasConvert<MDoppler> Convert;
  // Measure table Columns (e.g., MDoppler::ScalarColumn)
  typedef ScalarMeasColumn<MDoppler> ScalarColumn;
  typedef ArrayMeasColumn<MDoppler> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(MDoppler) Types Types;

  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. </note>
  // Default constructor; generates a zero rest Doppler
  MDoppler();
  // Create from data and reference
  // <group>
  MDoppler(const MVDoppler &dt);
  MDoppler(const MVDoppler &dt, const MDoppler::Ref &rf);
  MDoppler(const MVDoppler &dt, MDoppler::Types rf);
  MDoppler(const Quantity &dt);
  MDoppler(const Quantity &dt, const MDoppler::Ref &rf);
  MDoppler(const Quantity &dt, MDoppler::Types rf);
  MDoppler(const Measure *dt);
  MDoppler(const MeasValue *dt);
  // </group>

  //# Destructor
  virtual ~MDoppler();
  
  //# Operators
  
  //# General Member Functions
  // Tell me your type
  // <group>
  virtual const String &tellMe() const;
  static const String &showMe();
  virtual uInt type() const;
  static void assure(const Measure &in);
  // </group>
  // Translate reference code. The uInt version has a check for valid codes
  // (i.e. it is a safe cast).
  // <thrown>
  //   <li> AipsError in the uInt interface if illegal code given
  // </thrown>
  // <group>
  static MDoppler::Types castType(uInt tp);
  static const String &showType(MDoppler::Types tp);
  static const String &showType(uInt tp);
  // </group>
  // Translate string to reference code
  // <group>
  static Bool getType(MDoppler::Types &tp, const String &in);
  Bool giveMe(MDoppler::Ref &mr, const String &in);
  // </group>
  // Set the offset in the reference (False if non-matching Measure)
  virtual Bool setOffset(const Measure &in);
  // Set the reference type to the specified String. False if illegal
  // string, reference set to DEFAULT.
  virtual Bool setRefString(const String &in);
  // Get the default reference type
  virtual const String &getDefaultType() const;
  // Get a list of all known reference codes. nall returns the number in list,
  // nextra the number of specials (like planets) that should be at 
  // end of list). typ returns the list of corresponding types.
  // <group>
  virtual const String* allTypes(Int &nall, Int &nextra,
                                 const uInt *&typ) const;
  static const String* allMyTypes(Int &nall, Int &nextra,
                                  const uInt *&typ);
  // </group>
  // Check if all internal tables of types (both enum and String) are 
  // complete and correct. This function is called automatically if and when
  // necessary.
  // <thrown>
  //   <li> AipsError if a (programming) error in the types.
  // </thrown>
  // <group> 
  virtual void checkTypes() const;
  static void checkMyTypes();
  // </group>
  // Get the reference type (for records, including codes like R_)
  virtual String getRefString() const;
  // Get my type (as Register)
  static uInt myType();

  // Get in specified units
  Quantity get(const Unit &un) const;

  // Shift the input frequencies to the output frequencies. In the case of
  // simple Double inputs, it is assumed that the values are linearly dependent
  // on frequency. I.e. frequencies given as wavelength or time cannot be used.
  // <group>
  Vector<Double> shiftFrequency(const Vector<Double> &freq) const;
  Quantum<Vector<Double> >
    shiftFrequency(const Quantum<Vector<Double> > &freq) const;
  // </group>

  // Make a copy
  // <group>
  virtual Measure *clone() const;
  // </group>
  
 private:
  //# Enumerations
  
  //# Data
  
  //# Member functions


};


} //# NAMESPACE CASACORE - END

#endif
