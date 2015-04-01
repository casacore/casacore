//# MFrequency.h: A Measure: wave characteristics
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef MEASURES_MFREQUENCY_H
#define MEASURES_MFREQUENCY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVFrequency.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MFrequency;
class MCFrequency;
class MDoppler;
class MVDoppler;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;

//# Typedefs

// <summary>
// A Measure: wave characteristics
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
//   <li> <linkto class = MRadialVelocity>MRadialVelocity</linkto> class
//		for some other background.
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// MFrequency is a derived Measure class for wave characteristics.<br>
// An MFrequency can be generated from a simple value (or an
// <linkto class=MVFrequency>MFrequency</linkto> object), which is then
// interpreted as a frequency in Hz, and a reference, with an LSRK type
// as default.<br>
// It can also be generated from a Quantity, where the interpretation
// depends on the dimensionality of the Quantity:
// <ul>
// <li> time (e.g. s): period
// <li> frequency (e.g. Hz): frequency
// <li> angular frequency (e.g. arcmin/s): angular frequency
// <li> length (e.g. cm): wavelength
// <li> inverse length (e.g. mm<sup>-1</sup>): wave number
// <li> energy (e.g. J.s): energy (i.e. <em>h.nu</em>)
// <li> momentum (e.g. kg.m): <em>m.c/h</em>
// </ul>
// The different reference types of a frequency are:
// <ul>
//  <li> MFrequency::REST -- Rest frequency
//  <li> MFrequency::LSRD -- Local Standard of Rest (J2000) -- as the
//		dynamical definition (IAU, [9,12,7] km/s in galactic
//		coordinates)
//  <li> MFrequency::LSRK -- LSR as a kinematical (radio) definition --
//		20.0 km/s in direction ra,dec = [270,+30] deg (B1900.0)
//  <li> MFrequency::BARY -- Barycentric (J2000)
//  <li> MFrequency::GEO --- Geocentric
//  <li> MFrequency::TOPO -- Topocentric
//  <li> MFrequency::GALACTO -- Galacto centric (with rotation of 220 km/s
//		in direction l,b = [90,0] deg.
//  <li> MFrequency::LGROUP -- Local group velocity -- 308km/s towards
//		 l,b = [105,-7] deg (F. Ghigo)
//  <li> MFrequency::CMB -- CMB velocity -- 369.5km/s towards
//		l,b = [264.4, 48.4] deg (F. Ghigo)
//  <li> MFrequency::DEFAULT = LSRK
// </ul>
// <p>
// Conversion between the different types is done with the standard
// <linkto class=MeasConvert>MeasConvert</linkto> class 
// (<src>MFrequency::Convert</src> in this case). 
// Some of the conversions are only possible if frame information has been 
// filled in. The following frame information is necessary if a conversion
// goes to or from the (different) specified types:
// <ul>
//  <li><em>Radial Velocity</em>: REST
//  <li><em>Epoch</em>: TOPO, GEO
//  <li><em>Position</em>: TOPO
//  <li><em>Direction</em> all
// </ul>
// <br>
// To accommodate unknown or invalid frames, the additional reference type
// <ul>
//  <li> MFrequency::Undefined
// </ul>
// is available. Conversions to/from Undefined are not possible. 
// If attempted, an exception will be thrown.
// The name was chosen to be Undefined and not UNDEFINED in order to
// not collide with the (ugly) WCSLIB macro of the upper case name
// and in concordance with Stokes::Undefined.
// <br>
// An MFrequency can be created from an
// <linkto class=MDoppler>MDoppler</linkto> (and a rest frequency, (the
// <linkto class=QC>QC</linkto> class contains at least <src>QC::HI</src>))
// by the <src>fromDoppler()</src> member. It can be converted to an MDoppler
// with the <src>toDoppler()</src>. Comparable methods will be available
// for <linkto class=MFrequency>MFrequency</linkto> as 
// <src>toRadial()</src> and <src>fromRadial</src>.<br>
// If the Doppler shift is known (e.g. from another spectral line), the
// REST frequency can be determined with the <src>toREST()</src> member.
// <note role=caution> Conversion between the different frequencies can,
// due to relativistic effects, only be done approximately for very high
// (order c) radial velocities (shifted frequencies). A better approach
// would be to start from radial velocities and a rest frequency.
// </note>
// </synopsis>
//
// <example>
// Get the Doppler shift for an oberved HI frequency of 1380 MHz
// <srcblock>
//	cout << "Redshift for 1380 MHz: " <<
//		MDoppler::Convert( MFrequency( Quantity(1380., "MHz"),
//					       MFrequency::TOPO).toDoppler(QC::HI),
//				   MDoppler::Z)() << endl;
// </srcblock>				   
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2003/03/03">
// </todo>

class MFrequency : public MeasBase<MVFrequency, MeasRef<MFrequency> > {

 public:
  //# Friends
  // Conversion of data
  friend class MeasConvert<MFrequency>;

  //# Enumerations
  // Types of known MFrequencies
  // <note role=warning> The order defines the order in the translation
  // matrix FromTo
  // in the getConvert routine. Do not change the order without
  // changing the array. Additions should be made before N_types, and
  // an additional row and column should be coded in FromTo, and
  // in showType().</note>
  enum Types {
    REST,
    LSRK,
    LSRD, 
    BARY,
    GEO,
    TOPO,
    GALACTO,
    LGROUP,
    CMB,
    N_Types,
    Undefined = 64,
    N_Other,
    // all extra bits
    EXTRA = 64,
    // Defaults
    DEFAULT=LSRK,
    // Synonyms
    LSR=LSRK };

  //# Typedefs
  // Measure value container for this class (i.e. MFrequency::MVType)
  typedef MVFrequency MVType;
  // Measure conversion routines for this class (i.e. MFrequency::MCType)
  typedef MCFrequency MCType;
  // Measure reference (i.e. MFrequency::Ref)
  typedef MeasRef<MFrequency> Ref;
  // Measure conversion use (i.e. MFrequency::Convert)
  typedef MeasConvert<MFrequency> Convert;
  // Measure table Columns (e.g., MFrequency::ScalarColumn)
  typedef ScalarMeasColumn<MFrequency> ScalarColumn;
  typedef ArrayMeasColumn<MFrequency> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(MFrequency) Types Types;

  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. </note>
  // Default constructor; generates a zero rest frequency
  MFrequency();
  // Create from data and reference
  // <group>
  MFrequency(const MVFrequency &dt);
  MFrequency(const MVFrequency &dt, const MFrequency::Ref &rf);
  MFrequency(const MVFrequency &dt, MFrequency::Types rf);
  MFrequency(const Quantity &dt);
  MFrequency(const Quantity &dt, const MFrequency::Ref &rf);
  MFrequency(const Quantity &dt, MFrequency::Types rf);
  MFrequency(const Measure *dt);
  MFrequency(const MeasValue *dt);
  // </group>
  
  //# Destructor
  virtual ~MFrequency();
  
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
  static MFrequency::Types castType(uInt tp);
  static const String &showType(MFrequency::Types tp);
  static const String &showType(uInt tp);
  // </group>
  // Translate string to reference code
  // <group>
  static Bool getType(MFrequency::Types &tp, const String &in);

  // Throws an exception if the type string is not recognized
  static MFrequency::Types typeFromString(const String& in);


  Bool giveMe(MFrequency::Ref &mr, const String &in);
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
  
  // Get frequency in specified units
  Quantity get(const Unit &un) const;
  
  // Make a Doppler velocity from the frequency and the specified rest frequency
  // <group>
  MDoppler toDoppler(const MVFrequency &rest);
  MDoppler toDoppler(const MVFrequency &rest) const;
  // </group>
  // Local use only
  static MDoppler toDoppler(const Measure &in, const MVFrequency &rest);
  // Make a frequency from the Doppler velocity and the specified rest frequency
  // (default reference type LSRK)
  // <group>
  static MFrequency fromDoppler(const MDoppler &dop,
				const MVFrequency &rest);
  static MFrequency fromDoppler(const MDoppler &dop,
				const MVFrequency &rest,
				MFrequency::Types type);
  // For internal use only
  static MFrequency fromDoppler(const Measure &dop,
				const MVFrequency &rest,
				MFrequency::Types type);
  // </group>
  
  // Make a rest frequency using a Doppler velocity
  MFrequency toRest(const MDoppler &dop);
  // For local use only
  static MFrequency toRest(const Measure &in, const Measure &dop);
  
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
