//# MDirection.h: A Measure: astronomical direction
//# Copyright (C) 1995-2000,2002,2004,2007
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

#ifndef MEASURES_MDIRECTION_H
#define MEASURES_MDIRECTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVDirection.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MDirection;
class MCDirection;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;

//# Typedefs

// <summary>
//  A Measure: astronomical direction
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// MDirection forms a derived Measure class for a direction in space.<br>
// An MDirection can be generated from a
// <linkto class=MVDirection>MVDirection</linkto> or a pair of 
// <linkto class=Quantum>Quantities</linkto> specifying a longitudinal and a
// latitudinal angle.<br>
// The different reference types that can be used for a Direction are:
// <ul>
//  <li>MDirection::J2000 -- mean equator and equinox at J2000.0 (FK5)
//  <li>MDirection::JNAT --- geocentric natural frame
//  <li>MDirection::JMEAN -- mean equator and equinox at frame epoch
//  <li>MDirection::JTRUE -- true equator and equinox at frame epoch
//  <li>MDirection::APP ---- apparent geocentric position
//  <li>MDirection::B1950 -- mean epoch and ecliptic at B1950.0. The epoch
//	is taken from the frame epoch; or from the aipsrc variable
//	measures.b1950.d_epoch; or has default 2000.0
//  <li>MDirection::B1950_VLA -- mean epoch(1979.9)) and ecliptic at B1950.0
//  <li>MDirection::BMEAN -- mean equator and equinox at frame epoch
//  <li>MDirection::BTRUE -- true equator and equinox at frame epoch
//  <li>MDirection::GALACTIC -- galactic coordinates
//  <li>MDirection::HADEC -- topocentric HA and declination
//  <li>MDirection::AZEL --- topocentric Azimuth and Elevation (N through E)
//  <li>MDirection::AZELSW - topocentric Azimuth and Elevation (S through W)
//  <li>MDirection::AZELNE - topocentric Azimuth and Elevation (N through E)
//  <li>MDirection::AZELGEO --- geodetic Azimuth and Elevation (N through E)   
//  <li>MDirection::AZELSWGEO - geodetic Azimuth and Elevation (S through W)   
//  <li>MDirection::AZELNEGEO - geodetic Azimuth and Elevation (N through E)   
//  <li>MDirection::ECLIPTC   -- ecliptic for J2000 equator and equinox
//  <li>MDirection::MECLIPTIC -- ecliptic for mean equator of date
//  <li>MDirection::TECLIPTIC -- ecliptic for true equator of date
//  <li>MDirection::SUPERGAL -- supergalactic coordinates
//  <li>MDirection::ITRF -- coordinates wrt ITRF Earth frame
//  <li>MDirection::TOPO -- apparent topocentric position
//  <li>MDirection::ICRS -- International Celestial reference system
//  <li>MDirection::MERCURY -- the planet: has no data attached
//  <li>MDirection::VENUS
//  <li>MDirection::MARS
//  <li>MDirection::JUPITER
//  <li>MDirection::SATURN
//  <li>MDirection::URANUS
//  <li>MDirection::NEPTUNE
//  <li>MDirection::PLUTO
//  <li>MDirection::SUN
//  <li>MDirection::MOON
//  <li>MDirection::COMET  -- solar system body: no coordinates  attached,
//			 only table
//  <li>MDirection::DEFAULT = J2000
// </ul>
// <p>
// Conversion between the different types is done with the standard
// <linkto class=MeasConvert>MeasConvert</linkto> class 
// (<src>MDirection::Convert</src> in this case).<br>
// For some conversion additional <linkto class=MeasFrame>MeasFrame</linkto>
// information is essential. The following list specifies which information
// is needed if the conversion goes to or from the different types:
// <ul>
//   <li><em>Epoch</em>: all but J2000, B1950, GALACTIC, SUPGAL, ECLIPTIC, ICRS
//   <li><em>Positiom</em>: HADEC, AZEL, AZELGEO
// </ul>
// The conversion between B1950 and J2000 may have an Epoch. If none given
// an epoch of 2000.0 is assumed for the conversion, unless an aipsrc
// variable <em>measures.b1950.d_epoch</em> is given.
//
// Conversions are based on the IAU system of 
// <linkto class=Precession>precession</linkto> and 
// <linkto class=Nutation>nutation</linkto> (with
// IERS corrections if available); and on series expansions of the DE200
// planetary ephemeris (J system; for B sytem older expansions) for the
// <linkto class=Aberration>aberration</linkto> and the 
// <linkto class=SolarPos>solar position</linkto>.<br>
// The <em>HADEC</em> position has corrections for polar motion and the
// equation of equinoxes; the <em>AZEL</em> will include Earth tides and
// refraction at a later stage.<br>
// Note that conversion between B1950 and J2000 can only be approximate, and is
// based on FK4 to FK5 conversion. The best conversion is to convert first
// to an apparent position at the time of observation, and convert from there
// to the other standard (the correct route will be followed).<br>
// Another problem can arise if the source has proper motion and/or radial
// velocities. These should be taken into account. An
// MCatalog class will maybe take care of that.
// <note role=warning>
// The offset that can be specified in the MDirection::Ref is an MDirection
// offset, and can not be used for specifying angular offsets. shift()
// methods are available for these cases.
// </note>
// <p>
// To aid in formatting of the angles without having to check all difference
// referencetypes, the following global types are provided:
// <ul>
//  <li> GRADEC for types that are probably expressed in HM,DM
//  <li> GHADEC for types that are probably expressed in +-HM,DM
//  <li> GAZEL for types that are probably expressed in +-deg,deg
//  <li> GLONGLAT for types that are probably expressed in deg,deg
// </ul>
// they can be obtained with the globalType() method. 
// </synopsis>
//
// <example>
// See <linkto module=Measures>Measures</linkto> module description for
// extensive examples.
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2000/06/15">
//	<li>
// </todo>

class MDirection : public MeasBase<MVDirection, MeasRef<MDirection> > { 

public:
//# Friends
// Conversion of data
    friend class MeasConvert<MDirection>;

//# Enumerations
// Types of known MDirections
// <note role=warning> The order defines the order in the translation matrix FromTo
// in the getConvert routine. Do not change the order without
// changing the array. Additions should be made before N_types, and
// an additional row and column should be coded in FromTo, and
// in showType().</note>
    enum Types {
      J2000,
      JMEAN,
      JTRUE,
      APP,
      B1950,
      B1950_VLA,
      BMEAN,
      BTRUE,
      GALACTIC,
      HADEC,
      AZEL,
      AZELSW,
      AZELGEO,
      AZELSWGEO,
      JNAT,
      ECLIPTIC,
      MECLIPTIC,
      TECLIPTIC,
      SUPERGAL,
      ITRF,
      TOPO,
      ICRS,
      N_Types,
      // Planets. First one should be Mercury
      MERCURY = 32,
      VENUS,
      MARS,
      JUPITER,
      SATURN,
      URANUS,
      NEPTUNE,
      PLUTO,
      SUN,
      MOON,
      // Comet or other table-described solar system body
      COMET,
      N_Planets,
      // All extra bits
      EXTRA = 32,
      // Defaults
      DEFAULT=J2000,
      // Synonyms
      AZELNE=AZEL,
      AZELNEGEO=AZELGEO };
  // Global types
  enum GlobalTypes {
    GRADEC,
    GHADEC,
    GAZEL,
    GLONGLAT,
    N_GTypes};

  //# Typedefs
  // Measure value container for this class (i.e. MDirection::MVType)
  typedef MVDirection MVType;
  // Measure conversion routines for this class (i.e. MDirection::MCType)
  typedef MCDirection MCType;
  // Measure reference (i.e. MDirection::Ref)
  typedef MeasRef<MDirection> Ref;
  // Measure Convert (i.e. MDirection::Convert)
  typedef MeasConvert<MDirection> Convert;
  // Measure table Columns (e.g., MDirection::ScalarColumn)
  typedef ScalarMeasColumn<MDirection> ScalarColumn;
  typedef ArrayMeasColumn<MDirection> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(MDirection) Types Types;

//# Constructors
// <note role=tip> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. </note>
// Default constructor; generates the J2000 pole direction
    MDirection();
// Create from data and reference
// <group>
    MDirection(const MVDirection &dt);
    MDirection(const MVDirection &dt, const MDirection::Ref &rf);
    MDirection(const MVDirection &dt, MDirection::Types rf);
    MDirection(const Quantity &dt, const Quantity &dt1);
    MDirection(const Quantity &dt, const Quantity &dt1, 
	       const MDirection::Ref &rf);
    MDirection(const Quantity &dt, const Quantity &dt1, 
	       MDirection::Types rf);
    MDirection(const Quantum<Vector<Double> > &dt);
    MDirection(const Quantum<Vector<Double> > &dt, 
	       const MDirection::Ref &rf);
    MDirection(const Quantum<Vector<Double> > &dt, 
	       MDirection::Types rf);
    MDirection(const Measure *dt);
    MDirection(const MeasValue *dt);
    MDirection(const MDirection::Ref &rf);
    MDirection(MDirection::Types rf);
// </group>

//# Destructor
    virtual ~MDirection();

// Make an MDirection object given the name of a moving source (SUN, etc.)
// or of a known standard source (CygA, etc.).
    static MDirection makeMDirection(const String& sourceName);

//# Operators

//# General Member Functions
// Tell me your type ('Direction')
// <group>
    virtual const String &tellMe() const;
    static const String &showMe();
// </group>
// Tell me your reference type (as Register())
  // N.B. as defined in MDirection.cc, it does NOT return the type of an
  // instance, i.e. it just returns Register(static_cast<MDirection *>(0)).
    virtual uInt type() const;
// Assert you are a direction
    static void assure(const Measure &in);
// Tell me the global type (like GRADEC) for tp (tp like MDirection::J2000)
  static MDirection::GlobalTypes globalType(uInt tp);
// Translate reference code tp. The uInt version has a check for valid codes
  // (i.e. it is a safe cast).
  // <thrown>
  //   <li> AipsError in the uInt interface if illegal code given
  // </thrown>
  // <group>
  static MDirection::Types castType(uInt tp);
  static const String &showType(MDirection::Types tp);
  static const String &showType(uInt tp);
  // </group>
// Translate string to reference code
// <group>
  static Bool getType(MDirection::Types &tp, const String &in);
  Bool giveMe(MDirection::Ref &mr, const String &in);
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
  // N.B. Being static, it does NOT return the type of an instance, i.e. use it
  // as MDirection::myType(), not md.myType().
  static uInt myType();
  // Tell me if you are a pure model (e.g. a planet)
  virtual Bool isModel() const;

  // Get Measure data
  // <group>
  Quantum<Vector<Double> > getAngle() const;
  Quantum<Vector<Double> > getAngle(const Unit &inunit) const;
  // </group>
  // Shift the direction in longitude (radians if Double) and/or latitude.
  // If the trueAngle switch is True, the longitude shift will be in
  // angular units perpendicular to the direction to pole, along a great
  // circle. See <linkto class=MVDirection>MVDirection</linkto>
  // for more details.
  // <group>
  void shift(const Quantum<Double> &lng,
	     const Quantum<Double> &lat, Bool trueAngle=False);
  void shift(Double lng, Double lat, Bool trueAngle=False);
  void shiftLongitude(const Quantity &lng, Bool trueAngle=False);
  void shiftLongitude(Double lng, Bool trueAngle=False);
  void shiftLatitude(const Quantum<Double> &lat, Bool trueAngle=False);
  void shiftLatitude(Double lat, Bool trueAngle=False);
  void shift(const MVDirection &shft, Bool trueAngle=False);
  // </group>
  // Shift over an angle off in the direction pa. pa is measured from North,
  // in the direction of increasing longitude.
  // See <linkto class=MVDirection>MVDirection</linkto>
  // for implementation.
  // <group>
  void shiftAngle(const Quantum<Double> &off,
		  const Quantum<Double> &pa);
  void shiftAngle(Double off, Double pa);
  // </group>

// Make a copy
// <group>
    virtual Measure *clone() const;
// </group>

    // Convert to a String in astronomer-friendly format based on
    // reference frame
    String toString() const;

private:

//# Data

//# Member functions

};


} //# NAMESPACE CASACORE - END

#endif

