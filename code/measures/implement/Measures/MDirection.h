//# MDirection.h: A Measure: astronomical direction
//# Copyright (C) 1995,1996,1997,1998
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

#if !defined(AIPS_MDIRECTION_H)
#define AIPS_MDIRECTION_H

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MVDirection.h>

//# Forward Declarations
class MDirection;
class MCDirection;
template <class M, class F, class MC> class MeasConvert;

//# Typedefs

// <summary>
//  A Measure: astronomical direction
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
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
//  <li>MDirection::J2000 -- mean equator and equinox at J2000.0
//  <li>MDirection::JNAT --- geocentric natural frame
//  <li>MDirection::JMEAN -- mean equator and equinox at frame epoch
//  <li>MDirection::JTRUE -- true equator and equinox at frame epoch
//  <li>MDirection::APP ---- apparent geocentric position
//  <li>MDirection::B1950 -- mean epoch and ecliptic at B1950.0
//  <li>MDirection::BMEAN -- mean equator and equinox at frame epoch
//  <li>MDirection::BTRUE -- true equator and equinox at frame epoch
//  <li>MDirection::GALACTIC -- galactic coordinates
//  <li>MDirection::HADEC -- topocentric HA and declination
//  <li>MDirection::AZEL --- topocentric Azimuth and Elevation (N through E)
//  <li>MDirection::AZELSW - topocentric Azimuth and Elevation (S through W)
//  <li>MDirection::AZELNE - topocentric Azimuth and Elevation (N through E)
//  <li>MDirection::ECLIPTC   -- ecliptic for J2000 equator and equinox
//  <li>MDirection::MECLIPTIC -- ecliptic for mean equator of date
//  <li>MDirection::TECLIPTIC -- ecliptic for true equator of date
//  <li>MDirection::SUPERGAL -- supergalactic coordinates
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
//   <li><em>Epoch</em>: all but J2000, B1950, GALACTIC, SUPGAL, ECLIPTIC
//   <li><em>Positiom</em>: HADEC, AZEL
// </ul>
// Conversions are based on the IAU system of 
// <linkto class=Precession>precession</linkto> and 
// <linkto class=Nutation>nutation</linkto> (with
// IERS corrections if available); and on series expansions of the DE200
// planetary ephemeris (J system; for B sytem older expansions) for the
// <linkto class=Aberration>aberration</linkto> and the 
// <linkto class=SolarPos>solar position</linkto>.<br>
// The <em>APP</em> position has corrections for polar motion and the
// equation of equinoxes; the <em>AZEL</em> will include Earth tides and
// refraction at a later stage.<br>
// Note that conversion between B1950 and J2000 can only be approximate, and is
// based on FK4 to FK% conversion. The best conversion is to convert first
// to an apparent position at the time of observation, and convert from there
// to the other standard (the correct route will be followed).<br>
// Another problem can arise if the source has proper motion and/or radial
// velocities. These should be taken into account. An
// MCatalog class will maybe take care of that.
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
// <todo asof="1996/02/21">
//	<li>
// </todo>

class MDirection : public MeasBase<MVDirection,MeasRef<MDirection> > 
{
public:
//# Friends
// Conversion of data
    friend class MeasConvert<MDirection,MVDirection,MCDirection>;

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
      BMEAN,
      BTRUE,
      GALACTIC,
      HADEC,
      AZEL,
      AZELSW,
      JNAT,
      ECLIPTIC,
      MECLIPTIC,
      TECLIPTIC,
      SUPERGAL,
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
      N_Planets,
      // All extra bits
      EXTRA = 32,
      // Defaults
      DEFAULT=J2000,
      // Synonyms
      AZELNE=AZEL};
  // Global types
  enum GlobalTypes {
    GRADEC,
    GHADEC,
    GAZEL,
    GLONGLAT,
    N_GTypes};

//# Typedefs
// MeasRef use
    typedef class MeasRef<MDirection> Ref;
// MeasConvert use
    typedef class MeasConvert<MDirection,MVDirection,MCDirection> Convert;

//# Constructors
// <note role=tip> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. For reasons
// of compiler limitations the formal arguments had to be specified as
// <em>uInt</em> rather than the Measure enums that should be used as actual 
// arguments.</note>
// Default constructor; generates the J2000 pole direction
    MDirection();
// Create from data and reference
// <group>
    MDirection(const MVDirection &dt);
    MDirection(const MVDirection &dt, const MDirection::Ref &rf);
    MDirection(const MVDirection &dt, uInt rf);
    MDirection(const Quantity &dt, const Quantity &dt1);
    MDirection(const Quantity &dt, const Quantity &dt1, 
	       const MDirection::Ref &rf);
    MDirection(const Quantity &dt, const Quantity &dt1, 
	       uInt rf);
    MDirection(const Quantum<Vector<Double> > &dt);
    MDirection(const Quantum<Vector<Double> > &dt, 
	       const MDirection::Ref &rf);
    MDirection(const Quantum<Vector<Double> > &dt, 
	       uInt rf);
    MDirection(const Measure *dt);
    MDirection(const MeasValue *dt);
    MDirection(const MDirection::Ref &rf);
// </group>

//# Destructor
    ~MDirection();

//# Operators

//# General Member Functions
// Tell me your type ('Direction')
// <group>
    virtual const String &tellMe() const;
    static const String &showMe();
// </group>
// Tell me your reference type (as Register())
    virtual uInt type() const;
// Assert you are a direction
    static void assert(const Measure &in);
// Tell me the global type (like GRADEC) for tp (tp like MDirection::J2000)
  static MDirection::GlobalTypes globalType(uInt tp);
// Translate reference code tp (should be given as e.g. MDirection::J2000)
    static const String &showType(uInt tp);
// Translate string to reference code
// <group>
  static Bool getType(MDirection::Types &tp, const String &in);
  Bool giveMe(MDirection::Ref &mr, const String &in);
// This one for historic reasons only
  Bool giveMe(const String &in, MDirection::Ref &mr);
// </group>
  // Set the offset in the reference (False if non-matching Measure)
  virtual Bool setOffset(const Measure &in);
  // Set the reference type to the specified String. False if illegal
  // string, reference set to DEFAULT.
  virtual Bool setRefString(const String &in);
  // Get the default reference type
  virtual const String &getDefaultType() const;
  // Get the reference type (for records, including codes like R_)
  virtual String getRefString() const;

// Get Measure data
// <group>
    Quantum<Vector<Double> > getAngle() const;
    Quantum<Vector<Double> > getAngle(const Unit &inunit) const;
// </group>

// Make a copy
// <group>
    virtual Measure *clone() const;
// </group>

private:

//# Data

//# Member functions

};

#endif

