//# MDirection.h: A Measure: astronomical direction
//# Copyright (C) 1995, 1996
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

#if defined(_AIX)
#pragma implementation ("MDirection.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MVDirection.h>

//# Forward Declarations
class MDirection;

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
//  <li>MDirection::JMEAN -- mean equator and equinox at frame epoch
//  <li>MDirection::JTRUE -- true equator and equinox at frame epoch
//  <li>MDirection::APP ---- apparent geocentric position
//  <li>MDirection::B1950 -- mean epoch and ecliptic at B1950.0
//  <li>MDirection::BMEAN -- mean equator and equinox at frame epoch
//  <li>MDirection::BTRUE -- true equator and equinox at frame epoch
//  <li>MDirection::GALACTIC -- galactic coordinates
//  <li>MDirection::HADEC -- topocentric HA and declination
//  <li>MDirection::AZEL --- topocentric Azimuth and Elevation
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
//   <li><em>Epoch</em>: all but J2000, B1950, GALACTIC
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
// <linkto class=MCatalog>MCatalog</linkto> class will maybe take care of that.
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

    friend class MeasConvert<MDirection,MVDirection>;
//# Enumerations
// Types of known MDirections
// <note> The order defines the order in the translation matrix FromTo
// in the getConvert routine. Do not change the order without
// changing the array. Additions should be made before N_types, and
// an additional row and column should be coded in FromTo, and
// in showType().</note>
    enum Types {J2000,
		JMEAN,
		JTRUE,
		APP,
		B1950,
		BMEAN,
		BTRUE,
		GALACTIC,
		HADEC,
		AZEL,
		N_Types,
		DEFAULT=J2000};
// The list of actual routines provided.
// <note> For each <src>AA_BB</src> in the list a routine
// <src>static void AAtoBB(MVDirection &)</src> should be provided. The routines
// should be listed in the FromToRout array in the getConvert routine, in the
// order specified. In addition the type to which converted should be in the
// ToRef array, again in the proper order. </note>
    enum Routes {
	GAL_J2000,
	GAL_B1950,
	J2000_GAL,
	B1950_GAL,
	J2000_B1950,
	B1950_J2000,
	J2000_JMEAN,
	B1950_BMEAN,
	JMEAN_J2000,
	JMEAN_JTRUE,
	BMEAN_B1950,
	BMEAN_BTRUE,
	JTRUE_JMEAN,
	BTRUE_BMEAN,
	J2000_APP,
	APP_J2000,
	B1950_APP,
	APP_B1950,
	APP_HADEC,
	HADEC_AZEL,
	AZEL_HADEC,
	HADEC_APP,
	N_Routes };

//# Typedefs
// MeasRef use
    typedef MeasRef<MDirection> Ref;
// MeasConvert use
    typedef MeasConvert<MDirection,MVDirection> Convert;

//# Constructors
// <note> In the following constructors and other functions, all 
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
// </group>

//# Destructor
    ~MDirection();

//# Operators

//# General Member Functions
// Tell me your type
// <group>
    virtual const String &tellMe() const;
    static const String &showMe();
// </group>
// Translate reference code
    static const String &showType(uInt tp);
// Translate string to reference code
    Bool giveMe(const String &in, MDirection::Ref &mr);

// Get Measure data
// <group>
    Quantum<Vector<Double> > getAngle() const;
    Quantum<Vector<Double> > getAngle(const Unit &inunit) const;
// </group>

// Make a copy
    virtual void *clone() const;

private:
//# Enumerations
// Usage of the MeasConvert structure cache. Additions should fit into the
// space provided in MeasConvert (see <src>MC_N_Struct</src> constant),
// and should be coded in the <src>clearConvert()</src> method.
    enum StructUse {
	ROTMAT1,
	EULER1,
	MVPOS1, MVPOS2, MVPOS3,
	SOLPOSFROM, SOLPOSTO,
	ABERFROM, ABERTO,
	NUTATFROM, NUTATTO,
	PRECESFROM, PRECESTO,
	N_StructUse };

//# Data

//# Member functions

// Create conversion function pointer
    static void getConvert(MDirection::Convert &mc,
			   const MDirection::Ref &inref, 
			   const MDirection::Ref &outref);

// Create help structures for Measure conversion routines
    static void initConvert(uInt which, MDirection::Convert &mc);

// Delete the pointers used in the MeasConvert help structure cache
     static void clearConvert(MDirection::Convert &mc);

// Routines to convert directions from one reference frame to another
     static void doConvert(MVDirection &in,
			   const MDirection::Ref &inref,
			   const MDirection::Ref &outref,
			   const MDirection::Convert &mc);

};

#endif

