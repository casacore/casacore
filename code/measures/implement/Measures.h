//# Measures.h:  a module for units, quantities and coordinates
//# Copyright (C) 1994,1995,1996,1997
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
//# $Id$

#if !defined (AIPS_MODULE_MEASURES_H)
#define AIPS_MODULE_MEASURES_H

//# Includes
#include <aips/Measures/Unit.h>
#include <aips/Measures/UnitMap.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/QMath.h>
#include <aips/Measures/QLogical.h>
#include <aips/Measures/QC.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasValue.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MeasDetail.h>

// <module>
// 

// <summary>
// To manipulate dimensioned values
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tUnit tQuantum tMeasMath tMeasure"
//	 demos="dMeasure">

// <prerequisite>
// </prerequisite>

// <etymology>
// The name Measure derives from physical measurements, i.e. values with
// units and possibly a reference frame attached.
// </etymology>
//
// <synopsis> 
// The Measure model deals with units, physical quantities
// (i.e. values with a unit) and measures (i.e. quantities with a
// reference frame).
// Units are handled in the <a href="#Unit">Unit</a> section
// (see <linkto class="Unit">Unit.h</linkto>). 
// Quantities are handled in the <a href="#Quantum">Quantum</a> section
// (see <linkto class="Quantum">Quantum.h</linkto>).
// Measures are handled in the <a href="#Measure">Measure</a> section
// (see <linkto class="Measure">Measure.h</linkto>).
//
// <h3> Includes</h3>
// Including the <src>aips/Measures.h</src> will take care of all
// includes necessary for the handling of Units and Quantities, and the
// general Measure interface. For the use of individual Measures, the
// appropiate include files should be added. E.g. to be able to handle
// Directions, the following includes should be given:
// <srcblock>
//	#include <aips/Measures.h>
//	#include <aips/Measures/MDirection.h>
// </srcblock>
// An inclusion of the appropiate measure file, will also take care of the
// connected measure value (in this case <src>MVDirection</src>. However,
// if only the value suffices, it van be included on its own.<br>
// When doing actual conversions (see MeasConvert later on), by using the
// explicit Measure::Convert types, the description of the actual
// conversions (called MCmeasure, e.g. MCEpoch.h) should be included as well.
// Some users may want to use special directives available in the detailed
// calculations (e.g. <src>Nutation::D_Interval</src> to set a non-default
// nutation interpolation interval). The corresponding header file
// should then be included.
//
//  <a name="Unit"><h3> Physical units </h3></a>
// Physical units are basically used in quantities
// (see <linkto class="Quantum">Quantum</linkto>), i.e.
// a value and a dimension. The Unit class, or one of its subsidiaries,  will
// in general not be called separately. The only reason to make use of these
// classes is to generate additional 'tagged' units, i.e. units with a
// special name, e.g. 'beam' for a telescope  beam, or 'JY', a non-SI name
// for Jy.
//  <h3> Units </h3>
// A Unit is in principle specified as a String (or directly as "string"),
// and can be defined as either a Unit or a String.
// If defined as a Unit, the format of the string will be checked for a
// legal definition and relevant information (e.g. scale, dimension type) is
// cached in the Unit object, leading to (much) faster use; if defined as a
// String, the checking will be postponed
// until any use is made of the information in the string.
//
// A unit is a string of one or more fields separated 
// by 'space' or '.' (to indicate multiply) or '/' (to indicate divide).
// Multiple separators are acted upon (i.e. <src>m//s == m.s</src>).
// Separators are acted upon left-to-right (i.e. <src>m/s/A == (m/s)/A</src>;
// use () to indicate otherwise (e.g. <src>m/(s/A)</src> )).
//
// A field is a name, or a unit enclosed in (), optionally followed by an,
// optionally signed, decimal constant. E.g. <src>m.(m/s)-2 == m-1.s2</src> )
//
// Note that a 'space' or '.' before an opening '(' can be omitted.
//
// A name can consist of case-sensitive letters, '_', ''', ':', '"' and '0'
// ('0' not as first character). Digits 1-9 are allowed if preceded with
// an '_'. Possible legal names are e.g. Jy, R0, R_1, "_2.
// <note role=tip>
// <ul>
//   <li> ' is used for arcmin
//   <li> '' or " for arcsec 
//   <li> : :: and ::: are used for h, min, s respectively.
// </ul>
// </note>
// <note role=tip> The standard naming conventions for SI units are that they
// are all in lowercase, unless derived from a person's name, when they start
// with a capital letter. Notable exceptions are some of the astronomical
// SI related units (e.g. AU).
// </note> 
// A name can be preceded by a (standard) decimal prefix.
//
// A name must be defined in a Unit map before it can be used.
//
// All SI units and some customary units are part of the classes. User
// defined names can be added by the UnitMap::putUser() function (see
// <linkto class="UnitMap">UnitMap</linkto>). A special set of FITS related 
// units can be added by the <src>UnitMap::addFITS()</src> function. 
//
// Example:
// <srcblock>
//	km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
// </srcblock>
// There are 5 name lists in the UnitMap, which are searched in reverse order:
// <ol>
//   <li> Defining units: 	m, kg, s, A, K, cd, mol, rad, sr
//   <li> SI units:		including a.o. g, Jy, AU
//   <li> Customary units:	e.g. lb, hp, ly
//   <li> User defined units:	defined by user (e.g. beam, KPH, KM)
//   <li> Cached units:	for speed in operations
// </ol>
// All known names can be viewed by running the tUnit test program, or
// using the MapUnit::list() routine.
//
// The definitions that were current on 960509 are given at end of this file
// 
//
//  <h3> Working with units </h3>
// In general units are not used explicitly, but are embedded in quantities
// and coordinates.
//
// Explicit use of units is only necessary if:
// <ol>
//   <li> a unit string has to be tested for legality (e.g. exist JY?)
//   <li> a unit string has to be named (e.g. H0 for km/s/Mpc)
//   <li> some calculation on units has to be performed
//		(e.g. how many hp.s per eV)
// </ol>
//
// For these cases a Unit can be defined as either a String or a Unit. If
// specified as a Unit an automatic check (with exception if illegal) of
// the format of the unit string is performed
// <srcblock>
// Unit a="km/Ms"; String b="Mm/Gs"; //produce 'identical' units a and b
// Unit a("KpH");      		// will produce exception
// String a("KpH");		// will be accepted till some other action
//				// done on a
// // The following will define a unit named 'tag' with a value identical
// // to 5 mJy. After this definition tag can be used as any other unit,
// // e.g. Unit("Gtag/pc") will be a valid unit string.
// UnitMap::putUser("tag",UnitVal(5.,"mJy"),"my own unit name for 5 mJy");
// // The following will calculate how many hp.s per eV
// Double hpeV = (UnitVal("hp.s")/UnitVal("eV")).getFac();
// // maybe after checking for identical dimensions
// if ( UnitVal("hp.s") != UnitVal("eV")) { cout << "unexpected" << endl; }
// </srcblock>
// <note role=tip>
// UnitVal has the following special constants to easily check unit
// dimensions (note that they can be combined to e.g. generate velocity
// as 'UnitVal::LENGTH/UnitVal::TIME')
// <ul>
//  <li> UnitVal::NODIM
//  <li> UnitVal::LENGTH
//  <li> UnitVal::MASS
//  <li> UnitVal::TIME
//  <li> UnitVal::TEMPERATURE
//  <li> UnitVal::ANGLE
//  <li> UnitVal::SOLIDANGLE
//  <li> UnitVal::MOLAR
//  <li> UnitVal::CURRENT
//  <li> UnitVal::INTENSITY
// </ul>
// </note>
//
// See the <linkto class="UnitVal">UnitVal</linkto>
// for details of calculating with units. 
// See the <linkto class="UnitMap">UnitMap</linkto>
// for the details of defining/viewing named units.
// 
//
//  <a name="Quantum"><h3> Quantums and Quantities </h3></a>
// A Quantum is a  value with a unit. Quantums are templated on their value
// type (e.g. <src>Float</src>, <src>Vector<Double></src>). <em>Quantity</em>
// is a typedef
// for the (probably most common) <src>Quantum<Double></src>.
// The basic specification of a Quantum is:
// <srcblock>
// Quantum<Type> ( Type value, Unit unit);	// or: String unit or: "unit"
// Quantity( Double value, Unit unit);		// or: String unit or: "unit"
// </srcblock>
//
// E.g.
// <ul>
//   <li> <src>Quantity(5.,"m");</src>
//   <li> <src>Quantum<Double> (5.,"m");   // identical to previous</src>
//   <li> <src>Vector<Int> a(3); a(3) = 5; Quantum<Vector<Int> >(a,"Jy");</src>
// </ul>
//
// The following list of constructors is available.
// <note role=tip>
// In the following 'Unit' can be replaced by 'String' (or "string" everywhere.
// The only difference being a check for a legitimate unit string being 
// executed if Unit specified (with exception if error), and a much faster
// execution of the Unit is used repeatedly.
// <src>Quantum<Type></src> can, if Type equals Double, be replaced with 
// <src>Quantity</src>
// </note>
// <ul>
//   <li> <src>Quantum<Type>()			value 0 generated</src>
//   <li> <src>Quantum<Type>( Quantum<Type>)	copy constructor</src>
//   <li> <src>Quantum<Type>( Type factor)	value factor generated</src>
//   <li> <src>Quantum<Type>( Type factor, Unit unit) specified quantity</src>
//   <li> <src>Quantum<Type>( Type factor, Quantum<any> quant) specified
//						 factor,
//						the unit from the quant</src>
// </ul>
//
// The following operators and functions are defined on Quantums. They are,
// of course, only available if the template Type supports them (e.g. / will
// not be defined for a <src>Quantum<String></src> (whatever that may mean)).
// <ul>
//   <li> <src>=	assignment of identical <type></src>
//   <li> <src>* *=	multiply two Quantums of same <type></src>
//   <li> <src>/ /=	divide two Quantums of same <type></src>
//   <li> <src>+ +=	add two Quantums of same <type> and same unit dimensions</src>
//			(else exception)
//   <li> <src>- -=	subtract two Quantums of same <type> and same unit dimensions</src>
//   			(else exception)
//   <li> 	-	negate Quantum
//   <li> <src>== !=	compare unit dimensions and value of same <type></src>.
//			They will be unequal if the unit dimensions do not 
//			match or the values (converted to common 
//			base units) are unequal
//   <li> <src>< >	compare unit dimensions of same <type></src>.
//			 Exception if no match,
//	 		else compare the values
//   <li> <src><= >=</src>	ibid
//   <li> pow(Quantum, Int) raise to an (integer) power
//   <li> abs(Quant)	take absolute value
//   <li> ceil, floor(Quant)
//   <li> sin, cos, tan(Quant) correct units used
//   <li> asin, acos, atan(Quant), atan2(Q,Q) correct units used
//   <li> near, nearAbs
// </ul>
// 
//
// Quanta can be converted to other units by the following set of member
// functions:
// <ul>
//   <li> convert()		will convert the quantum to canonical units.
//				E.g. given myval=Quantity(5.,"Jy"),
//				myval.convert() will make myval have the value
//				Quantity(5.e-26,"kg.s-2")
//   <li> get()			will return the quantum converted to 
//				canonical units
//   <li> convert(Unit unit) will convert the quantum to the
//				specified unit with any remaining dimensions
//				expressed in canonical units. E.g given
//				myval as above, myval.convert("W/cm") will
//				make myval Quantity(5.e-28,"W/cm.m-1.s")
//   <li> get(Unit unit) 	will return the quantum converted to unit
//   <li> <src>convert(Quantum<any> quant)</src> will convert the quantum
//				to the units of the specified quant with the
//				same conversion rules as the previous one
//   <li> <src>get(Quantum<any> quant) will return the converted quantum</src>
// </ul>
// Quanta can be checked for having the correct unit dimensions (e.g. before
// addition or comparing) by the following two member functions, which will
// return a Bool value or raise an exception:
// <ul>
//   <li> <src>Bool isConform(Unit)</src>
//   <li> <src>Bool isConform(Quantum<any>)</src>
//   <li> <src>Bool check(UnitVal)</src>
//   <li> <src> void assert(UnitVal)</src>
// </ul>
//
// The value and units of a quantum can be set or retrieved separately by the
// following member functions:
// <ul>
//   <li> <src>Type getValue()</src>	return the value (as Type) of the quantum
//   <li> <src>Type getValue(Unit)</src>	return the value in specified units
//   <li> <src>Type getBaseValue()</src>	return the value in canonical units
//   <li> <src>String getUnit()</src>	return the units of the quantum
//   <li> <src>void setValue(Type val)</src> replace the value of the quantum with val,
//				leaving the units the same
//   <li> <src>void scale(Type)</src> 	scale the value (leaving units same) by
//				multiplying with the specified value
//   <li> <src>void setUnit(Unit)</src> replace the units of the quantum, leaving
//				the value the same.
//   <li> <src>void setUnit(Quantum<any>)</src> ibid
// </ul>
//
// The output operator ('<<') will produce the value of the quantum and its
// units. Given <src>Quantity myval(5.,"mJy");</src>,
//	<src>cout << myval;</src> will produce:
//	"5.0 mJy"; while <src>cout << myval.get("yW/m2")</src> will produce:
//	".00005 yW/m2.s"
// 
//
//  <h3> QC class of constant quantities </h3>
// In parallel with the 'C' class of undimensioned constants, the QC class
// contains dimensioned constants.
// On 960509 the following were defined:
// <ul>
//   <li>  <src>Quantum<Double> c;	// vel of light</src>
//   <li>  <src>Quantum<Double> G;	// Gravitational constant</src>
//   <li>  <src>Quantum<Double> h;	// Planck</src>
//   <li>  <src>Quantum<Double> HI;	// Frequency HI line
//   <li>  <src>Quantum<Double> R;	// Gas constant</src>
//   <li>  <src>Quantum<Double> NA;	// Avogadro</src>
//   <li>  <src>Quantum<Double> e;	// electron charge</src>
//   <li>  <src>Quantum<Double> mp;	// proton mass</src>
//   <li>  <src>Quantum<Double> mp_me;	// mp/me</src>
//   <li>  <src>Quantum<Double> mu0;	// permeability vacuum</src>
//   <li>  <src>Quantum<Double> epsilon0; // permittivity vacuum</src>
//   <li>  <src>Quantum<Double> k;	// Boltzmann</src>
//   <li>  <src>Quantum<Double> F;	// Faraday</src>
//   <li>  <src>Quantum<Double> me;	// mass electron</src>
//   <li>  <src>Quantum<Double> re;	// radius electron</src>
//   <li>  <src>Quantum<Double> a0;	// Bohr's radius</src>
//   <li>  <src>Quantum<Double> R0;	// Solar radius</src>
//   <li>  <src>Quantum<Double> k2;	// IAU Gaussian grav. const **2</src>
// </ul>
// 
//  <a name="Measure"><h3> Measures</h3></a>
//
// Measures are physical quantities within a certain reference frame. Examples
// are the Hour-angle and Declination of a source at a certain time and 
// observatory; an Ra/Dec for a certain mean epoch; an apparent frequency at
// a certain time given in eV; a local sidereal time at an observatory.<br>
// Measures can be converted from one reference frame to another (and this
// possibility is its main reason for existence). A simple B1950-J2000
// coordinate conversion example:
// <srcblock>
//  cout <<       		// output
//				// the conversion of a B1950 direction
//	MDirection::Convert( MDirection( Quantity( 20, "deg"),
//				         Quantity(-10, "deg"),
//					 MDirection::Ref( MDirection::B1950)),
//				// to J2000
//			     MDirection::Ref( MDirection::J2000)) () 
//				// where the constructor sets up a conversion
//				// engine, and the operator() converts
//		<< endl;
// </srcblock>
// or converting an UTC to a local apparent sidereal time:
// <srcblock>
//  // Set up the model for the input (default reference is UTC)
//	MEpoch model (	Quantity(0., "d"));
//  // Set up the frame with the observatory position
//	MPosition obs(	MVPosition(	Quantity( 10, "m"),
//					Quantity( -6, "deg"),
//					Quantity( 50, "deg")),
//			MPosition::Ref(MPosition::WGS84));
//	Measframe frame( obs);
//  // Set up the output reference
//	MEpoch::Ref outref(	MEpoch::LAST,
//				frame);
//  // Set up conversion
//	MEpoch::Convert	toLST(	model,
//				outref);
//  // Output a series of sidereal times (formatted in ddd::hh:mm:ss)
//	for (Double d = 12345; d<12346; d += 0.1) {
//	  cout << "Converted from UTC to LAST: " <<
//		d <<  " : " <<
//		toLST(d).getValue() << endl;
//	};
// </srcblock>
//
// The examples show the use of the 5 major classes involved in Measures:
// <srcblock>
//   Base         Example               Description
//  ------       ---------             -------------
//  Measure      MEpoch          has a value and a  reference
//  MeasValue    MVEpoch         value
//  MeasRef      MEpoch::Ref     contains type, frame, offset
//  MeasFrame    MeasFrame       contains Measures describing frame
//  MeasConvert  MEpoch::Convert contains conversion information and engine
// </srcblock>
//
// Each type of Measure has its own distinct class. Each
// is (weakly) derived from the <linkto class="Measure">Measure</linkto> base
// class, and its name starts with an <em>M</em>. Examples are:
// <ul>
//   <li> <linkto class="MEpoch">MEpoch</linkto>: an instance in time
//   <li> <linkto class="MDirection">MDirection</linkto>: a direction in space
//   <li> <linkto class="MPosition">MPosition</linkto>: a position on Earth
//   <li> <linkto class="MFrequency">MFrequency</linkto>: the characteristics
//		of a wave
//   <li> <linkto class="MDoppler">MDoppler</linkto>: a Doppler shift
//   <li> <linkto class="MRadialVelocity">MRadialVelocity</linkto>: a 
//		Doppler shift
// </ul>
// Others are being, or could be, considered.<br>
// <note role=tip>The current set can be deduced from the class list at the end of
// the html version of this module description.</note><br>
// <p>
// The main role of the Measure (and related) classes is to be able to convert
// an observed (or to be calculated) physical entity from one reference frame
// to another, e.g. a J2000 coordinate to galactic coordinates, or an TAI
// time to a local sidereal time (LAST).
// Simple unit conversions (e.g. an angle from mrad to deg), or calculations
// with values with attached units, are sufficiently catered for by the
// <a href="#Quantum">Quantum</a> classes.
// <p>
// Each measure has a <em>value</em> (<linkto class=MeasValue>MeasValue</linkto>) and
// a <em>reference</em> (<linkto class=MeasRef>MeasRef</linkto>).<br>
// The values are in general measure specific, weakly derived from MeasValue,
// and named with an initial <em>MV</em>. Examples are:
// <ul>  
// <li> <linkto class=MVEpoch>MVEpoch</linkto> (a high precision single value), 
// <li> <linkto class=MVDirection>MVDirection</linkto> (direction cosines), 
// <li> <linkto class=MVPosition>MVPosition</linkto> (3-vector positions),
// <li> <linkto class=MVFrequency>MVFrequency</linkto> (single, unit depended
// value).<br>
// <li> <linkto class=MVDoppler>MVDoppler</linkto> (single, unit depended value)
// <li> <linkto class=MVRadialVelocity>MVRadialVelocity</linkto> (single value)
// </ul>
// In addition some other value classes, not directly used in measures, are
// available. Examples:
// <ul>
//  <li> <linkto class=MVAngle>MVAngle</linkto> (to normalise
// and have specific I/O formatting for angle-like values)
// <li> <linkto class=MVTime>MVTime</linkto> (same for time-like values)
// </ul>
// <em>References</em> are measure specific. Each specific reference class is
// called <em>Measure</em>::Ref (e.g. <src>MEpoch::Ref</src>). It specifies
// the full reference frame of the specific measure, i.e. its type, an optional
// frame of measures (a MeasFrame, consisting of say a time and position), and
// an optional offset.
// It has at least a <em>reference code</em>
// (e.g. MDirection::B1950, MEpoch::LAST), with defaults for each measure
// (i.e. MDirection::J2000, MEpoch::UTC) if none specified. <br>
// In addition the reference can contain a <em>reference frame</em> 
// (<linkto class=MeasFrame>MeasFrame</linkto>) to specify from when and/or
// where the measure was obtained or calculated.<br>
// A third optional element of the reference is an <em>offset measure</em>, which
// indicates the offset (e.g. a sidereal date) that has to be added to the
// value referenced before it is used.<br>
// Examples of some measures are:
// <srcblock>
//	// An instance of time expressed in days (MJD) in UTC
//	MEpoch date(MVEpoch(Quantity(50237.29, "d")),
//		    MEpoch::Ref(MEpoch::UTC));
//	// which could also be expressed as:
//	MEpoch date(Quantity(50237.29, "d"),
//		    MEpoch::UTC);
//	// or using the default reference type:
//	MEpoch date(Quantity(50237.29, "d"));
//	// or as a time with an offset to a specific date:
//	MEpoch date(Quantity(12.3, "h"),		// time
//		    MEpoch::Ref(MEpoch::UTC,		// reference with
//		    		MEpoch(Quantity(50237, "d"))));  // offset
//	// A position of a telescope
//	MPosition pos(MVPosition(Quantity(25, "m"),	// height
//				 Quantity(20, "deg"),	// East longitude
//				 Quantity(53, "deg")),	// lattitude
//		      MPosition::WGS84);		// reference type
//	// Use this position in a frame
//	MeasFrame frame(pos);
//	// Specify an LAST (in MGSD) observed at this position:
//	MEpoch last(Quantity(51000.234, "d"),		// time and date
//		    MEpoch::Ref(MEpoch::LAST,		// indicate LAST
//				frame));		// and where observed
//	// Maybe we know the MJD of the observed sidereal time,
//	// but not its sidereal date. We could then specify it as an
//	// offset to the beginning of the sidereal day in progress at
//	// specified UTC
//	MEpoch last(Quantity(13.45, "h"),		// time
//		    MEpoch::Ref(MEpoch::LAST,		// indicate LAST
//				frame,			// where observed
//				MEpoch(51234,		// MJD of today
//				       MEpoch::Ref(MEpoch::TAI + MEpoch::RAZE)));
//	// where the RAZE indicates that the value will be truncated after
//	// conversion. In this case it will be converted to LAST to be able
//	// to add it as an offset to the specified LAST
//	//
//	// A direction (in RA/Dec) could be:
//	MDirection coord(MVDirection(Quantity(54, "deg"),	// RA
//				     Quantity(2034, "'")),	// DEC arcmin
//			 MDirection::Ref(MDirection::J2000));	// J2000 type
//	// If it were apparent coordinates, the time when observed should
//	// have been known. We could just add it to the frame defined above,
//	// and use it:
//	frame.set(date);				// add time to frame
//	MDirection acoord(MVDirection(Quantity(54, "deg"),	// RA
//				      Quantity(2034, "'")),	// DEC
//			  MDirection::Ref(MDirection::APP,	// apparent type
//					  frame));		// and when
//	// If it was given in HA/Dec, the position should have been known
//	// as well, but it is already in the frame, hence we could say:
//	MDirection acoord(MVDirection(Quantity(54, "deg"),	// HA
//				      Quantity(2034, "'")),	// DEC
//			  MDirection::Ref(MDirection::HADEC,	// type
//					  frame));		// when/where
// </srcblock>
// <note role=tip>In the above examples in general explicit <em>MV</em>
// values have been used to specified the measure's value. In many
// cases (depending on the actual measure) it can be omitted, and the data
// can be given directly to the measure constructor. See the 
// constructors for the individual measures for details.<br>
// If the reference is simple (i.e. no frame and/or offset) the 
// <em>Measure::Ref</em> can be omitted, and only the code has to be
// specified. </note>
// A <linkto class=MeasFrame>MeasFrame</linkto> is a container for specifying
// Measures needed to describe the circumstances under which the measure was
// observed (or for which it has to be calculated).
// E.g. the position on Earth (an <em>MPosition</em>) is necessary for 
// sidereal time and coordinates like HA/Dec and Az/El; the time 
// (<em>MEpoch</em>)
// is necessary for non-standard coordinates (apparent, mean, HA/Dec etc); 
// the coordinates (<em>MDirection</em>) for radial velocities; etc.<br>
// Although quite often the value has to be in a specific format (e.g. TBD for
// precession calculations; astronomical longitude for the LAST), the
// frame values can be given in any known reference format: conversion to the
// appropiate type will be done automatically if and when necessary.<br>
// Frames (and references) are never copied, but act always as containers
// with shallow copying only (i.e. <em>copied</em> frames will point to
// identical instances, and changes made in one copy will be visible in all
// others. This
// means, e.g., that in the following:
// <srcblock>
//	MeasFrame frame1(MEpoch(50236.12));
//	MeasFrame frame2(frame1);
// </srcblock>
// the two frames will be identical, and a change to one means a change to 
// the other. Furthermore, only the information needed for a specific 
// calculation will be used (and calculated). This means that one frame can
// be used specifying all of e.g. the position (which will probably stay the
// same for a series of calculations) and time; with the time being <em>set()</em>
// (if also the reference of the epoch changes) or <em>resetEpoch()</em> (if only
// the value changes, but the reference and its frame stay the same).
// A change in the frame will influence automatically any calculation (e.g.
// conversion to LAST) of which it is part.<br>
//
// The value of a measure (in <em>MV</em> format) can be obtained with the
// <em>getValue()</em> member function. The value in a variety of formats
// and units can be obtained with a (specific Measure dependent) series of
// <em>get()</em> members of both the <em>MV</em>-value and the Measure.<br>
//
// Measures in themselves are not really necessary for proper data reduction
// and the like. Its real value is the ability to transform a Measure from
// one reference type (and frame, offset) to another.<br>
// Conversion of a measure of a certain kind from one reference to another
// is done with the aid of special, measure specific,
// <linkto class=MeasConvert>MeasConvert</linkto> classes. Each conversion
// class is called <em>Measure</em>::Convert (e.g. MDirection::Convert).
// A conversion generates from an input reference (or an input measure) and
// an output reference a conversion functional, that can be used to convert
// specific values.<br>
// Example:
// <srcblock>
//  cout <<       		// output
//				// the conversion of a B1950 direction
//	MDirection::Convert( MDirection( Quantity( 20, "deg"),
//				         Quantity(-10, "deg"),
//					 MDirection::Ref( MDirection::B1950)),
//				// to J2000
//			     MDirection::Ref( MDirection::J2000)) () 
//				// where the constructor sets up a conversion
//				// engine, and the operator() converts
//		<< endl;
//</srcblock>
// The same could have been done by only setting up the conversion engine, and
// not specifing the default value to be converted in the Convert constructor
// by:
// <srcblock>
//  cout <<       		// output
//				// the conversion of a B1950 direction
//      MDirection::Convert(MDirection::Ref( MDirection::B1950),
//				// to J2000
//			    MDirection::Ref( MDirection::J2000))
//				// and use conversion on value
//				 (MVDirection( Quantity( 20, "deg"),
//					       Quantity(-10, "deg")))
//				// where the operator() converts
//		<< endl;
// </srcblock>
// Specifying the conversion engine separately, it can be re-used for other
// values:
// <srcblock>
//	MDirection::Convert conv(MDirection::Ref( MDirection::B1950),
//				 MDirection::Ref( MDirection::J2000));
//	// We have some coordinates from somewhere, say coord(0:N-1):
//	for (Int i=0; i<N; i++) {
//	   cout << "B1950: " << coord(i) << "= J2000: " <<
//		                conv(coord(i)) << endl;
//      };
// </srcblock>
// A larger example. Say you have the J2000 coordinates for a source (RA=11
// deg, DEC= -30 deg), and you want to observe it on May 17, 1996 (MJD=50220)
// at 8:18 UTC in a place
// with a Longitude of 150 deg (latitude of 20 deg) at 1000 m high,
//  you could get the
// apparent RA,DEC, and the LAST at that time (you could also go straight to
// HA/DEC or so) with (I write the example longer than necessary to indicate
// the steps, and with explicit reference to MV values):
// <srcblock>
// // The observatory position. Note that the reference is geodetic position 
//	MPosition myobs(MVPosition ( Quantity(1, "km") ,
//				     Quantity(150, "deg"),
//				     Quantity(20, "deg")),
//				     MPosition::WGS84);
// // The time I want to observe (note that it could be specified in many
// // other ways)
//	MEpoch obstime(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)), 
//	               MEpoch::UTC);
// // The frame specification for when and where to observe
//	MeasFrame frame(myobs, obstime);
// // The reference for a sidereal time (note the frame could be empty and 
// // filled at the actual conversion time)
//	MEpoch::Ref sidref( MEpoch::LAST, frame);
// // The reference for apparent coordinates:
//	MDirection::Ref appref( MDirection::APP, frame);
// // The conversion engine for my time to LAST
//	MEpoch::Convert tosid(obstime, sidref);
// // The conversion to sidereal time of obstime
//	MEpoch sidtime = tosid();
// // Conversion of UTC 10.8 h 
//	sidtime = tosid(MVEpoch(MVTime(1996, 5, 17, 10.8/24.))); 
// // Show me some time
//	cout << "LAST for UTC = 11:00: " <<
//		tosid(MVEpoch( MVTime( 1996, 5, 17, 11, 0))) << endl;
// // An offset reference (note the RAZE will keep only the integer part of
// // the day for the conversion result)
//	MEpoch::Ref offtime(obstime.getValue(), MEpoch::UTC+MEpoch::RAZE);
// // The reference for a sidereal with respect to a specified offset (note
// // that it is automatically calculated into correct units)
//	MEpoch::Ref sidoffref(MEpoch::LAST, frame, offtime);
// // Show the offset result
//	cout << "LAST today: " <<
//		MEpoch::Convert(11., sidoffref)() << endl;
// // Coordinate conversion from J2000
//	cout << "Apparent coordinates: " <<
//		MDirection::Convert ( MDirection(Quantum(11,"deg"),
//                                               Quantum(-30, "deg")),
//	                              MDirection::Ref( MDirection::APP,
//	                                               frame))() << endl;
// // Handier to have the conversion engine available
//	MDirection::Convert cvt( MDirection(Quantum(11,"deg"),
//                                          Quantum(-30, "deg")),
//	                         MDirection::Ref( MDirection::APP,
//	                                          frame));
// // Set another frame time (note it is now sidereal, not UTC. The
// // frame will automatically convert it (using the frame again for
// // position) to TDB for precession etc calculations).
//	frame.set(sidtime);
// // And look what same position is at this new time
//	cout << "Next position: " << cvt() << endl;
// </srcblock>
// <p>
// Some conversions need maybe some fine tuning (e.g. what is the acceptable
// interval for Nutation linear interpolation: could be different from the
// default interval; some time calculations will want to use the predicted
// IERS values rather than the actual determined; some Nutation will maybe
// use the IERS updates, some maybe the IAU analytic extensions).<br>
// The <linkto class=MeasDetail>MeasDetail</linkto> class
// is a general container, in which very specific parameters can be put to
// steer the conversion process beyond what is possible with just a list
// of measure reference types (that list is already long for some cases).
// Values, switches can be <src>set()</src> (and removed) to change the
// default behaviour of the conversions. In general the user will only need
// to use the details in very specific cases. The details that can be used
// are described in the classes that provide calculations (e.g.
// <linkto class=Nutation>Nutation</linkto>).<br>
// Some of the MeasDetail parameters can be set in the <src>aipsrc</src>
// resources files (either the system, site, or user (<src>.aipsrc</src>) ones.
// The method is described in <linkto class=MeasDetail>MeasDetail</linkto>.
// <p>
// Some details about the different classes follows. In the examples often
// a specific measure value (e.g. MVEpoch, the MeasValue for MEpoch), or a
// specific measure (e.g. MDirection, a direction in space) is used. This
// is only to visualise the use, any other measure could have been used.
// <p>
// <h4> MeasValue</h4>
// The MeasValue class derivatives are all named <em>MVmeasure</em>, e.g.
// <em>MVFrequency</em>, and represent the internal representation of the
// specific measure class. They all have at least the following constructors:
// <srcblock>
//	MV()
//	MV(MV)
//	MV(Double)
//	MV(Vector<Double>)
//	MV(Quantity)
//	MV(Vector<Quantity>)
//	MV(Quantum<Vector<Double> >)
// </srcblock>
// But most have also constructors like:
// <srcblock>
//	MV(Double, Double)
//	MV(Quantity, Quantity)
// </srcblock>
// The actual interpretation is class dependent: see the individual MV classes
// like </linkto class=MVEpoch>MVEpoch</linkto>,
// </linkto class=MVDirection>MVDirection</linkto>,
// </linkto class=MVPosition>MVPosition</linkto>,
// </linkto class=MVFrequency>MVFrequency</linkto>,
// </linkto class=MVDouble>MVDouble</linkto>,
// </linkto class=MVRadialVelocity>MVRadialVelocity</linkto>.
// A few examples:
// <srcblock>
//   MVEpoch(12345, 0.1e-20) will create one epoch (MJD12345.0), but preserving
//			   the precision of all information
//   MVDirection(Quantity(20,"deg"), Quantity(-10,"'")) will create a direction
//			   with an RA of 20 degree, and a DEC of -10 arcmin
//   MVFrequency(Quantity(5,"keV")) will create a frequency corresponding to
//			   the specified energy.
// </srcblock>
// All MVs have the <src>+=, -=, ==, !=, << </src>operators, and <src>near()</src>,
// <src>nearAbs()</src>, <src>print()</src> and <src>adjust()</src>
// and <src>readjust()</src> (which in general
// normalise to a value of 1 (e.g. MVDirection), or recalculates high
// precision values (e.g. MVEpoch) functions.<br>
// Information can be viewed with many <em>get</em> functions. In most cases
// getValue() will return the internal value as either Double or 
// Vector<Double>; get() will return the same, or converted values (e.g.
// a vector of length, angle, angle for MVPosition; while special
// one like getAngle() or getAngle(unit), getTime() etc will return Quantums
// (with optional conversion to specified units).<br>
// In general the Measure classes can be used without worrying about the
// MeasValues, since most Measure constructors have enough flexibility (and
// their own get()'s) to be able to use them independently).<br>
// Special cases are <linkto class=MVAngle>MVAngle</linkto> and 
// <linkto class=MVTime>MVTime</linkto>, which can do special formatting for
// time and angles (in earlier documentation they were called HMS etc.).
// <p>
// <h4> Measure</h4>
// The Measure class derivatives are all called <em>MMeasure</em>. Details
// can be found in
// <linkto class=MDirection>MDirection</linkto> (a celestial direction),
// <linkto class=MPosition>MPosition</linkto> (a position on Earth),
// <linkto class=MFrequency>MFrequency</linkto> (characteristics of 
// 	electro-magnetic wave),
// <linkto class=MEpoch>MEpoch</linkto> (an instance in time),
// <linkto class=MDoppler>MDoppler</linkto>,
// <linkto class=MRadialVelocity>MRadialVelocity</linkto>. <br>
// A measure has a value (kept in internal units in <em>MVmeasure</em>
// format) and a definition
// of the reference frame (MeasRef) of the value. The reference is optional, and
// will default to <em>Measure::DEFAULT</em>.<br>
// All measures have a set of standard constructors:
// <srcblock>
//	M();			// some default, e.g. pole directoon, time ==0)
// 	M(MV, MeasRef);
//	M(Quantity, MeasRef);
//	M(Quantum<Vector<Double> >, MeasRef);
//	M(Vector<Quantity>, MeasRef);
// </srcblock>
// But also some special ones (e.g. two Quantities for MDirection to specify
// two angles) depending on type. The MeasRef can be omitted (will then be
// defaulted to Measure::DEFAULT, e.g. MEpoch::DEFAULT); can be specified as
// a full reference as a <em>Measure::Ref</em> (e.g. <em>MDirection::Ref</em>)
// type; or as a simple reference as <em>Measure::TYPE</em> (e.g. 
// <em>MDirection::J2000</em>).<br>
// The individual elements of a Measure (i.e the MV value and the reference)
// can be overwritten (or set) with the <src>set()</src> methods.<br>
// <src>get()</src> methods (in general <src>get(unit)</src> 
// to return the internal value in some
// specified unit as a Quantum; and methods like <src>getAngle()</src>
// for e.g. MDirection)
// enable the user to obtain the value of the measure.<br>
// A <src>String tellMe()</src> will tell the type of Measure; a 
// <src>void assert(String)</src> and <src>Bool areYou(String)</src> will
// check the type; while a <src>String showType(Measure::TYPE)</src> will
// return the string value of a reference type code (e.g. J2000).<br>
// <p>
// Recall that a Measure is a value with a reference specified. The MeasConvert
// engines enable you to convert it into another Measure, with a different 
// reference (e.g. from J2000 to AZEL). The different get() methods (either
// directly, or indirectly using additional MV get() functions, or
// Quantum conversion methods, can convert the internal value into a value
// (or values) with user preferred units.<br>
// For reasons of speed (and safety) the allowed reference types for each
// Measure are enumerated in each measure class. The different reference 
// types for MDirection are, for example:
// <srcblock>
//    		MDirection::J2000,
//		MDirection::JMEAN,
//		MDirection::JTRUE,
//		MDirection::APP,
//		MDirection::B1950,
//		MDirection::BMEAN,
//		MDirection::BTRUE,
//		MDirection::GALACTIC,
//		MDirection::HADEC,
//		MDirection::AZEL,
//		MDirection::DEFAULT = MDirection::J2000
// </srcblock>
// The MEpoch has a special reference type (<src>MEpoch::RAZE</src>) that 
// can only be used
// in conjuncion with another reference type 
// (e.g. <src> MEpoch::UT1+MEpoch::RAZE)</src>.
// The meaning is: if a measure with such a reference type is converted to
// another reference type (say <src>MEpoch::LAST</src>) the 
// resultant (sidereal time)
// instance will be <em>razed</em> to an integer number of days; hence providing
// an easy way to specify sidereal times offset with the beginning of the
// current sidereal day.<br>
// To aid with external data, a <src>Bool giveMe(String, uInt)</src> will
// give the correct reference type to be used given the String type.
// Note that the
// uInt, rather than the corresponding enum is used, due to templating
// restrictions in some compilers.<br>
// The correct reference (MeasRef) and conversion (MeasConvert) class for
// each Measure (a frequency cannot be converted into an epoch) are templated,
// and have specified (and to be used) typedefs: <em>Measure::Ref</em> and
// <em>Measure::Convert</em> (e.g. <em>MEpoch::Ref, MEpoch::Convert</em>).
// <p>
// <h4>Measure errors </h4>
// In the current implementation, no errors are attached to a Measure. In the
// original design errors were foreseen, but up till now they have been left
// out.<br>
// The addition of errors is in principle an easy process. They could be 
// attached to either a Measure (as an additial MV value), or the MV's could
// be expanded to include errors (my preferred option at the moment). An
// MV being converted will then automatically have its error converted as
// well.<br>
// Before implementing, however, I think it would be worthwhile to look at
// the whole area of error handling. The easiest way would be to introduce
// for each of the defined aips++ standard values a corresponding E class
// (EDouble, EInt, EComplex, EuInt etc), and have all mathematical and
// logical operators that are defined for the standard classes be defined
// for the E-classes as well. It would then be easy to introduce errors
// everywhere.
// <p>
// <h4>MeasFrame</h4>
// A MeasFrame is a container with the instance of time
// (an MEpoch) and/or the position (an MPosition) for a measure reference.
// (Other Measures, like MDirection and MRadialVelocity are sometimes needed
// as well).
// MeasFrames are never actually copied, but only referred to (<em>shallow copy</em>)
// , so they can be used for all different types
// of measure reference. They are only necessary, but then essential, if the
// reference type does not fully specify the frame (like e.g. MDirection::J2000,
// or MEpoch::TAI do). Examples are the position necessary to go to
// MEpoch::LAST, the epoch necessary to go to MDirection::APP, the epoch and
// position necessary to reference an MDirection::AZEL.<br>
// A MeasFrame can be constructed empty (and used in references, as long as it
// is filled properly at the time of an actual conversion), or with one or
// Measures already defined with: <src>MeasFrame frame(a_Measure, ...)</src>.
// It can be filled, or re-filled, with <src>set(a_measure,....)</src>.<br>
// The conversion routines use different values of the frame values given (e.g.
// the precession and nutation will need the epoch in TDB time, the hour-angle
// constructor local apparent sidereal time, which needs the astronomical
// longitude etc.). For that reason the specification of an epoch or position
// in either the constructor or the set() will create conversion engines for
// conversion of the input measure to all appropiate values that can be asked
// by the conversion routines. Note that the actual conversion is only done
// when that value is requested (and is then saved for later use). It is,
// therefore, safe and probably good practice to have one frame in a certain
// conversion environment, filled with as much info as is needed at that stage.<br>
// To aid and speed up, <src>resetEpoch()</src> and <src>resetPosition()</src>
// methods are available. As arguments they accept the corresponding
// MV or a variety of Double and Quantum arguments to reset the <em>value</em>
// of the corresponding frame measure only. In that case the conversion engine
// won't be redesigned, leading to fast recalculation when necessary, since
// e.g. nutation values could be re-used.<br>
// In an observing environment you could hence setup a proper frame with the
// Observatory position, and an observing day offset (see MeasRef) time; and
// do resetEpoch() to update the time if and when necessary.<br>
// <p>
// <h4>MeasRef</h4>
// A MeasRef is a measure specific container (and its class reference is
// <src>Measure::Ref</src>, e.g. <src>MFrequency::Ref</src>) with the
// measure reference type (e.g. <src>MEpoch::UTC</src>), an optional (but in
// some cases necessary) MeasFrame (e.g. to specify where the sidereal time
// was determined), and, just for convenience, an optional offset (e.g.
// the MJD for which the time specified in the MEpoch referenced is valid).
// Note that if no frame or offset is necessary, the <src>Measure::TYPE</src>
// can be used everywhere where a <src>Measure::Ref</src> is needed.<br>
// A MeasRef is never copied (all copying and so is done by referencing). This
// means, for example, that if a specific MeasRef is part of the MEpoch
// definition for an epoch that is part of a MeasFrame, and you chnage that
// MeasRef, the change will automatically occur wherever that MeasRef is
// used (as e.g. in the frame). In most cases that is the expected response,
// but you should be aware of it, and not re-use a MeasRef for a completely
// different purpose.<br>
// A simple example:
// <srcblock>
// 	MEpoch mytime(MVEpoch(50236.5), MEpoch::UTC);
// // this will define a time in UTC on MJD 50236, 12 hours. The MVEpoch
// // explicit conversion could be left out for most compilers, but some
// // have trouble with automatic conversions.
// // Another way of doing it would be to use Quantities, which have
// // explicit constructors for all measures:
//	MEpoch mytime(Quantity(50236.5, "d"));
// </srcblock>
// A slighty more involved example, written out a bit:
// <srcblock>
// // Specify the location of the observatory (10m high, at given longitude
// // and latitude as geodetic position)
//	MPosition obs( 	MVPosition( 	Quantity( 10, "m"),
//					Quantity( -6, "deg"),
//					Quantity( 52, "deg")),
//			MPosition::WGS84);
// // If the current time is MJD50236, 12.3 h UTC, it could be specified as:
//	MEpoch tim(	MVEpoch(	Quantity( 50236, "d"),
//					Quantity( 12.3, "h")));
// // Note the default reference
// // For this example we will also specify it as:
//	MEpoch offtim(tim); 
//	offtim.set(MEpoch::DEFAULT+MEpoch::RAZE);
// // These two could define a frame
//	MeasFrame frame(tim, obs);
// // Or maybe as (since observatory will stay put)
//	MeasFrame frame1(obs);
// // and later addition of some time and its reference frame
//	frame1.set(tim);
// // with a change to another time value at a later stage with
//	frame1.resetEpoch(	MVEpoch(	Quantity( 50236, "d"),
//					Quantity( 13, "h")));
// // At this time we observe a sidereal time of 2.3 h. The actual instance
// // of time needs a sidereal date to specify, but we are too lazy to
// // look it up, hence we specify that this time has an offset, equal to
// // the sidereal time at offtim (which with the RAZE addition will be
// // converted to an integral number of days in whatever time it is
// // converted to)
//	MEpoch mylast(	MVEpoch(	Quantity( 2.3, "h")),
//			MEpoch::Ref(	MEpoch::LAST,
//					frame,
//					offtim));
// // Which specifies that we have a Local apparent sidereal time of 2.3 h
// // at the position specified by obs in the frame, at an offset offtim.
// // Note that the offset is given in UTC (and RAZE). Any conversion of
// // this mylast value to any other reference type, will always auto start
// // with a conversion of the offset to the current type (i.e LAST (with
// // the RAZE taking the integer part only)), and adding it to the value
// // given. Note that if an output reference has an offset, the resulting
// // value will be corrected for the specified offset as well.
// </srcblock>
// The reference type can be set with a set() function, and set() functions
// for the offset and frame will be present as well.<br>
// A <src>Bool empty()</src> checks if the reference is empty; <src>get()</src>
// functions provide the information in the reference; and a 
// <src>String showMe()</src> will return the type of measure (e.g. "Epoch") the
// MeasRef can be used for.
//<p>
// <h4> MeasDetail</h4>
// The MeasDetail class is a global, static container class.
// It is filled with a series of values, associated with enumerated names.
// The values can be <src>Bool</src>, <src>Double</src>, 
// <src>Vector<Double></src>, <src>Quantity</src> or 
// <src>Quantum<Vector<Double> ></src>; the names are dependent on the using
// class (e.g. <src>Precession::D_Interval</src> if used in a program by the
// <src>MeasDetail::set(name, value)</src> method, (or 
// <src>measures.precession.d_interval</src> if used in an aipsrc resource file), 
// which is a Double value giving a non-default interval
// over which the precession calculation is averaged).<br>
// The details are filled with <src>MeasDetail::set(code, value)</src>, and examined
// with <src>Bool MeasDetail::get(code, value&)</src>, or in the aipsrc files.
// Note that any program setting/removing will overwrite the external input.
// A <src>MeasDetail::remove(code)</src> can empty
// a field.<br>
// In normal operations the MeasDetail class will not be used (system knows best),
// but it can be used to overwrite default behaviour (like the approximation
// intervals used; type of IERS or the user's own data etc).<br>
// Some of the details can be set by the user in any one of the aipsrc (or
// .aipsrc if in the home directory) resource files. See the individual classes
// (Nutation etc, MeasIERS, MeasData) for the details that can be set, and
// <linkto class=MeasDetail>MeasDetail</linkto> about the use of aipsrc.
// <p> 
// <h4>MeasConvert</h4>
// The MeasConvert class converts Measures from one reference type and frame
// to another.
// It gathers all relevant
// information and analyses it to have fast multiple conversions.
// The MeasConvert classes are Measure specific, and should be used with
// the class names <src>Measure::Convert</src> (e.g. <src>MFrequency::Convert
// </src>).
// The () operator will do the actual conversion; constructors and set()
// methods will only fill the information necessary to do the conversion.
// MeasConvert is a non-copying container.<br>
// To set up the conversion engine, the MeasConvert object has to know the
// input data reference (remember the MeasRef contains information about the
// type, the possible reference frame and a possible offset), and an output
// reference. Using these references it will communicate with the appropiate
// Measure class to set up a series of routines that have to be executed in
// order to attain the goal. (Note that if the input and output reference
// both define a frame, but different ones, e.g. because you want to convert
// a  sidereal time at one place to a sidereal time at another place, the
// conversion machinery will always first go to the proper default (UTC in this
// case), and then go to the goal).<br>
// The actual conversion need a value to be converted, and it also can use
// a default Unit, so that if your frequencies are in nm, you can once
// specify that they are nm, and then simply convert a Double.<br>
// This means that the optimal constructor for a MeasConvert is:
// <srcblock>
// // The first argument will give the input reference, and, if a Quantum is
// // used to make the Measure, the default units for inputs to the conversion.
// // It acts as a 'model' for subsequent input to be converted.
// // () operator
//	Measure::Convert(	Measure(Quantum), 
// // the second argument gives the output reference
//				Measure::Ref);
// </srcblock>
// The actual constructors present include ones with the first argument only
// an input reference, rather than a full Measure.
// However, in all cases an empty or partial one can be constructed, with set()
// functions filling in the rest. The conversion engine is only
// (re-)setup if at least an input and output reference can be found.<br>
// After setting up the conversion engine, the () operator can be used with
// a variety of values to return a converted Measure. Possibilities are:
// <srcblock>
//	()		// convert the value as specified in the 'model'
//	(Double)	// convert the value first to appropiate units (if they
//			// were implicit in 'model' or explicitly set), and
//			// then convert
//	(Vector<Double>)// as Double
//	(Quantity)	// convert the full value, including its own units
//	(Quantum<Vector<Double> >) // as Quantity
//	(MeasValue)	// convert the specified appropiate MV
//	(Measure)	// set up a new conversion chain, using the value as
//			// 'model', and the old output reference,
//			//  and then convert
//	(Measure, Measure::Ref) // set up a new conversion chain for the
//			// 'model' given and the output reference given
//	(Measure::Ref)	// set up a new conversion chain using the old 'model'
//			// and the output reference given, and convert the
//			// existing model value
// </srcblock>
// A simple example to output the J2000 coordinates for a B1950 input (RA=20 deg,
// DEC=-10 deg):
// <srcblock>
//  cout << 	
//	MDirection::Convert( MDirection( Quantity( 20, "deg")
//			   		 Quantity(-10, "deg"),
//			     		 MDirection::Ref( MDirection::B1950)),
//			     MDirection::Ref( MDirection::J2000)) () << endl;
// </srcblock>
// In this example everything is done in one go (the () at the end does the
// conversion). Another example, to have a UTC to LAST converter:
// <srcblock>
// // Set up the model for the input (default reference is UTC)
//	MEpoch model (	Quantity(0., "d"));
// // Set up the frame with the observatory position
//	MPosition obs(	MVPosition(	Quantity( 10, "m"),
//					Quantity( -6, "deg"),
//					Quantity( 50, "deg")),
//			MPosition::Ref(MPosition::WGS84));
//	Measframe frame( obs);
// // set up the output reference
//	MEpoch::Ref outref(	MEpoch::LAST,
//				frame);
// // Set up conversion
//	MEpoch::Convert	toLST(	model,
//				outref);
// // Output a series of sidereal times (formatted in ddd::hh:mm:ss)
//	for (Double d = 12345; d<12346; d += 0.1) {
//	  cout << "Converted from UTC to LAST: " <<
//		d <<
//		toLST(d).getValue() << endl;
//	};
// </srcblock>
// <p>
// For specific purposes it would be very easy to set up a series of simple
// classes, that would do standard conversions. 
// <p>
// <h4> MeasData, MeasBase, other help classes</h4>
// A series of help classes are present to aid in the conversion, especially
// caching information. They are of no direct use for the end user (except
// maybe a few constants in MeasData, and some steering of details for
// advanced usage in them by the use of the MeasDetail class).<br>
// The classes are:
// <ul>
//  <li> <linkto class=MeasBase>MeasBase</linkto>:
//	base class (derived from Measure) for all real Measures
//  <li> <linkto class=MeasData>MeasData</linkto>:
//	all constants, polynomial factors, interface to IERS
//	database etc. which are not stored in Tables. (MeasTable looks after 
//	these). Mn short it provides all the actual data values necessary
//	for the conversions (and the other help classes)
//  <li> <linkto class=MeasIERS>MeasIERS</linkto>:
//	(static) class to converse with the IERS database(s)
//	(see <linkto class=MeasureReferenceData>MeasureReferenceData</linkto>).
//  <li> <linkto class=Precession>Precession</linkto>:
//	 all precession related calculations
//  <li> <linkto class=Nutation>Nutation</linkto>
//  <li> <linkto class=Aberration>Aberration</linkto>
//  <li> <linkto class=SolarPos>SolarPos</linkto>:
//	 all solarposition related calculations
//  <li> <linkto class=Euler>Euler</linkto>:
//	 representation of Euler rotation angles
//  <li> <linkto class=RotMatrix>RotMatrix</linkto>: a 3-D rotation matrix
//  <li> <linkto class=MeasDetail>MeasDetail</linkto>:
//	fine scale steering details
// </ul>
// <p>

// </synopsis> 
//
// <motivation>
// The Measures module originated to be able to convert ccordinates between
// different reference frames. The need for quantities to express the
// physical values of coordinates originated this way.
// Units were introduced in the described way to be able to handle any
// possible physical unit
// </motivation>
//
// <todo asof="960709">
//   <li> inlining
//   <li> look at the problem of rad*rad (which is, in general, not sr)
//   <li> Quantity input
// </todo>
//
// <example>
//  <h3> Known units on 960509 </h3>
// <srcblock>
// // UnitMap::list() will produce the following list:
//List all defined symbols
//
//Prefix table (20):
//    E         (exa)                        1e+18
//    G         (giga)                       1000000000
//    M         (mega)                       1000000
//    P         (peta)                       1e+15
//    T         (tera)                       1e+12
//    Y         (yotta)                      1e+24
//    Z         (zetta)                      1e+21
//    a         (atto)                       1e-18
//    c         (centi)                      0.01
//    d         (deci)                       0.1
//    da        (deka)                       10
//    f         (femto)                      1e-15
//    h         (hecto)                      100
//    k         (kilo)                       1000
//    m         (milli)                      0.001
//    n         (nano)                       1e-09
//    p         (pico)                       1e-12
//    u         (micro)                      1e-06
//    y         (yocto)                      1e-24
//    z         (zepto)                      1e-21
//Defining unit table (9):
//    A         (ampere)                     1 A
//    K         (kelvin)                     1 K
//    cd        (candela)                    1 cd
//    kg        (kilogram)                   1 kg
//    m         (metre)                      1 m
//    mol       (mole)                       1 mol
//    rad       (radian)                     1 rad
//    s         (second)                     1 s
//    sr        (steradian)                  1 sr
//SI unit table (48):
//    $         (currency)                   1
//    A         (ampere)                     1 A
//    AE        (astronomical unit)          149597870659 m
//    AU        (astronomical unit)          149597870659 m
//    Bq        (becquerel)                  1 s-1
//    C         (coulomb)                    1 s A
//    F         (farad)                      1 m-2 kg-1 s4 A2
//    Gy        (gray)                       1 m2 s-2
//    H         (henry)                      1 m2 kg s-2 A-2
//    Hz        (hertz)                      1 s-1
//    J         (joule)                      1 m2 kg s-2
//    Jy        (jansky)                     1e-26 kg s-2
//    K         (kelvin)                     1 K
//    L         (litre)                      0.001 m3
//    M0        (solar mass)                 1.98891944407e+30 kg
//    N         (newton)                     1 m kg s-2
//    Ohm       (ohm)                        1 m2 kg s-3 A-2
//    Pa        (pascal)                     1 m-1 kg s-2
//    S         (siemens)                    1 m-2 kg-1 s3 A2
//    S0        (solar mass)                 1.98891944407e+30 kg
//    Sv        (sievert)                    1 m2 s-2
//    T         (tesla)                      1 kg s-2 A-1
//    UA        (astronomical unit)          149597870659 m
//    V         (volt)                       1 m2 kg s-3 A-1
//    W         (watt)                       1 m2 kg s-3
//    Wb        (weber)                      1 m2 kg s-2 A-1
//    _         (dimensionless)              1
//    a         (year)                       31557600 s
//    arcmin    (arcmin)                     0.000290888208666 rad
//    arcsec    (arcsec)                     4.8481368111e-06 rad
//    as        (arcsec)                     4.8481368111e-06 rad
//    cd        (candela)                    1 cd
//    cy        (century)                    3155760000 s
//    d         (day)                        86400 s
//    deg       (degree)                     0.0174532925199 rad
//    g         (gram)                       0.001 kg
//    h         (hour)                       3600 s
//    l         (litre)                      0.001 m3
//    lm        (lumen)                      1 cd sr
//    lx        (lux)                        1 m-2 cd sr
//    m         (metre)                      1 m
//    min       (minute)                     60 s
//    mol       (mole)                       1 mol
//    pc        (parsec)                     3.08567758065e+16 m
//    rad       (radian)                     1 rad
//    s         (second)                     1 s
//    sr        (steradian)                  1 sr
//    t         (tonne)                      1000 kg
//Customary unit table (71):
//    "         (arcsec)                     4.8481368111e-06 rad
//    "_2       (square arcsec)              2.35044305391e-11 sr
//    '         (arcmin)                     0.000290888208666 rad
//    ''        (arcsec)                     4.8481368111e-06 rad
//    ''_2      (square arcsec)              2.35044305391e-11 sr
//    '_2       (square arcmin)              8.46159499408e-08 sr
//    :         (hour)                       3600 s
//    ::        (minute)                     60 s
//    :::       (second)                     1 s
//    Ah        (ampere hour)                3600 s A
//    Angstrom  (angstrom)                   1e-10 m
//    Btu       (British thermal unit (Int)) 1055.056 m2 kg s-2
//    CM        (metric carat)               0.0002 kg
//    Cal       (large calorie (Int))        4186.8 m2 kg s-2
//    FU        (flux unit)                  1e-26 kg s-2
//    G         (gauss)                      0.0001 kg s-2 A-1
//    Gal       (gal)                        0.01 m s-2
//    Gb        (gilbert)                    0.795774715459 A
//    Mx        (maxwell)                    1e-08 m2 kg s-2 A-1
//    Oe        (oersted)                    79.5774715459 m-1 A
//    R         (mile)                       0.000258 kg-1 s A
//    mile      (stokes)                     0.0001 m2 s-1
//    Torr      (torr)                       133.322368421 m-1 kg s-2
//    USfl_oz   (fluid ounce (US))           2.95735295625e-05 m3
//    USgal     (gallon (US))                0.003785411784 m3
//    WU        (WSRT flux unit)             5e-29 kg s-2
//    abA       (abampere)                   10 A
//    abC       (abcoulomb)                  10 s A
//    abF       (abfarad)                    1000000000 m-2 kg-1 s4 A2
//    abH       (abhenry)                    1e-09 m2 kg s-2 A-2
//    abOhm     (abohm)                      1e-09 m2 kg s-3 A-2
//    abV       (abvolt)                     1e-08 m2 kg s-3 A-1
//    ac        (acre)                       4046.8564224 m2
//    arcmin_2  (square arcmin)              8.46159499408e-08 sr
//    arcsec_2  (square arcsec)              2.35044305391e-11 sr
//    ata       (technical atmosphere)       98066.5 m-1 kg s-2
//    atm       (standard atmosphere)        101325 m-1 kg s-2
//    bar       (bar)                        100000 m-1 kg s-2
//    cal       (calorie (Int))              4.1868 m2 kg s-2
//    cwt       (hundredweight)              50.80234544 kg
//    deg_2     (square degree)              0.000304617419787 sr
//    dyn       (dyne)                       1e-05 m kg s-2
//    eV        (electron volt)              1.60217733e-19 m2 kg s-2
//    erg       (erg)                        0.001 kg s-2
//    fl_oz     (fluid ounce (Imp))          2.84130488996e-05 m3
//    ft        (foot)                       0.3048 m
//    fu        (flux unit)                  1e-26 kg s-2
//    fur       (furlong)                    201.168 m
//    gal       (gallon (Imp))               0.00454608782394 m3
//    ha        (hectare)                    10000 m2
//    hp        (horsepower)                 745.7 m2 kg s-3
//    in        (inch)                       0.0254 m
//    kn        (knot (Imp))                 0.514773333333 m s-1
//    lb        (pound (avoirdupois))        0.45359237 kg
//    ly        (light year)                 9.46073047e+15 m
//    mHg       (metre of mercury)           133322.387415 m-1 kg s-2
//    mile      (mile)                       1609.344 m
//    n_mile    (nautical mile (Imp))        1853.184 m
//    oz        (ounce (avoirdupois))        0.028349523125 kg
//    sb        (stilb)                      10000 m-2 cd
//    sq_arcmin (square arcmin)              8.46159499408e-08 sr
//    sq_arcsec (square arcsec)              2.35044305391e-11 sr
//    sq_deg    (square degree)              0.000304617419787 sr
//    statA     (statampere)                 3.33564095198e-10 A
//    statC     (statcoulomb)                3.33564095198e-10 s A
//    statF     (statfarad)                  1.11188031733e-12 m-2 kg-1 s4 A2
//    statH     (stathenry)                  899377374000 m2 kg s-2 A-2
//    statOhm   (statohm)                    899377374000 m2 kg s-3 A-2
//    statV     (statvolt)                   299.792458 m2 kg s-3 A-1
//    u         (atomic mass unit)           1.661e-27 kg
//    yd        (yard)                       0.9144 m
// </srcblock>
//
// </example>
// </module>

//# Dummy class definition for extractor
//# class Measures {};

#endif
