//# Quanta.h:  a module for units and quantities
//# Copyright (C) 1998,1999,2000,2004
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

#ifndef CASA_QUANTA_H
#define CASA_QUANTA_H

//# Includes
#include <casacore/casa/aips.h>

#include <casacore/casa/Quanta/Unit.h>
//# Next one at this place
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
// 

// <summary> a module for units and quantities </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tUnit tQuantum"
//	 demos="dMUString">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <etymology>
// The name Quanta derives from a physical quantity, i.e. a value with
// units attached.
// </etymology>
//
// <synopsis> 
// The Quanta model deals with units and physical quantities
// (i.e. values with a unit).
// Units are handled in the <a href="#Unit">Unit</a> section
// (see <linkto class="Unit">Unit.h</linkto>). 
// Quantities are handled in the <a href="#Quantum">Quantum</a> section
// (see <linkto class="Quantum">Quantum.h</linkto>).
// In addition the module contains some more general support classes
// (<linkto class=Euler>Euler</linkto> angles,
// <linkto class=RotMatrix>rotation matrix</linkto>,
// <linkto class=MUString>pointed string</linkto>), formatting for
// <linkto class=MVTime>time</linkto> and <linkto class=MVAngle>angle</linkto>
// classes and classes containing information for
// Measures (<linkto class=MeasValue>MeasValue</linkto> and the derived MV
// classes like <linkto class=MVEpoch>MVEpoch</linkto>). See the
// <a href="#MeasValue">MeasValue</a> section.
//
// <h3> Includes</h3>
// Including the <src>casa/Quanta.h</src> will take care of all
// includes necessary for the handling of pure Units and Quantities.
//
//  <anchor name="Unit"><h3> Physical units </h3></anchor>
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
// units can be added by the <src>UnitMap::addFITS()</src> function. For 
// details, see <linkto class="UnitMap">UnitMap</linkto>.
//
// Example:
// <srcblock>
//	km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
// </srcblock>
// There are 5 name lists in the UnitMap, which are searched in reverse order:
// <ol>
//   <li> Defining units: 	m, kg, s, A, K, cd, mol, rad, sr, _
//   <li> SI units:		including a.o. g, Jy, AU
//   <li> Customary units:	e.g. lb, hp, ly
//   <li> User defined units:	defined by user (e.g. beam, KPH, KM)
//   <li> Cached units:	for speed in operations
// </ol>
// All known names can be viewed by running the tUnit test program, or
// using the MapUnit::list() routine.
//
// The definitions that were current on 990915 are given at end of this file
// 
// <note role=caution>
// There is a difference between units without a dimension (non-dimensioned
// I will call them), and undimensioned units. Non-dimensioned examples are
// "", "%"; undimensioned examples: "beam", "pixel".
// </note>
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
//  <anchor name="Quantum"><h3> Quantums and Quantities </h3></anchor>
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
//   <li> <src> void assure(UnitVal)</src>
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
//   <li>  <src>Quantum<Double> HI;	// Frequency HI line</src>
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
// <p>
//  <anchor name="MeasValue"><h3> Values for Measures </h3></anchor>
// The MeasValue class derivatives are all named <em>MVmeasure</em>, e.g.
// <em>MVFrequency</em>, and represent the internal representation of the
// specific measure class. There main use is for the Measures module,
// but they can be used alone, e.g. for the conversion to formatted times,
// or the conversion of frequencies from say wavelength to frequency.
// They all have at least the following constructors:
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
// like <linkto class=MVEpoch>MVEpoch</linkto>,
// <linkto class=MVDirection>MVDirection</linkto>,
// <linkto class=MVPosition>MVPosition</linkto>,
// <linkto class=MVFrequency>MVFrequency</linkto>,
// <linkto class=MVDouble>MVDouble</linkto>,
// <linkto class=MVRadialVelocity>MVRadialVelocity</linkto>.
// <linkto class=MVBaseline>MVBaseline</linkto>,
// <linkto class=MVuvw>MVuvw</linkto>,
// <linkto class=MVEarthMagnetic>MVEarthMagnetic</linkto>,
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

// </synopsis> 
//
// <motivation>
// The Quanta model originated to handle physical quantities independent of their
// units.
// Units were introduced in the described way to be able to handle any
// possible physical unit.
// </motivation>
//
// <todo asof="1998/07/22">
//   <li> inlining
//   <li> look at the problem of rad*rad (which is, in general, not sr)
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
//Defining unit table (10):
//    A         (ampere)                     1 A
//    K         (kelvin)                     1 K
//    _         (undimensioned)              1 _
//    cd        (candela)                    1 cd
//    kg        (kilogram)                   1 kg
//    m         (metre)                      1 m
//    mol       (mole)                       1 mol
//    rad       (radian)                     1 rad
//    s         (second)                     1 s
//    sr        (steradian)                  1 sr
//SI unit table (50):
//    $         (currency)                   1 _
//    %         (percent)                    0.01
//    %%        (permille)                   0.001
//    A         (ampere)                     1 A
//    AE        (astronomical unit)          149597870659 m
//    AU        (astronomical unit)          149597870659 m
//    Bq        (becquerel)                  1 s-1
//    C         (coulomb)                    1 s.A
//    F         (farad)                      1 m-2.kg-1.s4.A2
//    Gy        (gray)                       1 m2.s-2
//    H         (henry)                      1 m2.kg.s-2.A-2
//    Hz        (hertz)                      1 s-1
//    J         (joule)                      1 m2.kg.s-2
//    Jy        (jansky)                     1e-26 kg.s-2
//    K         (kelvin)                     1 K
//    L         (litre)                      0.001 m3
//    M0        (solar mass)                 1.98891944407e+30 kg
//    N         (newton)                     1 m.kg.s-2
//    Ohm       (ohm)                        1 m2.kg.s-3.A-2
//    Pa        (pascal)                     1 m-1.kg.s-2
//    S         (siemens)                    1 m-2.kg-1.s3.A2
//    S0        (solar mass)                 1.98891944407e+30 kg
//    Sv        (sievert)                    1 m2.s-2
//    T         (tesla)                      1 kg.s-2.A-1
//    UA        (astronomical unit)          149597870659 m
//    V         (volt)                       1 m2.kg.s-3.A-1
//    W         (watt)                       1 m2.kg.s-3
//    Wb        (weber)                      1 m2.kg.s-2.A-1
//    _         (undimensioned)              1 _
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
//    lm        (lumen)                      1 cd.sr
//    lx        (lux)                        1 m-2.cd.sr
//    m         (metre)                      1 m
//    min       (minute)                     60 s
//    mol       (mole)                       1 mol
//    pc        (parsec)                     3.08567758065e+16 m
//    rad       (radian)                     1 rad
//    s         (second)                     1 s
//    sr        (steradian)                  1 sr
//    t         (tonne)                      1000 kg
//Customary unit table (74):
//    "         (arcsec)                     4.8481368111e-06 rad
//    "_2       (square arcsec)              2.35044305391e-11 sr
//    '         (arcmin)                     0.000290888208666 rad
//    ''        (arcsec)                     4.8481368111e-06 rad
//    ''_2      (square arcsec)              2.35044305391e-11 sr
//    '_2       (square arcmin)              8.46159499408e-08 sr
//    :         (hour)                       3600 s
//    ::        (minute)                     60 s
//    :::       (second)                     1 s
//    Ah        (ampere hour)                3600 s.A
//    Angstrom  (angstrom)                   1e-10 m
//    Btu       (British thermal unit (Int)) 1055.056 m2.kg.s-2
//    CM        (metric carat)               0.0002 kg
//    Cal       (large calorie (Int))        4186.8 m2.kg.s-2
//    FU        (flux unit)                  1e-26 kg.s-2
//    G         (gauss)                      0.0001 kg.s-2.A-1
//    Gal       (gal)                        0.01 m.s-2
//    Gb        (gilbert)                    0.795774715459 A
//    Mx        (maxwell)                    1e-08 m2.kg.s-2.A-1
//    Oe        (oersted)                    79.5774715459 m-1.A
//    R         (mile)                       0.000258 kg-1.s.A
//    St        (stokes)                     0.0001 m2.s-1
//    Torr      (torr)                       133.322368421 m-1.kg.s-2
//    USfl_oz   (fluid ounce (US))           2.95735295625e-05 m3
//    USgal     (gallon (US))                0.003785411784 m3
//    WU        (WSRT flux unit)             5e-29 kg.s-2
//    abA       (abampere)                   10 A
//    abC       (abcoulomb)                  10 s.A
//    abF       (abfarad)                    1000000000 m-2.kg-1.s4.A2
//    abH       (abhenry)                    1e-09 m2.kg.s-2.A-2
//    abOhm     (abohm)                      1e-09 m2.kg.s-3.A-2
//    abV       (abvolt)                     1e-08 m2.kg.s-3.A-1
//    ac        (acre)                       4046.8564224 m2
//    arcmin_2  (square arcmin)              8.46159499408e-08 sr
//    arcsec_2  (square arcsec)              2.35044305391e-11 sr
//    ata       (technical atmosphere)       98066.5 m-1.kg.s-2
//    atm       (standard atmosphere)        101325 m-1.kg.s-2
//    bar       (bar)                        100000 m-1.kg.s-2
//    beam      (undefined beam area)        1 _
//    cal       (calorie (Int))              4.1868 m2.kg.s-2
//    cwt       (hundredweight)              50.80234544 kg
//    deg_2     (square degree)              0.000304617419787 sr
//    dyn       (dyne)                       1e-05 m.kg.s-2
//    eV        (electron volt)              1.60217733e-19 m2.kg.s-2
//    erg       (erg)                        1e-07 m2.kg.s-2
//    fl_oz     (fluid ounce (Imp))          2.84130488996e-05 m3
//    ft        (foot)                       0.3048 m
//    fu        (flux unit)                  1e-26 kg.s-2
//    fur       (furlong)                    201.168 m
//    gal       (gallon (Imp))               0.00454608782394 m3
//    ha        (hectare)                    10000 m2
//    hp        (horsepower)                 745.7 m2.kg.s-3
//    in        (inch)                       0.0254 m
//    kn        (knot (Imp))                 0.514773333333 m.s-1
//    lb        (pound (avoirdupois))        0.45359237 kg
//    ly        (light year)                 9.46073047e+15 m
//    mHg       (metre of mercury)           133322.387415 m-1.kg.s-2
//    mile      (mile)                       1609.344 m
//    n_mile    (nautical mile (Imp))        1853.184 m
//    oz        (ounce (avoirdupois))        0.028349523125 kg
//    pixel     (pixel)                      1 _
//    sb        (stilb)                      10000 m-2.cd
//    sq_arcmin (square arcmin)              8.46159499408e-08 sr
//    sq_arcsec (square arcsec)              2.35044305391e-11 sr
//    sq_deg    (square degree)              0.000304617419787 sr
//    statA     (statampere)                 3.33564095198e-10 A
//    statC     (statcoulomb)                3.33564095198e-10 s.A
//    statF     (statfarad)                  1.11188031733e-12 m-2.kg-1.s4.A2
//    statH     (stathenry)                  899377374000 m2.kg.s-2.A-2
//    statOhm   (statohm)                    899377374000 m2.kg.s-3.A-2
//    statV     (statvolt)                   299.792458 m2.kg.s-3.A-1
//    debye     (electric dipole moment)     10-18 statC.cm
//    u         (atomic mass unit)           1.661e-27 kg
//    yd        (yard)                       0.9144 m
//    yr        (year)                       31557600 s
// </srcblock>
//
// </example>
// </module>

//# Dummy class definition for extractor
//# class Quanta {};


} //# NAMESPACE CASACORE - END

#endif



