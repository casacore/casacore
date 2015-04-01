//# Measures.h:  a module for coordinates
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2002
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

#ifndef MEASURES_MEASURES_H
#define MEASURES_MEASURES_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
// 

// <summary> a module for coordinates </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasMath tMeasure"
//	 demos="dMeasure">

// <prerequisite>
//   <li> <linkto module=Quanta>Quanta</linkto> module for units and quantities.
// </prerequisite>

// <etymology>
// The name Measure derives from physical measurements, i.e. values with
// units and possibly a reference frame attached.
// </etymology>
//
// <synopsis> 
// The Measure model deals with measures (i.e. quantities with a
// reference frame).
// Measures are handled in the <a href="#Measure">Measure</a> section
// (see <linkto class="Measure">Measure.h</linkto>).
//
// <h3> Includes</h3>
// Including the <src>measures/Measures.h</src> will take care of all
// includes necessary for the handling of Units and Quantities, and the
// general Measure interface. For the use of individual Measures, the
// appropiate include files should be added. E.g. to be able to handle
// Directions, the following includes could be given:
// <srcblock>
//	#include <casacore/measures/Measures.h>
//	#include <casacore/measures/Measures/MDirection.h>
// </srcblock>
// An inclusion of the appropiate measure file, will also take care of the
// connected measure value (in this case <src>MVDirection</src>). However,
// if only the value suffices, it can be included on its own (from the
// Quanta directory).<br>
// When doing actual conversions (see MeasConvert later on), by using the
// explicit Measure::Convert types, the description of the actual
// conversions (called MCmeasure, e.g. MCEpoch.h) should be included as well;
// in adition to general MeasConvert.h. 
//
//  <anchor name="Measure"><h3> Measures</h3></anchor>
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
//		radial velocity
//   <li> <linkto class="MBaseline">MBaseline</linkto>: a baseline
//   <li> <linkto class="Muvw">Muvw</linkto>: a uvw value
//   <li> <linkto class="MEarthMagnetic">MEarthMagnetic</linkto>: an
//		 earth' magnetic field value
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
// <linkto module="Quanta">Quanta</linkto> module classes.
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
// MeasValue and the MV classes can be found in the
// <linkto module=Quanta>Quanta</linkto> module.
// In addition some other value classes, not directly used in measures, are
// available. Examples:
// <ul>
//  <li> <linkto class=MVAngle>MVAngle</linkto> (to normalise
// and have specific I/O formatting for angle-like values)
// <li> <linkto class=MVTime>MVTime</linkto> (same for time-like values)
// </ul>
// <em>References</em> are measure specific. Each specific reference class is
// called <em>Measure</em>\::Ref (e.g. <src>MEpoch::Ref</src>). It specifies
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
// class is called <em>Measure</em>\::Convert (e.g. MDirection::Convert).
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
// use the IERS updates, some maybe the JPL DE databases).<br>
// The <linkto class=AipsrcValue>AipsrcValue</linkto> class can be used to
// specify very specific parameters that are used to steer
// the conversion process beyond what is possible with just a list
// of measure reference types (that list is already long for some cases).
// Values, switches can be <src>set()</src> (and removed) to change the
// default behaviour of the conversions. In general the user will only need
// to use the details in very specific cases. The details that can be used
// are described in the classes that provide calculations (e.g.
// <linkto class=Nutation>Nutation</linkto>), and in the aipsrc-data reference
// manual entry.<br>
// <p>
// Some details about the different classes follows. In the examples often
// a specific measure value (e.g. MVEpoch, the MeasValue for MEpoch), or a
// specific measure (e.g. MDirection, a direction in space) is used. This
// is only to visualise the use, any other measure could have been used.
// <p>
// <h4> MeasValue</h4>
// The MeasValue class derivatives are all named <em>MVmeasure</em>, e.g.
// <em>MVFrequency</em>, and represent the internal representation of the
// specific measure class.  Details
// can be found in the <linkto module=Quanta>Quanta</linkto> module.
// <p>
// <h4> Measure</h4>
// The Measure class derivatives are all called <em>MMeasure</em>.
// <linkto class=MDirection>MDirection</linkto> (a celestial direction),
// <linkto class=MPosition>MPosition</linkto> (a position on Earth),
// <linkto class=MFrequency>MFrequency</linkto> (characteristics of 
// 	electro-magnetic wave),
// <linkto class=MEpoch>MEpoch</linkto> (an instance in time),
// <linkto class=MDoppler>MDoppler</linkto>,
// <linkto class=MRadialVelocity>MRadialVelocity</linkto>
// <linkto class=MBaseline>MBaseline</linkto>,
// <linkto class=Muvw>Muvw</linkto>,
// <linkto class=MEarthMagnetic>MEarthMagnetic</linkto>,
//. <br>
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
// <src>void assured(String)</src> and <src>Bool areYou(String)</src> will
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
// <em>Measure::Convert</em> (e.g. <em>MEpoch::Ref, MEpoch::Convert</em>). In
// addition, Measure::MVType and Measure::MCType are defined for all
// measures.
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
// for each of the defined Casacore standard values a corresponding E class
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
// <h4> MeasData, MeasTable, MeasBase, other help classes</h4>
// A series of help classes are present to aid in the conversion, especially
// caching information. They are of no direct use for the end user (except
// maybe a few constants in MeasData).<br>
// The classes are:
// <ul>
//  <li> <linkto class=MeasBase>MeasBase</linkto>:
//	base class (derived from Measure) for all real Measures
//  <li> <linkto class=MeasData>MeasData</linkto>:
//	all constants, polynomial factors, interface to IERS
//	database etc. which are not stored in Tables. (MeasTable looks after 
//	these). Mn short it provides all the actual data values necessary
//	for the conversions (and the other help classes)
//  <li> <linkto class=MeasTable>MeasTable</linkto>:
//	interface for all data that comes from Tables rather than
//	the program
//  <li> <linkto class=MeasIERS>MeasIERS</linkto>:
//	(static) class to converse with the IERS database(s)
//  <li> <linkto class=MeasJPL>MeasJPL</linkto>:
//	(static) class to converse with the JPL DE database(s)
//  <li> <linkto class=Precession>Precession</linkto>:
//	 all precession related calculations
//  <li> <linkto class=Nutation>Nutation</linkto>
//  <li> <linkto class=Aberration>Aberration</linkto>
//  <li> <linkto class=SolarPos>SolarPos</linkto>:
//	 all solarposition related calculations
//  <li> <linkto class=Euler>Euler</linkto>:
//	 representation of Euler rotation angles
//  <li> <linkto class=RotMatrix>RotMatrix</linkto>: a 3-D rotation matrix
// </ul>
// <p>

// </synopsis> 
//
// <motivation>
// The Measures module originated to be able to convert ccordinates between
// different reference frames.
// </motivation>
//
// <todo asof="1998/07/22">
//   <li> inlining
// </todo>
//
// <example>
// See the individual measures for appropiate examples.
// </example>
// </module>

//# Dummy class definition for extractor
//# class Measures {};


} //# NAMESPACE CASACORE - END

#endif
