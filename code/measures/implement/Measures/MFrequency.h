//# MFrequency.h: A Measure: wave characteristics
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

#if !defined(AIPS_MFREQUENCY_H)
#define AIPS_MFREQUENCY_H

#if defined(_AIX)
#pragma implementation ("MFrequency.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MVFrequency.h>

//# Forward Declarations
class MFrequency;
class MDoppler;

//# Typedefs

// <summary>
// A Measure: wave characteristics
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
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
// interpreted as a frequency in Hz, and a reference, with an LSR type
// as default.<br>
// It can also be generated from a Quantity, where the interpretation
// depends on the dimensionality of the Quantity:
// <ul>
// <it> time (e.g. s): period
// <it> frequency (e.g. Hz): frequency
// <it> angular frequency (e.g. arcmin/s): angular frequency
// <it> length (e.g. cm): wavelength
// <it> inverse length (e.g. mm<sup>-1</sup>): wave number
// <it> energy (e.g. J.s): energy (i.e. <em>h.nu</em>)
// <it> momentum (e.g. kg.m): <em>m.c/h</em>
// </ul>
// The different reference types of a frequency are:
// <ul>
//  <li> MFrequency::REST -- Rest frequency
//  <li> MFrequency::LSR --- Local Standard of Rest (J2000) -- as the
//		dynamical definition (IAU, [9,12,7] km/s in galactic coordinates
//  <li> MFrequency::LSRK -- LSR as a kinematical definition -- 19.4 km/s
//		in direction ra,dec = [270,+30] deg (B1900)
//  <li> MFrequency::BARY -- Barycentric (J2000)
//  <li> MFrequency::GEO --- Geocentric
//  <li> MFrequency::TOPO -- Topocentric
//  <li> MFrequency::GALACTO -- Galacto centric (with rotation of 220 km/s
//		in direction l,b = [90,0] deg.
//  <li> MFrequency::DEFAULT = LSR
// <ul>
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
// <todo asof="1996/10/10">
// </todo>

class MFrequency : public MeasBase<MVFrequency,MeasRef<MFrequency> >
{
public:
//# Friends
// Conversion of data
    friend class MeasConvert<MFrequency,MVFrequency>;

//# Enumerations
// Types of known MFrequencies
// <note> The order defines the order in the translation matrix FromTo
// in the getConvert routine. Do not change the order without
// changing the array. Additions should be made before N_types, and
// an additional row and column should be coded in FromTo, and
// in showType().</note>
    enum Types {REST,
		LSR, 
		LSRK,
		BARY,
		GEO,
		TOPO,
		GALACTO,
		N_Types,
		DEFAULT=LSR};

// The list of actual routines provided.
// <note> For each <src>AA_BB</src> in the list a routine
// <src>static void AAtoBB(MVFrequency &)</src> should be provided. The routines
// should be listed in the FromToRout array in the getConvert routine, in the
// order specified. In addition the type to which converted should be in the
// ToRef array, again in the proper order. </note>
    enum Routes {
      LSR_BARY,
      BARY_LSR,
      BARY_GEO,
      GEO_TOPO,
      GEO_BARY,
      TOPO_GEO,
      LSR_GALACTO,
      GALACTO_LSR,
      LSRK_BARY,
      BARY_LSRK,
      REST_LSR,
      LSR_REST,
      N_Routes };

//# Typedefs
// Measure reference
    typedef MeasRef<MFrequency> Ref;
// Measure conversion use
    typedef MeasConvert<MFrequency,MVFrequency> Convert;

//# Constructors
// <note> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. For reasons
// of compiler limitations the formal arguments had to be specified as
// <em>uInt</em> rather than the Measure enums that should be used as actual 
// arguments.</note>
// Default constructor; generates a zero rest frequency
    MFrequency();
// Create from data and reference
// <group>
    MFrequency(const MVFrequency &dt);
    MFrequency(const MVFrequency &dt, const MFrequency::Ref &rf);
    MFrequency(const MVFrequency &dt, uInt rf);
    MFrequency(const Quantity &dt);
    MFrequency(const Quantity &dt, const MFrequency::Ref &rf);
    MFrequency(const Quantity &dt, uInt rf);
// </group>

//# Destructor
    ~MFrequency();

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
    Bool giveMe(const String &in, MFrequency::Ref &mr);
// Get frequency in specified units
    Quantity get(const Unit &un) const;

// Make a Doppler velocity from the frequency and the specified rest frequency
    MDoppler toDoppler(const MVFrequency &rest);

// Make a frequency from the Doppler velocity and the specified rest frequency
// (default reference type LSR)
// <group>
    static MFrequency fromDoppler(const MDoppler &dop,
				  const MVFrequency &rest);
    static MFrequency fromDoppler(const MDoppler &dop,
				  const MVFrequency &rest,
				  MFrequency::Types type);
// </group>

// Make a rest frequency using a Doppler velocity
    MFrequency toRest(const MDoppler &dop);

// Make a copy
    virtual void *clone() const;

private:
//# Enumerations
// Usage of the MeasConvert structure cache. Additions should fit into the
// space provided in MeasConvert (see <src>MC_N_Struct</src> constant),
// and should be coded in the <src>clearConvert()</src> method.
    enum StructUse {
      MVPOS1, MVDIR1,
      ABERFROM, ABERTO,
      N_StructUse };

//# Data

//# Member functions
// Create conversion function pointer
    static void getConvert(MFrequency::Convert &mc,
			   const MFrequency::Ref &inref, 
			   const MFrequency::Ref &outref);

// Create help structures for Measure conversion routines
    static void initConvert(uInt which, MFrequency::Convert &mc);

// Delete the pointers used in the MeasConvert help structure cache
     static void clearConvert(MFrequency::Convert &mc);

// Routine to convert frequency from one reference frame to another
    static void doConvert(MVFrequency &in,
			  const MFrequency::Ref &inref,
			  const MFrequency::Ref &outref,
			  const MFrequency::Convert &mc);

};

#endif
