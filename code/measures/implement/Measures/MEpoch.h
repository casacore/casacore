//# MEpoch.h: A Measure: instant in time
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

#if !defined(AIPS_MEPOCH_H)
#define AIPS_MEPOCH_H

#if defined(_AIX)
#pragma implementation ("MEpoch.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MVEpoch.h>

//# Forward Declarations
class MEpoch;

//# Typedefs

// <summary>
// A Measure: instant in time
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class
// </prerequisite>
//
// <etymology>
// Measure and Epoch
// </etymology>
//
// <synopsis>
// MEpoch forms derived Measure class for an instant in time.
// </synopsis>
//
// <example>
// Convert (with all steps explicit) a UTC to an IAT time.
// <srcblock>
//	cout << "TAI for UTC = MJD(50237.29): " <<
//		MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))() <<
//		endl;
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1996/02/21">
//	<li>
// </todo>

class MEpoch : public MeasBase<MVEpoch,MeasRef<MEpoch> >
{
public:
//# Friends
// Conversion of data
   friend class MeasConvert<MEpoch,MVEpoch>;
//# Enumerations
// Types of known MEpochs
// <note> The order defines the order in the translation matrix FromTo
// in the getConvert routine. Do not change the order without
// changing the array. Additions should be made before N_types, and
// an additional row and column should be coded in FromTo, and
// in showType().</note>
    enum Types {
      // Local Apparent Sidereal Time
      LAST,
      // Local Mean Sidereal Time
      LMST,
      // Greenwich Mean ST1
      GMST1,
      GAST,
      UT1,
      UT2,
      UTC,
      TAI,
      TDT,
      TCG,
      TDB,
      TCB,
      // Number of types
      N_Types,
      // Reduce result to integer days
      RAZE = 32,
      // All extra bits
      EXTRA = RAZE,
      // Synonyms
      IAT=TAI,	
      GMST=GMST1,
      TT=TDT,
      UT=UT1,
      // Default
      DEFAULT=UTC
    };
// The list of actual routines provided.
// <note> For each <src>AA_BB</src> in the list a routine
// <src>static void AAtoBB(MVEpoch &)</src> should be provided. The routines
// should be listed in the FromToRout array in the getConvert routine, in the
// order specified. In addition the type to which converted should be in the
// ToRef array, again in the proper order. </note>
    enum Routes {
	LAST_GAST,
	GAST_LAST,
	LMST_GMST1,
	GMST1_LMST,
	GMST1_UT1,
	UT1_GMST1,
	GAST_UT1,
	UT1_GAST,
	UT1_UTC,
	UTC_UT1,
	UT1_UT2,
	UT2_UT1,
	UTC_TAI,
	TAI_UTC,
	TAI_TDT,
	TDT_TAI,
	TDT_TDB,
	TDB_TDT,
	TDT_TCG,
	TCG_TDT,
	TDB_TCB,
	TCB_TDB,
	N_Routes,
	RAZING};

//# Typedefs
// Measure reference
    typedef class MeasRef<MEpoch> Ref;
// Measure Convert
    typedef class MeasConvert<MEpoch,MVEpoch> Convert;

//# Constructors
// <note> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. For reasons
// of compiler limitations the formal arguments had to be specified as
// <em>uInt</em> rather than the Measure enums that should be used as actual 
// arguments.</note>
// Default constructor; generates an instant at MJD 0 TAI
    MEpoch();
// Create from data and reference
// <group>
    MEpoch(const MVEpoch &dt);
    MEpoch(const MVEpoch &dt, const MEpoch::Ref &rf);
    MEpoch(const MVEpoch &dt, uInt rf);
    MEpoch(const Quantity &dt);
    MEpoch(const Quantity &dt, const MEpoch::Ref &rf);
    MEpoch(const Quantity &dt, uInt rf);
// </group>

//# Destructor
    ~MEpoch();

//# Operators

//# General Member Functions
// Tell me your type
// <group>
    virtual const String &tellMe() const;
    static const String &showMe();
// </group>
// Translate reference code
    static const String &showType(uInt tp);
// Make reference code from String
    Bool giveMe(const String &in, MEpoch::Ref &mr);
// Get time in specified units
     Quantity get(const Unit &inunit) const;

// Create copy
    virtual void *clone() const;

private:
//# Enumerations
// Usage of the MeasConvert structure cache. Additions should fit into the
// space provided in MeasConvert (see <src>MC_N_Struct</src> constant),
// and should be coded in the <src>clearConvert()</src> method.
    enum StructUse {
	NUTATFROM, NUTATTO,
	N_StructUse };

//# Data

//# Member functions

// Create conversion function pointer
    static void getConvert(MEpoch::Convert &mc,
			   const MEpoch::Ref &inref,
			   const MEpoch::Ref &outref);

// Create help structures for Measure conversion routines
    static void initConvert(uInt which, MEpoch::Convert &mc);

// Delete the pointers used in the MeasConvert help structure cache
     static void clearConvert(MEpoch::Convert &mc);

// Routine to convert time from one reference frame to another
    static void doConvert(MVEpoch &in,
			  const MEpoch::Ref &inref,
			  const MEpoch::Ref &outref,
			  const MEpoch::Convert &mc);
};

#endif
