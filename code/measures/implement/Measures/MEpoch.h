//# MEpoch.h: A Measure: instant in time
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

#if !defined(AIPS_MEPOCH_H)
#define AIPS_MEPOCH_H

#if defined(_AIX)
#pragma implementation ("MEpoch.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Quanta/MVEpoch.h>

//# Forward Declarations
class MEpoch;
class MCEpoch;
template <class M, class F, class MC> class MeasConvert;

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
//	#include <aips/Measures.h>
//	#include <aips/Measures/MEpoch.h>
//	cout << "TAI for UTC = MJD(50237.29): " <<
//		MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))() <<
//		endl;
// </srcblock>
// Results in:
// <srcblock>
//	TAI for UTC = MJD(50237.29): Epoch: 50237::06:58:06.0000
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1997/04/15">
//	<li>
// </todo>

class MEpoch : public MeasBase<MVEpoch,MeasRef<MEpoch> > {

public:
  //# Friends
  friend class MeasConvert<MEpoch, MVEpoch, MCEpoch>;
  
  //# Enumerations
  // Types of known MEpochs
  // <note role=caution> The order defines the order in the translation matrix
  // in the MCEpoch class. Do not change the order without
  // changing the array. Additions should be made before N_types, and
  // an additional row and column should be coded in FromTo (MCEpoch), and
  // in showType().</note>
  enum Types {
    // Local Apparent Sidereal Time
    LAST,
    // Local Mean Sidereal Time
    LMST,
    // Greenwich Mean ST1
    GMST1,
    // Greenwich Apparent ST
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
    ET=TT,
    // Default
    DEFAULT=UTC
  };
  
  //# Typedefs
  // Measure reference (i.e. MEpoch::Ref)
  typedef class MeasRef<MEpoch> Ref;
  // Measure Convert (i.e. MEpoch::Convert)
  typedef class MeasConvert<MEpoch,MVEpoch,MCEpoch> Convert;
  
  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. For reasons
  // of compiler limitations the formal arguments had to be specified as
  // <em>uInt</em> rather than the Measure enums that should be used as actual 
  // arguments.</note>
  // Default constructor; generates an instant at MJD 0 UTC
  MEpoch();
  // Create from data and reference
  // <group>
  MEpoch(const MVEpoch &dt);
  MEpoch(const MVEpoch &dt, const MEpoch::Ref &rf);
  MEpoch(const MVEpoch &dt, uInt rf);
  MEpoch(const Quantity &dt);
  MEpoch(const Quantity &dt, const MEpoch::Ref &rf);
  MEpoch(const Quantity &dt, uInt rf);
  MEpoch(const Measure *dt);
  MEpoch(const MeasValue *dt);
  // </group>
  
  //# Destructor
  ~MEpoch();
  
  //# Operators
  
  //# General Member Functions
  // Tell me your type
  // <group>
  virtual const String &tellMe() const;
  static const String &showMe();
  virtual uInt type() const;
  static void assert(const Measure &in);
  // </group>
  // Translate reference code
  static const String &showType(uInt tp);
  // Translate string to reference code
  // <group>
  static Bool getType(MEpoch::Types &tp, const String &in);
  Bool giveMe(MEpoch::Ref &mr, const String &in);
  // This one for historic reasons only
  Bool giveMe(const String &in, MEpoch::Ref &mr);
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
  // Get my type (as Register)
  static uInt myType();

  // Get time in specified units
  Quantity get(const Unit &inunit) const;
  
  // Create copy
  // <group>
  virtual Measure *clone() const;
  // </group>
  
private:
  //# Enumerations
  
  //# Data
  
  //# Member functions
  
};

#endif
