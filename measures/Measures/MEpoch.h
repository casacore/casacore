//# MEpoch.h: A Measure: instant in time
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2002
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

#ifndef MEASURES_MEPOCH_H
#define MEASURES_MEPOCH_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVEpoch.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MEpoch;
class MCEpoch;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;

//# Typedefs

// <summary>
// A Measure: instant in time
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
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
//	#include <casacore/measures/Measures.h>
//	#include <casacore/measures/Measures/MEpoch.h>
//	#include <casacore/measures/Measures/MCEpoch.h>
//      #include <casacore/casa/logging/LogIO.h>
//      
//	cout << "TAI for UTC = MJD(50237.29): " <<
//		MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))() <<
//		endl;
//      LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "readEphem"));
//      os << LogIO::DEBUG1 << " at ";
//      os.output() << MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))();
//      os << LogIO::POST;
// </srcblock>
// Results in:
// <srcblock>
//	TAI for UTC = MJD(50237.29): Epoch: 50237::06:58:06.0000 (on stdout)
//       at Epoch: 50237::06:58:06.0000 (in logger)
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2000/06/15">
//	<li>
// </todo>

class MEpoch : public MeasBase<MVEpoch, MeasRef<MEpoch> > {

public:
  //# Friends
  friend class MeasConvert<MEpoch>;
  
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
  // Measure value container for this class (i.e. MEpoch::MVType)
  typedef MVEpoch MVType;
  // Measure conversion routines for this class (i.e. MEpoch::MCType)
  typedef MCEpoch MCType;
  // Measure reference (i.e. MEpoch::Ref)
  typedef MeasRef<MEpoch> Ref;
  // Measure Convert (i.e. MEpoch::Convert)
  typedef MeasConvert<MEpoch> Convert;
  // Measure table Columns (e.g., MEpoch::ScalarColumn)
  typedef ScalarMeasColumn<MEpoch> ScalarColumn;
  typedef ArrayMeasColumn<MEpoch> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(MEpoch) Types Types;
    
  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. </note>
  // Default constructor; generates an instant at MJD 0 UTC
  MEpoch();
  // Create from data and reference
  // <group>
  MEpoch(const MVEpoch &dt);
  MEpoch(const MVEpoch &dt, const MEpoch::Ref &rf);
  MEpoch(const MVEpoch &dt, MEpoch::Types rf);
  MEpoch(const Quantity &dt);
  MEpoch(const Quantity &dt, const MEpoch::Ref &rf);
  MEpoch(const Quantity &dt, MEpoch::Types rf);
  MEpoch(const Measure *dt);
  MEpoch(const MeasValue *dt);
  // </group>
  
  //# Destructor
  virtual ~MEpoch();
  
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
  static MEpoch::Types castType(uInt tp);
  static const String &showType(MEpoch::Types tp);
  static const String &showType(uInt tp);
  // </group>
  // Translate string to reference code
  // <group>
  static Bool getType(MEpoch::Types &tp, const String &in);
  Bool giveMe(MEpoch::Ref &mr, const String &in);
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


} //# NAMESPACE CASACORE - END

#endif
