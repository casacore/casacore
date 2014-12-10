//# MPosition.h: A Measure: position on Earth
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

#ifndef MEASURES_MPOSITION_H
#define MEASURES_MPOSITION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MPosition;
class MCPosition;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;

//# Typedefs

// <summary>
//  A Measure: position on Earth
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
// MPosition forms derived Measure class for an instant in time.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2000/06/15">
//	<li>
// </todo>

class MPosition : public MeasBase<MVPosition, MeasRef<MPosition> > {

 public:
  //# Friends
  // Conversion of data
  friend class MeasConvert<MPosition>;

  //# Enumerations
  // Types of known MPositions
  // <note role=warning> The order defines the order in the translation
  // matrix FromTo
  // in the getConvert routine. Do not change the order without
  // changing the array. Additions should be made before N_types, and
  // an additional row and column should be coded in FromTo, and
  // in showType().</note>
  enum Types {
    ITRF,
    WGS84,
    N_Types,
    DEFAULT=ITRF};

  //# Typedefs
  // Measure value container for this class (i.e. MPosition::MVType)
  typedef MVPosition MVType;
  // Measure conversion routines for this class (i.e. MPosition::MCType)
  typedef MCPosition MCType;
  // Measure reference (i.e. MPosition::Ref)
  typedef MeasRef<MPosition> Ref;
  // Measure Convert (i.e. MPosition::Convert)
  typedef MeasConvert<MPosition> Convert;
  // Measure table Columns (e.g., MPosition::ScalarColumn)
  typedef ScalarMeasColumn<MPosition> ScalarColumn;
  typedef ArrayMeasColumn<MPosition> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(MPosition) Types Types;

  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. </note>
  // Default constructor; generates the ITRF centre
  MPosition();
  // Create from data and reference
  // <group>
  MPosition(const MVPosition &dt);
  MPosition(const MVPosition &dt, const MPosition::Ref &rf);
  MPosition(const MVPosition &dt, MPosition::Types rf);
  MPosition(const Quantity &dt, const Quantity &dt1, const Quantity &dt2);
  MPosition(const Quantity &dt, const Quantity &dt1, const Quantity &dt2,
	    const MPosition::Ref &rf);
  MPosition(const Quantity &dt, const Quantity &dt1, const Quantity &dt2,
	    MPosition::Types rf);
  MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt);
  MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt, 
	    const MPosition::Ref &rf);
  MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt, 
	    MPosition::Types rf);
  MPosition(const Measure *dt);
  MPosition(const MeasValue *dt);
  // </group>
  
  // <group>
  MPosition(const MPosition &);
  MPosition &operator=(const MPosition &);
  // </group>
  
  //# Destructor
  virtual ~MPosition();
  
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
  static MPosition::Types castType(uInt tp);
  static const String &showType(MPosition::Types tp);
  static const String &showType(uInt tp);
  // </group>
  // Translate string to reference code
  // <group>
  static Bool getType(MPosition::Types &tp, const String &in);
  // this one throws an exception for an unrecognized String
  static MPosition::Types getType(const String& in);

  Bool giveMe(MPosition::Ref &mr, const String &in);
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
  
  // Get Measure data
  // <group>
  Quantum<Vector<Double> > get(const Unit &inunit) const;
  Quantum<Vector<Double> > getAngle() const;
  Quantum<Vector<Double> > getAngle(const Unit &inunit) const;
  // </group>
  
  // Make copy
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
