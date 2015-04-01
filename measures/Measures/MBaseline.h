//# MBaseline.h: A Measure: Baseline on Earth
//# Copyright (C) 1998-2000,2002,2004,2007
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

#ifndef MEASURES_MBASELINE_H
#define MEASURES_MBASELINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVBaseline.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MBaseline;
class MCBaseline;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;

//# Typedefs

// <summary> A Measure: Baseline on Earth </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMBaseline" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// From Measure and Baseline
// </etymology>
//
// <synopsis>
// MBaseline forms derived Measure class for an interferometer baseline.
// Baselines can be given in any of the direction types, or as ITRF, the 
// IERS base.<br>
// Note that at the moment no correction for Earth tides (error <~ 0.05 mm/km
// EW baseline), plate motion (not relevant for telescopes on same plate) and
// relativistic effects are incorporated. B1950 has the same caveat as in
// <linkto class=MDirection>MDirection</linkto>.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2000/06/15">
//	<li> add some Earth tide model
// </todo>

class MBaseline : public MeasBase<MVBaseline, MeasRef<MBaseline> > {

 public:
  //# Friends
  // Conversion of data
  friend class MeasConvert<MBaseline>;

  //# Enumerations
  // Types of known MBaselines
  // <note role=warning>
  // The order defines the order in the translation matrix FromTo
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
    B1950_VLA,
    BMEAN,
    BTRUE,
    GALACTIC,
    HADEC,
    AZEL,
    AZELSW,
    AZELGEO,
    AZELSWGEO,
    JNAT,
    ECLIPTIC,  
    MECLIPTIC,
    TECLIPTIC,
    SUPERGAL,
    ITRF,
    TOPO,
    ICRS,
    N_Types,
    // Defaults
    DEFAULT=ITRF,
    // Synonyms
    AZELNE=AZEL,
    AZELNEGEO=AZELGEO };

  //# Typedefs
  // Measure value container for this class (i.e. MBaseline::MVType)
  typedef MVBaseline MVType;
  // Measure conversion routines for this class (i.e. MBaseline::MCType)
  typedef MCBaseline MCType;
  // Measure reference (i.e. MBaseline::Ref)
  typedef MeasRef<MBaseline> Ref;
  // Measure Convert (i.e. MBaseline::Convert)
  typedef MeasConvert<MBaseline> Convert;
  // Measure table Columns (e.g., MBaseline::ScalarColumn)
  typedef ScalarMeasColumn<MBaseline> ScalarColumn;
  typedef ArrayMeasColumn<MBaseline> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(MBaseline) Types Types;

//# Constructors
// <note role=tip> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. </note>
// Default constructor; generates the ITRF centre
    MBaseline();
// Create from data and reference
// <group>
    MBaseline(const MVBaseline &dt);
    MBaseline(const MVBaseline &dt, const MBaseline::Ref &rf);
    MBaseline(const MVBaseline &dt, MBaseline::Types rf);
    MBaseline(const Measure *dt);
    MBaseline(const MeasValue *dt);
// </group>

  // <group>
  MBaseline(const MBaseline &);
  MBaseline &operator=(const MBaseline &);
  // </group>

//# Destructor
    virtual ~MBaseline();

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
  static MBaseline::Types castType(uInt tp);
  static const String &showType(MBaseline::Types tp);
  static const String &showType(uInt tp);
  // </group>
  // Translate string to reference code
  // <group>
  static Bool getType(MBaseline::Types &tp, const String &in);
  Bool giveMe(MBaseline::Ref &mr, const String &in);
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
  // Get the correct MBaseline type from a given direction type (or v.v.)
  // <group>
  static MBaseline::Types fromDirType(const MDirection::Types in);
  static MDirection::Types toDirType(const MBaseline::Types in);
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
