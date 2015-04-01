//# Muvw.h: A Measure: uvw on Earth
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

#ifndef MEASURES_MUVW_H
#define MEASURES_MUVW_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/casa/Quanta/MVuvw.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Muvw;
class MCuvw;
class MDirection;
template <class M> class MeasConvert;
template <class M> class ArrayMeasColumn;
template <class M> class ScalarMeasColumn;

//# Typedefs

// <summary> A Measure: uvw on Earth </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMuvw" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// From Measure and uvw
// </etymology>
//
// <synopsis>
// Muvw is the derived Measure class for an interferometer uvw.
// uvws can be given in any of the direction types, or as ITRF, the 
// IERS base.<br>
// Note that at the moment no correction for Earth tides (error <~ 0.05 mm/km
// EW uvw), plate motion (not relevant for telescopes on same plate) and
// relativistic effects are incorporated. B1950 has the same caveat as in
// <linkto class=MDirection>MDirection</linkto>.
// </synopsis>
//
// <example>
// <srcblock>
//      // Specify an Epoch and a telescope position
//	MEpoch tbm(Quantity(50927.92931, "d"));
//	MPosition pos(MVPosition(-4750915.84032, 2792906.17778, 
//				 -3200483.75028), 
//		      MPosition::ITRF);
//      // Use them in a frame
//	MeasFrame mf(tbm, pos);
//      // Specify an uvw (note that values here are in m)
//	MVuvw mvb0(100 ,10, 0);
//	cout << "uvw: " << mvb0 << endl;
//      // Specify a reference (type and where and when) for the following uvw
//	Muvw::Ref mbref0(Muvw::ITRF, mf);
//	Muvw mb0(mvb0, mbref0);
//      // Show the uvw
//	cout << "uvw: " << mb0 << endl;
//	cout << "uvw reference: " << mbref0 << endl;
//      // Another reference
//	Muvw::Ref mbref1(Muvw::J2000);
//	cout << "uvw reference: " << mbref1 << endl;
//      // Convert the uvw coordinates to the other reference and show it
//	cout << "Test uvw conversion ..." << endl;
//	Muvw::Convert bconv(mb0, mbref1);
//	cout << "Converted " << mb0 << endl <<
//	  " to " << mbref1 << endl <<
//	  " as " << bconv() << endl;
// </srcblock>
// </example>
//
// <motivation>
// To be able to handle conversions between uvw coordinates with different 
// reference directions.
// </motivation>
//
// <todo asof="2000/11/08">
//	<li> EW baselines
// </todo>

class Muvw : public MeasBase<MVuvw, MeasRef<Muvw> > {

public:
  //# Friends
  // Conversion of data
  friend class MeasConvert<Muvw>;
  
  //# Enumerations
  // Types of known Muvws
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
  // Measure value container for this class (i.e. Muvw::MVType)
  typedef MVuvw MVType;
  // Measure conversion routines for this class (i.e. Muvw::MCType)
  typedef MCuvw MCType;
  // Measure reference (i.e. Muvw::Ref)
  typedef MeasRef<Muvw> Ref;
  // MeasConvert use (i.e. Muvw::Convert)
  typedef MeasConvert<Muvw> Convert;
  // Measure table Columns (e.g., Muvw::ScalarColumn)
  typedef ScalarMeasColumn<Muvw> ScalarColumn;
  typedef ArrayMeasColumn<Muvw> ArrayColumn;
  // Reference enum Types (included originally for gcc 2.95)  
  typedef WHATEVER_SUN_TYPEDEF(Muvw) Types Types;
  
  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. </note>
  // Default constructor; generates the ITRF centre
  Muvw();
  // Create from data and reference
  // <group>
  Muvw(const MVuvw &dt);
  Muvw(const MVuvw &dt, const Muvw::Ref &rf);
  Muvw(const MVuvw &dt, Muvw::Types rf);
  Muvw(const Measure *dt);
  Muvw(const MeasValue *dt);
  // </group>
  // Copy constructor and assign
  // <group>
  Muvw(const Muvw &);
  Muvw &operator=(const Muvw &);
  // </group>
  
  //# Destructor
  virtual ~Muvw();
  
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
  static Muvw::Types castType(uInt tp);
  static const String &showType(Muvw::Types tp);
  static const String &showType(uInt tp);
  // </group>
  // Translate string to reference code
  // <group>
  static Bool getType(Muvw::Types &tp, const String &in);
  Bool giveMe(Muvw::Ref &mr, const String &in);
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
  // Get the correct Muvw type from a given direction type (or v.v.)
  // <group>
  static Muvw::Types fromDirType(const MDirection::Types in);
  static MDirection::Types toDirType(const Muvw::Types in);
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
