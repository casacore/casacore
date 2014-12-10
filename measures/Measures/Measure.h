//# Measure.h: Physical quantities within reference frame
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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

#ifndef MEASURES_MEASURE_H
#define MEASURES_MEASURE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class Unit;
class MeasValue;
class MRBase;
template <class T> class Quantum;
template <class T> class Vector;

// <summary>
// Physical quantities within reference frame
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module="Measures">Measures module</linkto> description
//   <li> <linkto class=Quantum>Quantum</linkto> dimensioned values
//   <li> <linkto class=MeasValue>MeasValue</linkto> internal measure values
//   <li> <linkto class=MeasRef>MeasRef</linkto> class to specify reference
// 		 frame
//   <li> <linkto class=MeasConvert>MeasConvert</linkto> class, doing actual conversions
//		of a measure from one reference frame to another
//   <li> Some classes if you really want to understand details:
//	<ul>
//	  <li> <linkto class=MeasBase>MeasBase</linkto> class, the immediate
//		parent of all specific Measures
//	  <li> <linkto class=MCBase>MCBase</linkto> class, the base class
//		for all specific conversion routines (like
//		<linkto class=MCEpoch>MCEpoch</linkto>).
//   	  <li> <linkto class=MeasData>MeasData</linkto> class, containing a set
//		of generally usable constants, and all program data necessary for
//		conversions.
//	</ul>
//   	  <li> <linkto class=MeasTable>MeasTab;e</linkto> class, containing
//		the interface for external Tables (like leap-seconds, IERS data,
//		JPL data).
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// Measure forms the abstract base class for physical quantities within
// a reference frame. Examples of derived classes are:
// <ul>
//   <li> <linkto class=MEpoch>MEpoch</linkto>: a moment in time
//   <li> <linkto class=MDirection>MDirection</linkto>: a direction in space
//   <li> <linkto class=MPosition>MPosition</linkto>: a position on Earth
//   <li> <linkto class=MFrequency>MFrequency</linkto>: wave characteristics
//   <li> <linkto class=MRadialVelocity>MRadialVelocity</linkto>: a space
//				radial velocity
//   <li> <linkto class=MDoppler>MDoppler</linkto>: a Doppler velocity
// </ul>
// Measure is the generic name for the more specific instances like, e.g.,
// MEpoch, an instant in time.<br>
// A Measure has both a value (specified in some value internal to the specific
// Measure, in general called <em>MVMeasure</em> (e.g. MVEpoch)), see
// <linkto class=MeasValue>MeasValue</linkto> for general details; and a
// reference type and frame specifier (see 
// <linkto class=MeasRef>MeasRef</linkto> class).<br>
// The <linkto class=MeasRef>MeasRef</linkto> specifies the reference type
// of the value, e.g. TAI, UTC, LAST. In addition the 
//  <linkto class=MeasRef>MeasRef</linkto> specifies a possible offset (e.g.
// the beginning of the year, or today), and, if necessary, Measures necessary
// for defining the absolute quantity (e.g. an 
// <linkto class=MPosition>MPosition</linkto> on Earth for LAST), using a
// reference frame specifier (see
// <linkto class=MeasFrame>MeasFrame</linkto> class).<br>
// The <src>MeasRef</src> class is templated, but typedefs exist
// (and should be used) to
// easily specify the correct one, e.g. <src>MEpoch::Ref</src>.<br>
// A Measure can be converted from one reference frame to another (e.g.
// an MDirection can be converted from J2000 to apparent coordinates) by
// setting up a measure specific conversion engine (see 
// <linkto class=MeasConvert>MeasConvert</linkto> class and below).
// From an input
// <src>MeasRef</src> frame and an output <src>MeasRef</src> frame it
// constructs a conversion <em>functional</em>, that can be fed values (with
// the <src>() operator</src>).<br>
// Some conversions can, in addition to the main type (like TAI), specify
// details to completely describe any conversion process (e.g. the type
// of nutation calculation) by specifying 
// <linkto class=Aipsrc>Aipsrc</linkto> keyword/value pairs. <br>
// <p>
// Measures can in general be constructed from a <src>MeasRef</src> and a
// value. The value can be expressed in the internally used units (e.g.
// <linkto class=MVEpoch>MVEpoch</linkto> for <src>MEpoch</src>, 
// <linkto class=MVDirection>MVDirection</linkto> for <src>MDirection</src>), or
// as a <src>Quantum</src>, i.e. a value with a dimension (e.g. (20,"km/s"))
// (see <linkto class=Quantum>Quantum</linkto> class). The preferred way of
// construction is by using the constructor:
// <srcblock>
//	Measure(MVmeasure, Measure::Ref)
// </srcblock>
// where the reference can be omitted,
// defaulting to <src>Measure::DEFAULT</src>), or in simple cases (not needing
// additional frame information) be specified directly as a code (e.g.
// <src>MEpoch::IAT</src>).<br> 
// <p>
// The value of the <src>Measure</src> can be obtained by a variety of 
// <src>get</src> functions, returning in general internal or <src>Quantum</src>
// values. The preferred way is a <src>getValue(void)</src>, which returns
// the specific <src>MVmeasure</src> value, which can then be further formatted
// using the appropiate <src>MVmeasure</src> get() functions.<br>
// Special formatting (like hh:mm:ss.t, dd.mm.ss.t, yy/mm/dd etc)
// are catered for in <em>conversion-type</em> classes like 
// <linkto class=MVAngle>MVAngle</linkto>, 
// <linkto class=MVTime>MVTime</linkto>.<br>
// <p>
// Conversion (within a Measure type) from one reference frame to another
// is done by the <linkto class=MeasConvert>MeasConvert</linkto> class. The
// class is templated, but has typedefs <src>Measure::Convert</src> (e.g.
// MEpoch::Convert) for easy, and recommended,  reference.<br>
// The basic constructors for a
// <src>Measure::Convert</src> are:
// <srcblock>
// // With a default Measure included
// 	Measure::Convert(Measure val, Measure::Ref outref);
// // With only input and output reference frames given 
//	Mesaure::Convert( Measure::Ref inref, Measure::Ref outref);
// </srcblock>
// The <src>val</src>
// is used as a <em>model</em> for subsequent input values into this
// <em>conversion engine</em>, including possible units; the <src>outref</src>
// specifies the output reference frame wanted. The constructor analyses the
// conversion wanted, and sets up a vector of routine calls to be called
// in sequence for the conversion. The actual conversion is done
// by the <src>() operator</src>.<br>
// To aid in using the raw measures, each class has also a Measure::MVType and
// Measure::MCType defined. They denote respectively the Measure Value class
// of the internal value, and the class with conversion routines.
// <p>
// <note role=tip> In the member description a number of <em>dummy</em> routines are
// present. They are the only way I have found to get <em>cxx2html</em> to
// get the belonging text properly present.
// </note>
// </synopsis>
//
// <example>
// <srcblock>
// #include <casacore/measures/Measures.h>
// #include <casacore/casa/Measure/MEpoch.h>
// // Example is only to show what can be done, not the easiest way
// // Set up a simple reference (no offset or secondary Measures). It
// // indicates that times are given in MJD TAI.
//	MEpoch::Ref reftai(MEpoch::TAI);
// // Same, but indicating MJD UTC
//      MEpoch::Ref refutc(MEpoch::UTC);
// // Set up an MEpoch (note that no reference is given. In that case a
// // default is assumed (for MEpoch UTC). MJD2000 is a provided constant
// // of the MJD at 2000.0
//     	MEpoch UTCval(Quantity(MeasData::MJD2000, "d"), reftai);
// // Set up, just for fun, an epoch, UTC for B1950.0:
//	MEpoch val1950(Quantity(MeasData::MJDB1950, "d"));
// // and use it as an offset in a reference
//	MEpoch::Ref ref1950(MEpoch::TAI, val1950);
// // An epoch for J2000 with an offset of B1950.0 will than be
//	MEpoch val20_50(Quantity(MeasData::MJD2000-MeasData::MJDB1950, "d"),
//		        ref1950);
// // Set up conversion from TAI(with values in days w.r.t. B1950.0) to UTC:
//	MEpoch::Convert tai_to_utc(val20_50, refutc);
// // And convert a value (in this case the value in val20_50, the model)
// // from TAI(relative to B1950.0) to 'absolute' UTC
//	MEpoch result = tai_to_utc();
// //   Show result
//      cout << "Result 1: " << result << endl;
// // To convert 10 years since B1950.0
//	result = tai_to_utc(Quantity(10.,"a"));
//      cout << "Result 2: " << result << endl;
// // To convert any value in years(the last used units of the model) since B1950.0
//	result = tai_to_utc(12.3);
//      cout << "Result 3: " << result << endl;
// </srcblock>
// Which generates the output:
// <srcblock>
//	Result 1: Epoch: 51544::11:59:25.2154
//	Result 2: Epoch: 36934::10:09:42.1283
//	Result 3: Epoch: 37774::11:57:41.1085
// </srcblock>
// </example>
//
// <motivation>
// To be able to specify a physical entity absolutely in any reference frame;
// and to be able to convert from one frame to another. E.g. Local Sidereal
// Time to Temps Atomic International. A templated version for the MeasRef
// and MeasConvert was chosen to be able to check most arguments at
// compile time.
// </motivation>
//
// <todo asof='1997/04/15'>
//	<li> more Measures, e.g. MPlanet
//	<li> operators on Measures (e.g. MEpoch - MEpoch == MDuration)
// </todo>

class Measure {

public:
  //# Enumerations
  // Each derived class should have a <src>Types</src> enumeration, specifying
  // the recognised frame types. It is formatted as:
  // <srcblock>
  //	enum Types {
  //	CODE1,
  //	CODE2,
  //	...,
  //	N_Types,		// Number of types
  //	SPEC1 = n,		// Possible special manipulator code
  //	.....,
  //	SYNONYM1 = CODEn,	// Probable synonyms
  //	....,
  //	DEFAULT = CODEm};
  // </srcblock>
  // Dummy for cxx2html
  enum Types {N_Types, DEFAULT = 0};
  
  //# Typedefs
  // Each Measure should have typedefs of the form:
  // <srcblock>
  // typedef MeasConvert<class a_Measure, class its_MV, its_MC> Convert;
  // typedef MeasRef<class a_Measure> Ref;
  // </srcblock>
  // Dummy for cxx2html
  typedef void* Convert;
  //# Friends
  // Each derived class should have:
  // <srcblock>
  // 	friend class MeasConvert<a_Measure, its_MV, its_MC>;
  // </srcblock>
  // Output a Measure
  friend std::ostream &operator<<(std::ostream &os, const Measure &meas);
  
  //# Constructors
  
  //# Destructor
  // Destructor
  virtual ~Measure();
  
  //# Operators
  
  //# General Member Functions
  // Each Measure should have the following set functions (with appropiate
  // MVs and Ref):
  // <srcblock>
  //	void set(const MVmeasure &dt);
  //	void set(const Measure::Ref &rf);
  //	void set(const MVmeasure &dt, const Measure::Ref &rf);
  // </srcblock>
  // <group>
  virtual void set(const MeasValue &dt) = 0;
  virtual Bool putValue(const Vector<Quantum<Double> > &in) = 0;
  // </group>
  // Set the offset in the reference (False if non-matching Measure)
  virtual Bool setOffset(const Measure &in) = 0;
  //
  // Check the type of derived Measure entity (e.g. "Epoch")
  virtual Bool areYou(const String &tp) const = 0;
  // Get the type (== Register() of derived Measure (faster than Strings)
  // All should have:
  // static uInt myType();
  virtual uInt type() const = 0;
  // Assert that we are the correct Measure type
  // <thrown>
  //   <li> AipsError if wrong Measure type
  // </thrown>
  // Each Measure should have:
  // <src> static void assure(const Measure &in); </src>
  // <group>
  virtual void assured(const String &tp) const = 0;
  // </group>
  // Tell me your Measure type (e.g. "Epoch")
  virtual const String &tellMe() const = 0;
  
  // Each Measure should have the following static methods to give its
  // name (e.g. Epoch) or reference type (e.g. UTC):<br>
  // <srcblock>
  // // Show the Measure type (e.g. "Direction")
  //    static const String &showMe();
  // // Cast an integer to the appropriate reference type. Avaialable to provide
  // // a safe cast in cases where Measure type is not explicitly known.
  //    static Measure::Types castType(uInt tp);
  // // Show the reference type (e.g. MEpoch::showType(MEpoch::IAT) == "TAI")
  //    static const String &showType(uInt tp);
  //    static const String &showType(Measure::Types tp);
  // </srcblock>
  // <group>
  virtual String getRefString() const = 0;
  // </group>
  // Tell me if you are a pure model (e.g. a planet)
  virtual Bool isModel() const;
  //
  // Each derived class should have a string-to-code translation routine
  // for the reference type. The routine returns False if unknown String (and
  // a default mr), else an appropiate mr reference.
  // <srcblock>
  //	Bool giveMe(Measure::Ref &mr, const String &in);
  //	static Bool getType(Measure::Types &tp, const String &in);
  // </srcblock>
  // <group>
  // Dummy for cxx2html
  void dummy_giveMe() const {}
  // </group>
  //
  // Set the reference type to the specified String. False if illegal
  // string, reference set to DEFAULT.
  virtual Bool setRefString(const String &in) = 0;
  // Get the default reference type
  virtual const String &getDefaultType() const = 0;
  // Get a list of all known reference codes. nall returns the number in list,
  // nextra the number of specials (like planets) that should be at 
  // end of list). typ returns the list of corresponding types.
  // All should have
  // <srcblock>
  //  static const String* allMyTypes(Int &nall, Int &nextra,
  //			              const uInt *&typ);
  // </srcblock>
  // <group>
  virtual const String* allTypes(Int &nall, Int &nextra,
                                 const uInt *&typ) const;
  // </group>
  //
  // Check if all internal tables of types (both enum and String) are 
  // complete and correct. This function is called automatically if and when
  // necessary.
  // <thrown>
  //   <li> AipsError if a (programming) error in the types.
  // </thrown>
  // All should have
  // <srcblock>
  //  static void checkMyTypes();
  // </srcblock>
  // <group> 
  virtual void checkTypes() const = 0;
  // </group>
  //
  // A general string checking routine to be used in derived measures.
  // Its arguments are the string to be converted (in), an array of
  // strings to check against (tname), and its length (N_name). The check
  // is case insensitive and mini-max. A return value less than N_name indicates
  // success.
  static uInt giveMe(const String &in, Int N_name, 
		     const String tname[]);
  // Each class should have a function to return its reference:
  // <srcblock>
  //  	Measure::Ref getRef() const;
  // </srcblock>
  // <group>
  // Dummy for cxx2html
  void dummy_getRef() const {}
  // </group>
  //
  // Each derived class should be able to get its internal value and have:
  // <srcblock>
  // 	const MVmeasure &getValue() const;
  // </srcblock>
  // To get dimensioned data, each derived class should contain the 
  // appropiate one of:
  // <srcblock>
  //    Quantity get(const Unit &unit) const;
  //    Quantum<Vector<Double> > get(const Unit &unit) const;
  // </srcblock>
  // <group>
  void dummy_getValue() const {}
  // </group>
  //
  //
  // Get unit (only available if Measure generated from a Quantum, else "")
  virtual const Unit &getUnit() const = 0;
  
  // Get data pointer (used by MeasConvert)
  virtual const MeasValue* getData() const = 0;
  
  // Get general reference pointer
  virtual MRBase *getRefPtr() const = 0;
  
  // Print a Measure
  virtual void print(std::ostream &os) const = 0;
  // Create a copy
  // <group>
  virtual Measure *clone() const = 0;
  // </group>
protected:
  
private:
  //# Enumerations
  
  //# Data
  // Each class will have the following information:
  // Actual data
  // <srcblock>
  //	MVmeasure data;
  // </srcblock>
  // Reference frame data
  // <srcblock>
  //	MeasRef<Measure> ref;
  // </srcblock>
  // Possible input units
  // <srcblock>
  //	Unit unit;
  // </srcblock>
  // And maybe later (or somewhere else)
  // <srcblock>
  //	MeasErr error;
  // </srcblock>
  // <group>
  // Dummy for cxx2html
  void dummy_data() const {}
  // </group>
  //
  //# Member functions
  // Clear the measure
  virtual void clear() = 0;
  
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
std::ostream &operator<<(std::ostream &os, const Measure &meas);
// </group>


} //# NAMESPACE CASACORE - END

#endif
