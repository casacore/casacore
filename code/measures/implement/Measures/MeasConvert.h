//# MeasConvert.h: Conversion of Measures
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

#if !defined(AIPS_MEASCONVERT_H)
#define AIPS_MEASCONVERT_H

#if defined(_AIX)
#pragma implementation ("MeasConvert.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasRef.h>

//# Forward Declarations

//# Typedefs

//# Constants
// Length of structure cache
    const uInt MC_N_Struct = 16;

// <summary> Conversion of Measures </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
//   <li> <linkto class=MeasRef>MeasRef</linkto> class 
//   <li> <linkto class=Quantum>Quantum</linkto> class 
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// MeasConvert can convert a Measure to the same type of Measure in a
// different reference frame. The MeasConvert is a templated class, but
// has typedefs for the allowed conversions, like <src>MEpoch::Convert.</src><br>
// The basic operation is to create a MeasConvert with either of:
// <ul>
//   <li> MEpoch::Convert(MEpoch, MEpoch::Ref), where the 
//	<linkto class=MEpoch>MEpoch</linkto> is a template for subsequent
//	conversions, i.e. it will remember the value and 
//	the input reference frame. And the 
//	<linkto class=MeasRef>MeasRef</linkto> is the output reference class.
//   <li> MEpoch::Convert(MEpoch) with a subsequent setOut(MEpoch::Ref)
//   <li> MEpoch::Convert(MEpoch::Ref in, MEpoch::Ref out) is a template for
//	 conversions from the input reference to the output reference. The
//	'template' model used is the default value for the Measure, with
//	no units. 
//   <li> MEpoch::Convert(Unit, MEpoch::Ref in, MEpoch::Ref out) is a
//	 template for
//	 conversions from the input reference to the output reference. The
//	'template' model used is the default value for the Measure, with
//	the default units as specified.
//   <li> MEpoch::Convert() with a setModel(MEpoch) and setOut().
// </ul>
// An empty MeasRef indicates no conversion <br>.
// The constructor, and set functions, analyse the 'template' Measure and the
// output reference frame, and construct a pointer (in practice a list
// of pointers to bypass the necessity of creating too many conversion
// functions) to a conversion routine. Functionals will maybe used in
// a later version in stead of a vector of function pointers. During the
// implementation process, I have, for a variety of reasons, changed the
// list of function pointers to a list of numbers, with the Measure having
// just a <src>doConvert()</src> function containing an appropiate
// switch statement. <br>
// Actual conversions are done with the () operator, which produces a new
// MEpoch (or other Measure).<br>
// Possible arguments are (MVEpoch is used here generic, and indicates the
// internal format of a Measure; possibly, to make sure distinction between
// values with and without units is possible, even simple Measures will
// have their own internal class format, e.g. MVDouble. This will also aid
// in the possibility that I am still pursuing to have a fully dynamic 
// conversion possibility. However, to be able to use pointers to functions
// in any reasonable way for all possible input and output types, a
// multi-level approach is necessary, with all possible datatypes derived
// from some MeasValue.):
// <ul>
//   <li> (MEpoch, MEpoch::Ref): will create a new conversion method, and use 
//	it to produce the result of converting the MEpoch to the specified
//	frame
//    <li> (MEpoch): will create a new conversion method from the
//	MEpoch to the MeasRef belonging to the MeasConvert
//    <li> (Quantity): will use the conversion chain deduced from the
//	MEpoch model in the definition of MeasConvert, and will convert the
//	Quantity
//    <li> (Quantum<Vector<Double> >) as previous
//    <li> (Double): will use the units (if present) as specified in the
//		MeasConvert object to construct the internal value
//		 to be converted
//    <li> (Vector<Double> >): as previous
// </ul>
// Float versions will be produced if necessary.<br>
// The conversion analyser expects that all Measure classes have a set
// of routines to do the actual analysing and conversion.<br>
// If the standard conversion is not sufficient, additional methods can be
// added at the end of the list with the <src>addMethod()</src> member
// function, or at the beginning of the list with <src>insertMethod()</src>.
// The whole list can be cleared with <src>clearMethod()</src>.<br>
// To ease the specification of the Method, each Measure has a typedef
// (e.g. MEpoch_ConvType) specifying the Method type.
// </synopsis>
//
// <example>
// See <linkto class=Measure>Measure</linkto> for an example
// </example>
//
// <motivation>
// Conversion of Measures will in general be done on a series of values.
// Separating the analysis of the calculations necessary for the conversion
// from the actual conversion could speed up the process.
// </motivation>
//
// <todo asof="1996/02/22">
// </todo>

template<class M, class F> class MeasConvert {
public:
//# Friends
// Output
    friend ostream &operator<<( ostream &os, const MeasConvert<M,F> &mc);

//# Constructors
// <note> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. For reasons
// of compiler limitations the formal arguments had to be specified as
// <em>uInt</em> rather than the Measure enums that should be used as actual 
// arguments.</note>
// Construct an empty MeasConvert. It is not usable, unless a setModel, and
// probably a setOut has been done.
    MeasConvert();
// Copy constructor
    MeasConvert(const MeasConvert<M,F> &other);
// Copy assignment
    MeasConvert<M,F> &operator=(const MeasConvert<M,F> &other);

// Construct a conversion for the specified Measure and reference
// <group>
    MeasConvert(const M &ep);
    MeasConvert(const M &ep, const MeasRef<M> &mr);
    MeasConvert(const M &ep, uInt mr);
    MeasConvert(const MeasRef<M> &mrin, const MeasRef<M> &mr);
    MeasConvert(const MeasRef<M> &mrin, uInt mr);
    MeasConvert(uInt mrin, const MeasRef<M> &mr);
    MeasConvert(uInt mrin, uInt mr);
    MeasConvert(const Unit &inunit, const MeasRef<M> &mrin, 
		const MeasRef<M> &mr);
    MeasConvert(const Unit &inunit, const MeasRef<M> &mrin, 
		uInt mr);
    MeasConvert(const Unit &inunit, uInt mrin, 
		const MeasRef<M> &mr);
    MeasConvert(const Unit &inunit, uInt mrin, 
		uInt mr);
// </group>

//# Destructor
    ~MeasConvert();

//# Operators
// The actual conversion operations
// <group>
// Convert model Measure to output frame
    const M &operator()();
    const M &operator()(Double val);
    const M &operator()(const Vector<Double> &val);
    const M &operator()(const Quantum<Double> &val);
    const M &operator()(const Quantum<Vector<Double> > &val);
    const M &operator()(const F &val);
    const M &operator()(const M &val);
    const M &operator()(const M &val, const MeasRef<M> &mr);
    const M &operator()(const M &val, uInt mr);
    const M &operator()(const MeasRef<M> &mr);
    const M &operator()(uInt mr);
// </group>

//# General Member Functions
// Set a new model for the conversion
    void setModel(const M &val);
// Set a new output reference
// <group>
    void setOut(const MeasRef<M> &mr);
    void setOut(uInt mr);
// </group>
// Set a new model and reference
// <group>
    void set(const M &val, const MeasRef<M> &mr);
    void set(const M &val, uInt mr);
// </group>
// Set a new model value only
    void set(const F &val);
// Set a new model unit only
    void set(const Unit &inunit);

// Clear the method vector (and the user structure)
  void clearMethod();
// Add a method (Note: uInt should be an enum from the appropiate Measure)
  void addMethod(uInt method);
// Insert a method at begin of list
  void insertMethod(uInt method);
// Get number of methods
  Int nMethod() const;
// Get method
  uInt getMethod(uInt which) const;
// Insert a user structure in list
  void addStruct(uInt which, void *struc);
// Get user structure
  const void *getStruct(uInt which) const;

private:
//# Data
// The model template Measure
    M* model;
// The model unit to be used in conversions
    Unit unit;
// The output reference
    MeasRef<M> outref;
// The input offset
    F *offin;
// The output offset
    F *offout;
// Vector of conversion routines (length variable)
    Block<uInt> crout;
// Vector of structures to be used by Measure's doConvert 
// (Length = MC_N_Struct)
    Block<void *> cstruc;
// Cyclic buffer for return values
// <group>
// Current pointer
    Int lres;
    M *result[4];
// </group>
// Local variables that can be used in conversion
// <group>
    F *locres;
// </group>

//# Member functions
// Initialise pointers
    void init();
// Copy a MeasConvert
    void copy(const MeasConvert<M,F> &other);
// Clear self
    void clear();
// Create the conversion routine chain
    void create();
// Convert a value
// <group>
    const F &convert();
    const F &convert(const F &val);
// </group>
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output
template <class M, class F>
ostream &operator<<( ostream &os, const MeasConvert<M,F> &mc);
// </group>

#endif


