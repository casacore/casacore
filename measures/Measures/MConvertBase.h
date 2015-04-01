//# MConvertBase.h: Conversion of Measures Base
//# Copyright (C) 1995,1996,1997,1999,2000,2001
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

#ifndef MEASURES_MCONVERTBASE_H
#define MEASURES_MCONVERTBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Unit;
class MeasValue;
class Measure;
class MRBase;

//# Typedefs

//# Constants

// <summary> Conversion of Measures Base</summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
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
// MConvertBase can convert a Measure to the same type of Measure in a
// different reference frame. The MConvertBase is a templated class, but
// has typedefs for the allowed conversions, like <src>MEpoch::Convert.</src><br>
// The basic operation is to create a MConvertBase with either of:
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
//	MEpoch to the MeasRef belonging to the MConvertBase
//    <li> (Quantity): will use the conversion chain deduced from the
//	MEpoch model in the definition of MConvertBase, and will convert the
//	Quantity
//    <li> (Quantum<Vector<Double> >) as previous
//    <li> (Double): will use the units (if present) as specified in the
//		MConvertBase object to construct the internal value
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

class MConvertBase {

public:
  
  //# Friends
  
  //# Constructors
  
  //# Destructor
  virtual ~MConvertBase();
  
  //# Operators
  
  //# General Member Functions
  // Set a new model for the conversion
  virtual void setModel(const Measure &val) = 0;
  // Set a new model value only
  virtual void set(const MeasValue &val) = 0;
  // Set a new model unit only
  virtual void set(const Unit &inunit) = 0;
  // Add a method (Note: uInt should be an enum from the appropiate Measure)
  virtual void addMethod(uInt method) = 0;
  // Add a FrameTypes used (as specified in MeasFrame::FrameTypes)
  virtual void addFrameType(uInt tp) = 0;
  // Get number of methods
  virtual Int nMethod() const = 0;
  // Get method
  virtual uInt getMethod(uInt which) const = 0;
  // Print a conversion engine
  virtual void print(ostream &os) const = 0;
  
private:
  //# Data
  
  //# Member functions

};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output decalration
ostream &operator<<( ostream &os, const MConvertBase &mc);
// </group>


} //# NAMESPACE CASACORE - END

#endif
