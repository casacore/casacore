//# MeasRef.h: Reference frame for physical measures
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

#if !defined(AIPS_MEASREF_H)
#define AIPS_MEASREF_H

#if defined(_AIX)
#pragma implementation ("MeasRef.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasFrame.h>

//# Forward Declarations
class String;
template <class Ms> class MeasRef;
imported class ostream;

// <summary> Reference frame for physical measures </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Quantum>Quantum</linkto> class 
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// From Measure and Reference frame
// </etymology>
//
// <synopsis>
// MeasRef specifies the reference frame for a physical quantity
// specified by one of the derived <linkto class=Measure>Measure</linkto>
// classes (e.g. <linkto class=MEpoch>MEpoch</linkto>)
//
// A MeasRef consists of one or more <em>type</em> indicators,
// an optional offset (e.g. beginning of year), and sometimes
// one or more Measures specifying the reference frame (e.g. an
// <linkto class=MPosition>MPosition</linkto> for LST). A time
// (<linkto class=MEpoch>MEpoch</linkto>) could e.g. have a type
// <src>MEpoch::TAI</src>, and an MEpoch as offset, 
// <src>MEpoch(Quantity(40745,"d"),MeasRef &myref)</src>.<br>
// It is obvious that a circular reference between Measure and MeasRef
// is possible. Therefore, each Measure has a <em>default</em> reference
// (necessary anyway to be able to start a Measure chain). For MEpoch
// the default is an MJD in UT; and the default Measure for an MEpoch reference
// is 0.<br>
// Some <src>MeasRef</src> could need additional conversion information
// ( example: type of Nutation calculations). They are provided by
// the <linkto class=MeasDetail>MeasDetail</linkto> class, used by the
// <linkto class=MeasConvert>MeasConvert</linkto> class.<br>
// All constructors are related to a specific Measure, to be able to check
// relations at compile time.
// </synopsis>
//
// <example>
// See <linkto class=Measure>Measure</linkto> for an example
// </example>
//
// <motivation>
// To gather all reference frame information in the one class.
// </motivation>
//
// <todo asof="1996/02/21">
// </todo>

template<class Ms> class MeasRef {

public:
//# Friends
// Output
    friend ostream &operator<<( ostream &os, const MeasRef<Ms> &mr);

//# Constructors
// Construct an empty MeasRef. I.e. it will have a standard,
// <em>default</em>, type; no offsets and Frame.
    MeasRef();
// Copy constructor
    MeasRef(const MeasRef<Ms> &other);
// Copy assignment
    MeasRef &operator=(const MeasRef<Ms> &other);
// Construct a reference with specified type, offset and Frame
// <group>
// Note: the following should really be (and should still be called as), but
// compiler does not accept it:
//   <src> MeasRef(Ms::Types tp); </src>
// Furthermore, default arguments are not supported with templated classes:
    MeasRef(uInt tp);
    MeasRef(uInt tp, const Ms &ep);
    MeasRef(uInt tp, const MeasFrame &mf);
    MeasRef(uInt tp, const MeasFrame &mf, const Ms &ep);
// </group>

//# Destructor
    ~MeasRef();

//# Operators
// Check if same MeasRef
    Bool operator==(const MeasRef<Ms> &other) const;
// Check if unequal MeasRef
    Bool operator!=(const MeasRef<Ms> &other) const;

//# General Member Functions
// Check if empty reference
    Bool empty() const;
// Check the type of Measure the reference can be used for
// <group>
    static const String &showMe();
// </group>
// Return the type of the reference
// Note: the following should really be (and should be interpreted as), but
// compiler does not accept it:
//   <src> Ms::Types getType();</src>
    uInt getType() const;
// Return the frame of reference
    const MeasFrame &getFrame();
// Return the first frame which has specified information. Checking is done in 
// argument order.
// <thrown>
//   <li> AipsError if neither reference has a frame or the proper type
// </thrown>
// <group>
static const MeasFrame &framePosition(const MeasRef<Ms> &ref1,
				      const MeasRef<Ms> &ref2);
static const MeasFrame &frameEpoch(const MeasRef<Ms> &ref1,
				   const MeasRef<Ms> &ref2);
static const MeasFrame &frameDirection(const MeasRef<Ms> &ref1,
				       const MeasRef<Ms> &ref2);
static const MeasFrame &frameRadialVelocity(const MeasRef<Ms> &ref1,
					    const MeasRef<Ms> &ref2);
// </group>
// Return the offset (or 0)
    const Ms *const offset();
// Set the type
// <thrown>
//   <li> AipsError if wrong Measure
// </thrown>
// Note: the following should really be (and should be called as), but
// compiler does not accept it:
// <group>
//   <src> void set(Ms::Types tp);</src>
    void setType(uInt tp);
    void set(uInt tp);
// </group>
// Set a new offset
    void set(const Ms &ep);
// Set a new frame
    void set(const MeasFrame &mf);
    
private:

// Representation class
class RefRep {
    public:
// Constructor
// <note> Next one must be in-line for (some?) compilers </note>
    RefRep() : type(0), offmp(0), frame(), cnt(1) {};
// Destructor
// <note> Next one must be in-line for (some?) compilers </note>
    ~RefRep() {delete offmp;}; 
// The actual data
// <group>
// Type of reference
    uInt type;
// Pointer to main Measure, defining an offset
    Ms *offmp;
// Reference frame
    MeasFrame frame;
// </group>
// Usage count
    Int cnt;
};

//# Data
    RefRep *rep;

//# Member functions
// Create an instance of MeasRef
    void create();

// Copy an instance
    MeasRef copy();
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output a reference
template <class Ms>
 ostream &operator<<( ostream &os, const MeasRef<Ms> &mr);
// </group>

#endif
