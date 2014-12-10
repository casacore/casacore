//# MRBase.h: Base for Reference frame for physical measures
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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

#ifndef MEASURES_MRBASE_H
#define MEASURES_MRBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;

// <summary> Base for Reference frame for physical measures </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Quantum>Quantum</linkto> class 
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// From Measure and Reference and Base
// </etymology>
//
// <synopsis>
// MRBase is the abstract base class for reference frames.
// Reference frames are specified (see <linkto class=Measure>Measure</linkto>)
// as <src>Measure::Ref</src> (e.g. <src>MEpoch::Ref</src>).
//
// A Measure::Ref is a container for <em>type</em> indicators,
// (e.g. <src>MDirection::J2000</src>),
// an optional offset (e.g. beginning of year), and, if necessary, a
// <linkto class=MeasFrame>MeasFrame</linkto>.<br>
// A MeasFrame consists of
// one or more Measures specifying the reference frame (e.g. an
// <linkto class=MPosition>MPosition</linkto> for a sidereal time definition).
// A time
// (<linkto class=MEpoch>MEpoch</linkto>) could e.g. have a type
// <src>MEpoch::TAI</src>, and an MEpoch as offset: 
// <srcblock>
// MEpoch off(Quantity(40745,"d"), MEpoch::Ref(MEpoch::UTC));
// MEpoch::Ref myref(MEpoch::TAI, off);
// </srcblock>
//
// It is obvious that a circular reference between Measure and Measure::Ref
// is possible. Therefore, each Measure has a <em>default</em> reference
// (necessary anyway to be able to start a Measure chain). For MEpoch
// the default is e.g. an MJD in UTC; and the default Measure for
// an MEpoch reference is 0.<br>
// References are copied by reference; i.e. a reference can be used in many
// places without overhead.<br>
// Some <src>Measure::Ref</src> could need additional conversion information
// ( example: type of Nutation calculations). They are provided by
// <linkto class=Aipsrc>Aipsrc</linkto> keywords. <br>
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
// <todo asof="1997/04/15">
// </todo>

class MRBase {
  
public:

  //# Friends
  friend ostream &operator<<(ostream &os, const MRBase &meas);
  
  //# Constructors
  
  //# Destructor
  virtual ~MRBase();
  
  //# Operators
  
  //# General Member Functions
  // Check if empty reference
  virtual Bool empty() const = 0;
  // Check the type of Measure the reference can be used for:<br>
  //   <src> static const String &showMe() = 0; </src>.<br>
  // Return the type of the reference
  // <note role=caution> the following should really be
  // (and should be interpreted as), but
  // compiler does not accept it:</note>
  //   <src> Ms::Types getType();</src>
  virtual uInt getType() const = 0;
  // Return the frame of the reference
  virtual MeasFrame &getFrame() = 0;
  // Return the first frame which has specified information. Checking is done in 
  // argument order.
  // <thrown>
  //   <li> AipsError if neither reference has a frame or the proper type
  // </thrown>
  // <srcblock>
  //  static const MeasFrame &framePosition(const MRBase &ref1,
  //						const MRBase &ref2) = 0;
  //  static const MeasFrame &frameEpoch(const MRBase &ref1,
  //					 const MRBase &ref2) = 0;
  //  static const MeasFrame &frameDirection(const MRBase &ref1,
  //					     const MRBase &ref2) = 0;
  //  static const MeasFrame &frameRadialVelocity(const MRBase &ref1,
  //						  const MRBase &ref2) = 0;
  // </srcblock>
  // Return the offset (or 0)
  virtual const Measure* offset() const = 0;
  // Set the type
  // <thrown>
  //   <li> AipsError if wrong Measure
  // </thrown>
  // <note role=caution> the following should really be (and should be called as), but
  // compiler does not accept it:</note>
  //   <src> void set(Ms::Types tp);</src>
  // <group>
  virtual void setType(uInt tp) = 0;
  virtual void set(uInt tp) = 0;
  // </group>
  // Set a new offset:<br>
  //   void set(const Measure &ep);
  // Set a new frame
  virtual void set(const MeasFrame &mf) = 0;
  
  // Print a Measure
  virtual void print(ostream &os) const = 0;
  
protected:
  
private:
  
  //# Data
  
  //# Member functions
  
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
ostream &operator<<(ostream &os, const MRBase &meas);
// </group>


} //# NAMESPACE CASACORE - END

#endif
