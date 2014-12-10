//# MeasRef.h: Reference frame for physical measures
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

#ifndef MEASURES_MEASREF_H
#define MEASURES_MEASREF_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MRBase.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
template <class Ms> class MeasRef;

// <summary> Reference frame for physical measures </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MRBase>MRBase</linkto>: the MeasRef base class
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
// classes (e.g. <linkto class=MEpoch>MEpoch</linkto>). It is derived from
// <linkto class=MRBase>MRBase</linkto>, which describes the class.
//
// MeasRef containres are created using the <src>Measure::Ref</src> class
// (e.g. <src>MDirection::Ref</src>).
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

template<class Ms> class MeasRef : public MRBase {
  
public:
  
  //# Friends
  
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
  // <note role=caution> The following should really be (and should 
  // still be called as), but
  // compiler does not accept it, due to incomplete definition when
  // called in MeasBase: </note>
  //   <src> MeasRef(Ms::Types tp); </src>
  // Furthermore, default arguments are not supported with templated classes:
  explicit MeasRef(const uInt tp);
  MeasRef(const uInt tp, const Ms &ep);
  MeasRef(const uInt tp, const MeasFrame &mf);
  MeasRef(const uInt tp, const MeasFrame &mf, const Ms &ep);
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
  virtual Bool empty() const;
  // Check the type of Measure the reference can be used for
  // <group>
  static const String &showMe();
  // </group>
  // Return the type of the reference
  // <note role=caution> the following should really be 
  // (and should be interpreted as), but
  // cannot create a virtual function:</note>
  //   <src> Ms::Types getType();</src>
  virtual uInt getType() const;
  // Return the frame of reference
  virtual MeasFrame &getFrame();
  // Return the first frame which has specified information. Checking is done in 
  // argument order.
  // <thrown>
  //   <li> AipsError if neither reference has a frame or the proper type
  // </thrown>
  // <group>
  static const MeasFrame &framePosition(MRBase &ref1,
					MRBase &ref2);
  static const MeasFrame &frameEpoch(MRBase &ref1,
				     MRBase &ref2);
  static const MeasFrame &frameDirection(MRBase &ref1,
					 MRBase &ref2);
  static const MeasFrame &frameRadialVelocity(MRBase &ref1,
					      MRBase &ref2);
  static const MeasFrame &frameComet(MRBase &ref1,
				     MRBase &ref2);
  // </group>
  // Return the offset (or 0)
  virtual const Measure* offset() const;
  // Set the type
  // <thrown>
  //   <li> AipsError if wrong Measure
  // </thrown>
  // <note role=caution> the following should really be 
  // (and should be called as), but
  // compiler does not accept it, since a virtual function:</note>
  //   <src> void set(Ms::Types tp);</src>
  // <group>
  virtual void setType(uInt tp);
  virtual void set(uInt tp);
  // </group>
  // Set a new offset
  void set(const Ms &ep);
  // Set a new offset (for internal use only)
  void set(const Measure &ep);
  // Set a new frame
  virtual void set(const MeasFrame &mf);
  
  // Print a Measure
  virtual void print(ostream &os) const;
  
private:
  
  // Representation class
  class RefRep {
  public:
    // Constructor
    // <note role=warning> Next one must be in-line for (some?) compilers </note>
    RefRep() : type(Ms::DEFAULT), offmp(0), frame() {}
    // Destructor
    // <note role=warning> Next one must be in-line for (some?) compilers </note>
    ~RefRep() {delete offmp;} 
    // The actual data
    // <group>
    // Type of reference
    typename Ms::Types type;
    // Pointer to main Measure, defining an offset
    Measure *offmp;
    // Reference frame
    MeasFrame frame;
    // </group>
  };
  
  //# Data
  CountedPtr<RefRep> rep_p;
  
  //# Member functions
  // Create an instance of MeasRef
  void create();
  
  // Copy an instance
  MeasRef copy();
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/Measures/MeasRef.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
