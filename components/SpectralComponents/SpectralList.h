//# SpectralList.h: A set of SpectralElements
//# Copyright (C) 2001
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

#ifndef COMPONENTS_SPECTRALLIST_H
#define COMPONENTS_SPECTRALLIST_H

//# Includes
#include <casa/aips.h>
#include <casa/Containers/Block.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class SpectralElement;
class RecordInterface;
class String;
template <class T> class Vector;

// <summary>
// A set of SpectralElements
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SpectralElement>SpectralElement</linkto> class
// </prerequisite>
//
// <etymology>
// From spectral line and element list
// </etymology>
//
// <synopsis>
// The SpectralList class is a container for a set of spectral elements.
//
// The list can be used in the
// <linkto class=SpectralFit>SpectralFit</linkto> class and in the
// <linkto class=SpectralEstimate>SpectralEstimate</linkto> class.
//
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a container for fitting of spectral profiles to an observed spectrum
// </motivation>
//
// <todo asof="2001/02/04">
//   <li> add more profile types
// </todo>

class SpectralList {
 public:

  //# Constructors
  // Default constructor creates an empty list
  SpectralList();
  // Construct a list with a maximum length of n (0: unlimited length)
  explicit SpectralList(uInt nmax);
  // Construct with an initial element
  explicit SpectralList(const SpectralElement &in);
  // Copy constructor (deep copy)
  SpectralList(const SpectralList &other);

  //#Destructor
  // Destructor
  ~SpectralList();

  //# Operators
  // Assignment (copy semantics)
  SpectralList &operator=(const SpectralList &other);
  // Evaluate the value of the sum of the elements at x
  Double operator()(const Double x) const;
  // Get element n
  // <thrown>
  //  <li> AipsError if illegal n
  // </thrown>
  // <group>
  const SpectralElement* operator[](const uInt n) const;
  SpectralElement* operator[](const uInt n);
  // </group>

  //# Member functions
  // Get the number of elements in list
  uInt nelements() const { return list_p.nelements(); };

  // Get the profile values for all elements in list. The evaluation
  // is for the length of the given <src>prof</src>, assuming x values of
  // 0,1,... if no x given.
  // <group>
  template <class MT>
    void evaluate(Vector<MT> &y) const;
  template <class MT>
    void evaluate(Vector<MT> &y, const Vector<MT> &x) const;
   // </group>

  // Calculate the residuals at the points x; by subtracting the model from y.
  // x=0,1,2,.. if not given.
  // <thrown>
  //  <li> AipsError if y and x have different lengths
  // </thrown>
  // <group>
  template <class MT>
    void residual(Vector<MT> &y) const;
  template <class MT>
    void residual(Vector<MT> &y, const Vector<MT> &x) const;
  // </group>

  // Add elements to list (False if list has max length and full)
  // <group>
  Bool add(const SpectralElement &in);
  Bool add(const SpectralList &in);
  // </group>
  // Insert in sort order in the list
  // <group>
  void insert(const SpectralElement &in);
  void insert(const SpectralList &in);
  // </group>
  // Set an element in the list. Return False if more than one place beyond
  // end of list; or if beyond max size.
  Bool set(const SpectralElement &in, const uInt which);

  // Clear the list
  void clear();

  // Set a maximum size of the list
  void set(const uInt nmax);

  // Sort the list on the first parameter (i.e. peak value for Gaussian)
  void sort();

  // Convert to and from a Record (see details in SpectralElement)
  // <group>
  Bool fromRecord (String& errMsg, const RecordInterface& container);
  Bool toRecord(RecordInterface& container) const;
  //</group>

 private:
  //#Data
  // Max length allowed of list
  uInt nmax_p;
  // List of elements
  PtrBlock<SpectralElement *> list_p;

  //# Member functions
  // Compare two elements
  Int compar(const SpectralElement &p1, const SpectralElement &p2) const;

};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
std::ostream &operator<<(std::ostream &os, const SpectralList &lst);
// </group>


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <components/SpectralComponents/SpectralList2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
