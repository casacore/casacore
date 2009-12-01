//# SparseDiffRep.h: Representation of an automatic differential class data
//# Copyright (C) 2007
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
//# $Id: SparseDiffRep.h,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

#ifndef SCIMATH_SPARSEDIFFREP_H
#define SCIMATH_SPARSEDIFFREP_H

//# Includes
#include <casa/aips.h>
#include <casa/vector.h>
#include <utility>

// Using
using std::pair;

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward declarations

  // <summary>
  // Representation of data for the spare automatic differentiation calss.
  // </summary>
  //
  // <use visibility=local>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tSparseDiff.cc" demos="">
  // </reviewed>
  //
  // <prerequisite>
  // <li> <linkto class=SparseDiff>SparseDiff</linkto>
  // </prerequisite>
  //
  // <etymology>
  // Class that represents partial derivatives obtained by automatic
  // differentiation.
  // </etymology>
  //
  // <synopsis>
  // Class representing
  // the data necessary for automatic differentiation. The structure contains a
  // value, and the derivatives of the value with respect to a number of 
  // dependent variables.
  //
  // The actual differentiation and access is done through the 
  // <linkto class=SparseDiff>SparseDiff</linkto> class.
  //
  // <example>
  // See the example in <linkto class=SparseDiff>SparseDiff</linkto>
  // </example>
  //
  // <motivation>
  // To separate the data container from the actual calculations.
  // To be able to create special conatiners; constructors and destructors
  // (including memory allocation) to speed up processes.
  //
  // <templating arg=T>
  //  <li> any class that has the standard mathematical and comparison
  //	operators defined
  // </templating>
  //
  // <todo asof="2007/11/27">
  // <li> Nothing I know of
  // </todo>

  template <class T> class SparseDiffRep {
  public:
    //# Typedefs
    typedef T 			value_type;
    typedef value_type&		reference;
    typedef const value_type&	const_reference;
    typedef value_type*		iterator;
    typedef const value_type*	const_iterator;

    //# Constructors
    // Construct a constant with a value of zero.  Zero derivatives.
    SparseDiffRep();

    //# Operators
    // Assignment operators
    // <group>
    SparseDiffRep<T> &operator=(const T &v);
    SparseDiffRep<T> &operator=(const vector<pair<uInt, T> > &grad);
    SparseDiffRep<T> &operator=(const SparseDiffRep<T> &other);
    void operator*=(const T other);
    void operator/=(const T other);
    void operator+=(const T other);
    void operator-=(const T other);
    // </group>

    //# Member functions
  
    // Clear for reuse
    void clear() { grad_p.clear(); }

    //# Data
    // The function value
    T val_p;
    // The derivatives
    vector<pair<uInt, T> > grad_p;
    // Link to indicate its status (1=linked in stack; 2=used in modules)
    uInt link_p;
  };


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Mathematics/SparseDiffRep.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
