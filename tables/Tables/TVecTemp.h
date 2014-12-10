//# TVecTemp.h: Templated table vectors held in memory as a temporary
//# Copyright (C) 1994,1995,1999
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
//# $Id$

#ifndef TABLES_TVECTEMP_H
#define TABLES_TVECTEMP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TVec.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Vector;


// <summary>
// Templated table vectors held in memory as a temporary
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TabVecRep
// </prerequisite>

// <etymology>
// TabVecTemp is the class dealing with a table vector when used as
// a temporary in math operations.
// </etymology>

// <synopsis> 
// TabVecTemp objects enable the use of Vector objects as table vectors.
// They are used for 2 purposes:
// <ol>
// <li> To convert a Vector to a TableVector. This is used to
//        allow the use of Vectors in TableVector expressions.
//        The TabVecTemp object uses the Vector copy constructor,
//        which is very cheap due to its reference semantics.
// <li> To hold the result of an operation (like addition) on
//        two TableVector objects.
// </ol>
// </synopsis> 

// <motivation>
// TabVecTemp is derived from TabVecRep and as such a letter for
// the envelope class TableVector.
// </motivation>

// <templating arg=T>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
// </templating>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> In the future temporary results may need to use a file,
//          because table vectors can potentially be very, very long.
// </todo>


template<class T> class TabVecTemp : public TabVecRep<T>
{
  //# Make members of parent class known.
protected:
  using TabVecRep<T>::tag_p;
  using TabVecRep<T>::nrel_p;

public:
    // Create table vector containing the given Vector (reference semantics).
    // It will set the origin to zero.
    TabVecTemp (const Vector<T>&);

    // Create table vector containing a Vector with given length.
    TabVecTemp (uInt leng);

    // Destruct the object.
    ~TabVecTemp();

    // Return a reference to a value.
    inline const T& operator() (uInt index) const;

    // Return a reference to a value.
    inline T& operator() (uInt index);

    // Get a value (virtual function).
    T value (uInt index) const;
    // Get a value (virtual function).
    void getVal (uInt index, T&) const;

    // Put a value (virtual function).
    void putVal (uInt index, const T&);

    // Set entire vector to a value.
    void set (const T&);

protected:
    Vector<T>* vecPtr_p;
};



//# Return a reference to a value.
template<class T>
inline const T& TabVecTemp<T>::operator() (uInt index) const
    { return (*vecPtr_p)(index); }
template<class T>
inline T& TabVecTemp<T>::operator() (uInt index)
    { return (*vecPtr_p)(index); }



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TVecTemp.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
