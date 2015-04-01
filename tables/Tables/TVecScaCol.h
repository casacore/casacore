//# TVecScaCol.h: Templated table scalar column vectors
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

#ifndef TABLES_TVECSCACOL_H
#define TABLES_TVECSCACOL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TVec.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableColumn;
template<class T> class ScalarColumn;
class String;


// <summary>
// Templated table scalar column vectors
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TabVecRep
// </prerequisite>

// <etymology>
// TabVecScaCol is the class dealing with a table vector representing
// a column of scalars in a table.
// </etymology>

// <synopsis> 
// TabVecScaCol objects are a view on a column of scalars in a table.
// The semantics of these table vectors are the same as the normal
// vectors. So for example, changing an element in the table vector
// means changing the corresponding field in the underlying table.
// </synopsis> 

// <motivation>
// TabVecScaCol is derived from TabVecRep and as such it is a letter for
// the envelope class TableVector.
// </motivation>

// <templating arg=T>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
// </templating>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


template<class T>
class TabVecScaCol : public TabVecRep<T>
{
  //# Make members of parent class known.
protected:
  using TabVecRep<T>::tag_p;
  using TabVecRep<T>::nrel_p;

public:
    // Create a table vector from the given table column.
    // This constructor is for TableVector and allows elements to be changed.
    TabVecScaCol (const TableColumn& column);

    // Destruct the object.
    ~TabVecScaCol ();

    // Nr of elements (ie. #rows in table).
    uInt nelem() const;

    // Get a value.
    T value (uInt index) const;

    // Get a value.
    void getVal (uInt index, T&) const;

    // Put a value.
    void putVal (uInt index, const T&);

    // Set entire vector to a value.
    void set (const T&);

protected:
    ScalarColumn<T>*  colPtr_p;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TVecScaCol.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
