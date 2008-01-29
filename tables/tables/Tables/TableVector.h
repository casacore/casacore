//# TableVector.h: Templated readonly table column vectors
//# Copyright (C) 1994,1995,1996,1999,2000
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

#ifndef TABLES_TABLEVECTOR_H
#define TABLES_TABLEVECTOR_H

//# Includes
#include <casa/aips.h>
#include <tables/Tables/TVec.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class Table;
class TableColumn;
class ROTableColumn;
template<class T> class TableVectorHelper;
class String;


// <summary>
// Templated readonly table column vectors
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Vector
//   <li> Table
// </prerequisite>

// <etymology>
// ROTableVector allows to operate on a column in a readonly table as a vector.
// </etymology>

// <synopsis> 
// A ROTableVector object is a readonly view of data in a Table.
// This means that the vector data is readonly and cannot be changed.
//
// Table vectors can be used in the same way as the normal vectors.
// They allow to handle a column in a table as a vector.
// Many mathematical and logical operations are defined for them
// in TabVecMath.h and TabVecLogic.h. In fact, constructors exist
// to convert a ROTableColumn or a Vector object to a ROTableVector,
// so they can often directly be used in a table vector expression.
// There are 2 kinds of table vectors:
// <ul>
//  <li> A table vector representing a scalar column in a table.
//         The data types of the vector and the column must conform.
// </li> A temporary vector, which is held in memory.
//         These are usually the result of operations on table vectors.
// </ul>
//
// ROTableVector is implemented by referencing the counted TabVecRep object.
// A default constructor is defined to allow construction of an array
// of ROTableVector objects. However, this constructs an object not
// referencing anything. Functions like operator() will fail (i.e. result
// in a segmentation fault) when used on such objects. The functions
// isNull and throwIfNull can be used to test on this.
// </synopsis> 

// <example>
// <srcblock>
//    // Create a table vector for column COL1.
//    Table tab ("Table.data");
//    ROTableVector<Int> tabvec(tab, "COL1");
//    // Multiply it by a constant.
//    // The result has to be stored in a TableVector,
//    // since a ROTableVector cannot be written.
//    TableVector<Int> temp = 2 * tabvec;
// </srcblock>
// </example>

// <motivation>
// It is very useful to be able to handle a column as a vector.
// To handle a column in a readonly table, a ROTableVector class
// is needed, otherwise output operations could not be forbidden.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> derive from Lattice one day
//   <li> support slicing
//   <li> support table array columns
//   <li> do we ever need Row vectors?
// </todo>


template<class T>
class ROTableVector
{
public:
    // The default constructor creates a null table vector.
    // This does not contain an actual vector and cannot be used until
    // it references an actual vector (using function reference).
    // Its sole purpose is to be able to construct an array of ROTableVectors.
    // Note that operator(), etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    ROTableVector();

    // Create a readonly table vector from the given table column name.
    // Only scalar columns are supported.
    ROTableVector (const Table&, const String& columnName);

    // Create a readonly table vector from the given table column.
    // Only scalar columns are supported.
    // This constructor converts a ROTableColumn to a ROTableVector and
    // allows the use of ROTableColumn objects in table vector expressions.
    ROTableVector (const ROTableColumn& column);

    // Create a table vector from another one (reference semantics)
    ROTableVector (const ROTableVector<T>&);

    // Create table vector containing given Vector (reference semantics)
    // This constructor converts a Vector to a ROTableVector and
    // allows the use of Vector objects in table vector expressions.
    ROTableVector (const Vector<T>&);

    // Destruct the object.
    ~ROTableVector();

    // Test if the table vector is null, i.e. has no actual vector.
    // This is the case if the default constructor has been used.
    inline Bool isNull() const;

    // Throw an exception if the table vector is null, i.e.
    // if function isNull() is true.
    void throwIfNull() const;

    // Make a reference to the table vector of the other ROTableVector.
    // It will replace an already existing reference.
    // It handles null objects correctly.
    void reference (const ROTableVector<T>&);

    // Make a (normal) Vector from a ROTableVector (copy semantics).
    Vector<T> makeVector() const;

    // Get the value of a single pixel.
    inline T operator() (uInt index) const;

    //# Get a slice.
//#    ROTableVector<T> operator() (const NSlice&) const;

    // Get nr of dimensions (is always 1).
    inline uInt ndim() const;

    // Get nr of elements (ie. vector length).
    inline uInt nelements() const;

    // Test if the shape of the given table vector conforms.
    inline Bool conform (const ROTableVector<T>&) const;

    // Test if the shape of the given vector conforms.
    inline Bool conform (const Vector<T>&) const;

    // Test if internal state is correct.
    Bool ok() const;

protected:
    TabVecRep<T>* tabVecPtr_p;

    // Check if a new TabxxxVec succeeded; if so, link to it.
    void checkLink();

    // Destruct the object. It decreases the reference count in the
    // underlying object.
    void destruct();

private:
    // Assigning to a ROTableVector is impossible, because the
    // vector is readonly.
    ROTableVector<T>& operator=(const ROTableVector<T>&);

public:
    // Return the TabVecRep reference.
    inline const TabVecRep<T>& tabVec() const;
};





// <summary>
// Templated read/write table column vectors
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> ROTableVector
// </prerequisite>

// <etymology>
// TableVector allows to operate on a column in a table as a vector.
// </etymology>

// <synopsis> 
// TableVector is similar to class
// <linkto class="ROTableVector:description">ROTableVector</linkto>.
// The only difference is that it references a column in a Table
// object, so it can be written.
// </synopsis> 

// <example>
// <srcblock>
//    // Create a table vector for column COL1.
//    Table tab ("Table.data");
//    TableVector<Int> tabvec(tab, "COL1");
//    // Multiply it by a constant.
//    // The result is stored back in the vector, thus in the
//    // underlying column.
//    tabvec *= 2;
// </srcblock>
// </example>

// <motivation>
// It is very useful to be able to handle a column as a vector.
// It allows to manipulate the data in a very convenient way.
// </motivation>

// <templating arg=T>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
// </templating>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> derive from Lattice one day
//   <li> support slicing
//   <li> support table array columns
//   <li> do we ever need Row vectors?
// </todo>


template<class T>
class TableVector : public ROTableVector<T>
{
  //# Make members of parent class known.
protected:
  using ROTableVector<T>::checkLink;
  using ROTableVector<T>::tabVecPtr_p;

public:
    // The default constructor creates a null table vector.
    // This does not contain an actual vector and cannot be used until
    // it references an actual vector (using function reference).
    // Its purpose is to be able to construct an array of TableVectors.
    // Note that operator(), etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    TableVector();

    // Create a table vector from the given table column name.
    // Only scalar columns are supported.
    TableVector (const Table&, const String&);

    // Create a table vector from the given table column.
    // Only scalar columns are supported.
    TableVector (const TableColumn&);

    // Create a table vector from another one (reference semantics)
    TableVector (const TableVector<T>&);

    // Create table vector containing given Vector (reference semantics)
    TableVector (const Vector<T>&);

    // Create table vector containing a Vector with given length.
    TableVector (uInt leng);

    // Destruct the object.
    ~TableVector();

    // Make a reference to another TableVector.
    void reference (const TableVector<T>&);

    // Assign a table vector to another one (copy semantics)
    // <group>
    inline TableVector<T>& operator= (const TableVector<T>&);
    inline TableVector<T>& operator= (const ROTableVector<T>&);
    // </group>

    // Set all elements to a value.
    // <group>
    inline TableVector<T>& operator= (const T&);
    inline void set (const T& value);
    // </group>

    // Put a value into a single pixel.
    // <br><src> tabvec(i) = value; </src>
    inline void set (uInt index, const T& value);

    //# Get a slice.
//#//    TableVector<T> operator() (const NSlice&);

    // Return the TabVecRep reference.
    inline TabVecRep<T>& tabVec();

    // Create a TableVector from a TabVecRep as result of an operation.
    inline TableVector (TabVecRep<T>&);
};


template<class T>
inline Bool ROTableVector<T>::isNull() const
    { return (tabVecPtr_p == 0  ?  True : False); }

template<class T>
inline uInt ROTableVector<T>::ndim () const
    { return tabVecPtr_p->ndim(); }

template<class T>
inline uInt ROTableVector<T>::nelements() const
    { return tabVecPtr_p->nelements(); }

//# Check if 2 table vectors are conformant.
template<class T>
inline Bool ROTableVector<T>::conform (const ROTableVector<T>& vec) const
    { return tabVecPtr_p->conform (*vec.tabVecPtr_p); }
template<class T>
inline Bool ROTableVector<T>::conform (const Vector<T>& vec) const
    { return tabVecPtr_p->conform (vec); }

//# Get the ith pixel.
template<class T>
inline T ROTableVector<T>::operator() (uInt index) const
    { return tabVecPtr_p->value (index); }

//# Return the TabVecRep (for TabVecMath and Logic).
template<class T>
inline const TabVecRep<T>& ROTableVector<T>::tabVec() const
    { return *tabVecPtr_p; }


//# Create a new object as a result of an addition, etc..
template<class T>
inline TableVector<T>::TableVector (TabVecRep<T>& vec)
    { tabVecPtr_p = vec.link(); }

//# Assign a table vector to this one.
template<class T>
inline TableVector<T>& TableVector<T>::operator= (const ROTableVector<T>& that)
{
    tabVecPtr_p->assign (that.tabVec());
    return *this;
}
template<class T>
inline TableVector<T>& TableVector<T>::operator= (const TableVector<T>& that)
{
    return operator= ((const ROTableVector<T>&)that);
}

template<class T>
inline void TableVector<T>::set (uInt index, const T& value)
{
    tabVecPtr_p->putVal (index, value);
}
template<class T>
inline void TableVector<T>::set (const T& value)
{
    tabVecPtr_p->set (value);
}
template<class T>
inline TableVector<T>& TableVector<T>::operator= (const T& value)
{
    tabVecPtr_p->set (value);
    return *this;
}

//# Return the TabVecRep (for TabVecMath and Logic).
template<class T>
inline TabVecRep<T>& TableVector<T>::tabVec()
    { return *tabVecPtr_p; }




} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <tables/Tables/TableVector.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
