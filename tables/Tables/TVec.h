//# TVec.h: Templated base class for table vectors
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

#ifndef TABLES_TVEC_H
#define TABLES_TVEC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Enumeration of possible table vectors
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
// <use visibility=local>
// <synopsis>
// Define the type of table vectors.
// Alas, this enum has to be defined outside the class, because
// some compilers do not support an enum in a templated class.
// </synopsis>
// <group name=enum>
enum TabVecTag {
    // Table Vector is a scalar column
    TagScaCol   = 1,
    // Table Vector is a temporary vector (i.e. a regular vector).
    TagTemp     = 2
};
// </group>



// <summary>
// Templated base class for table vectors
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableVector
// </prerequisite>

// <etymology>
// TabVecRep is the representation of a table vector.
// </etymology>

// <synopsis> 
// TabVecRep is the counted referenced letter class for the envelope
// class TableVector. It is an abstract base class for the actual
// table vector classes TabVecScaCol and TabVecTemp.
//
// All operations defined for TableVector are immediately passed to
// the corresponding virtual TabVecRep function.
// The header files TVecMath.h and TVecLogic.h declare all the
// mathematical and logical functions for TabVecRep.
// </synopsis> 

// <motivation>
// A virtual function call only works when used with an object
//  pointer or reference. To allow the use of virtual functions
// in value objects, an extra level of indirection is used.
// This is called the letter/envelope idiom and is described in
// "Advanced C++" by J. Coplien.
// Class TableVector is the envelope to the letters TabVecRep and
// its derivations.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> put the TabVecTag enum inside the class definition
//   <li> support array columns
// </todo>


template<class T>
class TabVecRep
{
public:

    // Create empty table vector.
    // TabVecRep cannot be contructed by the user, because it is an
    // abstract base class (it contains pure virtual functions).
    TabVecRep();

    // Destruct the object.
    virtual ~TabVecRep();

    // Get nr of dimensions.
    inline uInt ndim() const;

    // Get nr of elements (ie. vector length).
    inline uInt nelements() const;

    // Test if vector shape conforms another table vector.
    inline Bool conform(const TabVecRep<T>&) const;

    // Test if vector shape conforms another vector.
    inline Bool conform(const Vector<T>&) const;

    // Check internal consistency.
    Bool ok() const;

    // Increments the reference count.
    inline TabVecRep<T>* link();

    // Decrements the reference count and returns the resulting count.
    inline uInt unlink();

    // Get the tag (the type of vector).
    inline TabVecTag getTag() const;

    // Get a value.
    virtual T value (uInt index) const = 0;

    // Get a value.
    virtual void getVal (uInt index, T&) const = 0;

    // Put a value.
    virtual void putVal (uInt index, const T&) = 0;

    // Set entire vector to a value.
    virtual void set (const T&) = 0;

    // Set to another table vector.
    virtual void assign (const TabVecRep<T>&);

protected:
    uInt      count_p;               //# reference count
    TabVecTag tag_p;
    Int       nrel_p;                //# #elements (<0 = ask derived class)

    // Get nr of elements.
    virtual uInt nelem() const;

public:
    // Check if vectors are comformant.
    void validateConformance (uInt) const;

    // Create a new temporary vector (for result of math operations).
    // TabVecTemp<T>& cannot be used, because the template instantiation
    // mechanism instantiates TabVecTemp, which depends on TabVecRep and
    // therefore gives errors.
    void* newVec() const;
};



template<class T>
inline uInt TabVecRep<T>::ndim() const
    { return 1; }

template<class T>
inline uInt TabVecRep<T>::nelements() const
    { return (nrel_p<0  ?  nelem() : nrel_p); }

//# Check if 2 table vectors are conformant.
template<class T>
inline Bool TabVecRep<T>::conform (const TabVecRep<T>& vec) const
    { return (nelements() == vec.nelements()  ?  True : False); }
template<class T>
inline Bool TabVecRep<T>::conform (const Vector<T>& vec) const
    { return (nelements() == vec.nelements()  ?  True : False); }

//# Maintain reference count.
template<class T>
inline TabVecRep<T>* TabVecRep<T>::link()
{
    count_p++;
    return this;
}
template<class T>
inline uInt TabVecRep<T>::unlink()
    { return --count_p; }

//# Return the tag.
template<class T>
inline TabVecTag TabVecRep<T>::getTag() const
    { return tag_p; }




} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TVec.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
