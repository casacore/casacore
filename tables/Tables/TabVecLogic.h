//# TabVecLogic.h: Global functions for table vector logical operations
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

#ifndef TABLES_TABVECLOGIC_H
#define TABLES_TABVECLOGIC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/tables/Tables/TVecLogic.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Comparison between two table vectors
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Element by element comparisons between the left and right table vectors.
// The result is true only if the comparison is true for every element
// of the table vectors.
// At some point operators will be available that return masks where the
// comparison is true.
// The left and right operands must be conformant (i.e. have equal length).
// </synopsis>

// <group name=vectorComparison>
template<class T> inline
    Bool allLE (const TableVector<T>& left, const TableVector<T>& right);
template<class T> inline
    Bool allLT (const TableVector<T>& left, const TableVector<T>& right);
template<class T> inline
    Bool allGE (const TableVector<T>& left, const TableVector<T>& right);
template<class T> inline
    Bool allGT (const TableVector<T>& left, const TableVector<T>& right);
template<class T> inline
    Bool allEQ (const TableVector<T>& left, const TableVector<T>& right);
template<class T> inline
    Bool allNE (const TableVector<T>& left, const TableVector<T>& right);
// </group>



// <summary>
// Comparison between a table vector and a scalar
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Element by element comparisons between a table vector and a scalar,
// which behaves as if it were a conformant table vector filled with the
// scalar value.
// At some point operators will be available that return masks where the
// comparison is true.
// </synopsis>

// <group name=scalarComparison>
template<class T> inline
    Bool allLE (const TableVector<T>& left, const T& right);
template<class T> inline
    Bool allLE (const T& left, const TableVector<T>& right);
template<class T> inline
    Bool allLT (const TableVector<T>& left, const T& right);
template<class T> inline
    Bool allLT (const T& left, const TableVector<T>& right);
template<class T> inline
    Bool allGE (const TableVector<T>& left, const T& right);
template<class T> inline
    Bool allGE (const T& left, const TableVector<T>& right);
template<class T> inline
    Bool allGT (const TableVector<T>& left, const T& right);
template<class T> inline
    Bool allGT (const T& left, const TableVector<T>& right);
template<class T> inline
    Bool allEQ (const TableVector<T>& left, const T& right);
template<class T> inline
    Bool allEQ (const T& left, const TableVector<T>& right);
template<class T> inline
    Bool allNE (const TableVector<T>& left, const T& right);
template<class T> inline
    Bool allNE (const T& left, const TableVector<T>& right);
// </group>


//# Implement all functions inline.
//# The actual work is done in TVecLogic.cc.
//#
#define TABVECLOGICOPER(NAME) \
template<class T> inline \
Bool aips_name2(all,NAME) (const TableVector<T>& l, \
			   const TableVector<T>& r) \
    { return aips_name2(tabVecReptv,NAME) (l.tabVec(), r.tabVec()); } \
template<class T> inline \
Bool aips_name2(all,NAME) (const T& val, const TableVector<T>& tv) \
    { return aips_name2(tabVecRepvall,NAME) (val, tv.tabVec()); } \
template<class T> inline \
Bool aips_name2(all,NAME) (const TableVector<T>& tv, const T& val) \
    { return aips_name2(tabVecRepvalr,NAME) (tv.tabVec(), val); }

TABVECLOGICOPER(LE)
TABVECLOGICOPER(LT)
TABVECLOGICOPER(GE)
TABVECLOGICOPER(GT)
TABVECLOGICOPER(EQ)
TABVECLOGICOPER(NE)


// 
// Element by element comparisons between the "l" and "r" table vectors. The
// result is true if the comparison is true for some element of the vectors.
// At some point operators will be available that return masks where the
// comparison is true. The vectors must conform or an exception is thrown.
template<class T> inline
Bool anyLE (const TableVector<T>& l, const TableVector<T>& r)
{
    return (allGT (l, r)  ?  False : True);
}
template<class T> inline
Bool anyLT (const TableVector<T>& l, const TableVector<T>& r)
{
    return (allGE (l, r)  ?  False : True);
}
template<class T> inline
Bool anyGE (const TableVector<T>& l, const TableVector<T>& r)
{
    return (allLT (l, r)  ?  False : True);
}
template<class T> inline
Bool anyGT (const TableVector<T>& l, const TableVector<T>& r)
{
    return (allLE (l, r)  ?  False : True);
}
template<class T> inline
Bool anyEQ (const TableVector<T>& l, const TableVector<T>& r)
{
    return (allNE (l, r)  ?  False : True);
}
template<class T> inline
Bool anyNE (const TableVector<T>& l, const TableVector<T>& r)
{
    return (allEQ (l, r)  ?  False : True);
}


// 
// Element by element comparisons between a table vector and a scalar, which
// behaves as if it were a conformant vector filled with the value "val."
// The result is true if the comparison is true for some element of the vector.
// At some point operators will be available that return masks where the
// comparison is true.
template<class T> inline
Bool anyLE (const TableVector<T>& tv, const T &val)
{
    return (allGT (tv, val)  ?  False : True);
}
template<class T> inline
Bool anyLE (const T &val, const TableVector<T>& tv)
{
    return (allGT (val, tv)  ?  False : True);
}
template<class T> inline
Bool anyLT (const TableVector<T>& tv, const T &val)
{
    return (allGE (tv, val)  ?  False : True);
}
template<class T> inline
Bool anyLT (const T &val, const TableVector<T>& tv)
{
    return (allGE (val, tv)  ?  False : True);
}
template<class T> inline
Bool anyGE (const TableVector<T>& tv, const T &val)
{
    return (allLT (tv, val)  ?  False : True);
}
template<class T> inline
Bool anyGE (const T &val, const TableVector<T>& tv)
{
    return (allLT (val, tv)  ?  False : True);
}
template<class T> inline
Bool anyGT (const TableVector<T>& tv, const T &val)
{
    return (allLE (tv, val)  ?  False : True);
}
template<class T> inline
Bool anyGT (const T &val, const TableVector<T>& tv)
{
    return (allLE (val, tv)  ?  False : True);
}
template<class T> inline
Bool anyEQ (const TableVector<T>& tv, const T &val)
{
    return (allNE (tv, val)  ?  False : True);
}
template<class T> inline
Bool anyEQ (const T &val, const TableVector<T>& tv)
{
    return (allNE (val, tv)  ?  False : True);
}
template<class T> inline
Bool anyNE (const TableVector<T>& tv, const T &val)
{
    return (allEQ (tv, val)  ?  False : True);
}
template<class T> inline
Bool anyNE (const T &val, const TableVector<T>& tv)
{
    return (allEQ (val, tv)  ?  False : True);
}



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TabVecLogic.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
