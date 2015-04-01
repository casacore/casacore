//# TVecLogic.h: Internal functions for table vector logical operations
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

#ifndef TABLES_TVECLOGIC_H
#define TABLES_TVECLOGIC_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class TabVecRep;



// <summary>
// Comparison between two table vectors
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Element by element comparisons between two table vectors
// or between a table vector and a scalar.
// The result is true only if the comparison is true for every element
// of the table vectors.
// At some point operators will be available that return masks where the
// comparison is true.
// The left and right operands must be conformant (i.e. have equal length).
// The functions are the implementation of the wrapper functions
// defined in TabVecLogic.h.
// </synopsis>

// <group name=vectorComparison>
template<class T>
    Bool tabVecReptvLE (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecReptvLT (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecReptvGE (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecReptvGT (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecReptvEQ (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecReptvNE (const TabVecRep<T>& left, const TabVecRep<T>& right);
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
// The functions are the implementation of the wrapper functions
// defined in TabVecLogic.h.
// </synopsis>

// <group name=scalarComparison>
template<class T>
    Bool tabVecRepvalrLE (const TabVecRep<T>& left, const T& right);
template<class T>
    Bool tabVecRepvallLE (const T& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecRepvalrLT (const TabVecRep<T>& left, const T& right);
template<class T>
    Bool tabVecRepvallLT (const T& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecRepvalrGE (const TabVecRep<T>& left, const T& right);
template<class T>
    Bool tabVecRepvallGE (const T& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecRepvalrGT (const TabVecRep<T>& left, const T& right);
template<class T>
    Bool tabVecRepvallGT (const T& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecRepvalrEQ (const TabVecRep<T>& left, const T& right);
template<class T>
    Bool tabVecRepvallEQ (const T& left, const TabVecRep<T>& right);
template<class T>
    Bool tabVecRepvalrNE (const TabVecRep<T>& left, const T& right);
template<class T>
    Bool tabVecRepvallNE (const T& left, const TabVecRep<T>& right);
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TVecLogic.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
