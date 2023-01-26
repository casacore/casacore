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
    bool tabVecReptvLE (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecReptvLT (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecReptvGE (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecReptvGT (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecReptvEQ (const TabVecRep<T>& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecReptvNE (const TabVecRep<T>& left, const TabVecRep<T>& right);
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
    bool tabVecRepvalrLE (const TabVecRep<T>& left, const T& right);
template<class T>
    bool tabVecRepvallLE (const T& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecRepvalrLT (const TabVecRep<T>& left, const T& right);
template<class T>
    bool tabVecRepvallLT (const T& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecRepvalrGE (const TabVecRep<T>& left, const T& right);
template<class T>
    bool tabVecRepvallGE (const T& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecRepvalrGT (const TabVecRep<T>& left, const T& right);
template<class T>
    bool tabVecRepvallGT (const T& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecRepvalrEQ (const TabVecRep<T>& left, const T& right);
template<class T>
    bool tabVecRepvallEQ (const T& left, const TabVecRep<T>& right);
template<class T>
    bool tabVecRepvalrNE (const TabVecRep<T>& left, const T& right);
template<class T>
    bool tabVecRepvallNE (const T& left, const TabVecRep<T>& right);
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TVecLogic.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
