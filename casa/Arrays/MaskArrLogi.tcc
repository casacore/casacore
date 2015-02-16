//# MaskArrLogi.cc: Element by element logical operations on arrays.
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

#ifndef CASA_MASKARRLOGI_TCC
#define CASA_MASKARRLOGI_TCC

#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define MARRLOGI_B_ALLFUNC_MA(ALLFUNC,OP,STRALLFUNC) \
template<class T> \
Bool ALLFUNC (const MaskedArray<T> &left, const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
         ("Bool ::" STRALLFUNC "(const MaskedArray<T> &, const Array<T> &)" \
          " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage = \
        left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    Bool rightDelete; \
    const T *rightStorage = right.getStorage(rightDelete); \
    const T *rightS = rightStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = True; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*leftmaskS) { \
            foundOne = True; \
            if (! (*leftarrS OP *rightS) ) { \
	        retval = False; \
	        break; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeStorage(rightStorage, rightDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRALLFUNC "(const MaskedArray<T> &, const Array<T> &)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


#define MARRLOGI_B_ALLFUNC_AM(ALLFUNC,OP,STRALLFUNC) \
template<class T> \
Bool ALLFUNC (const Array<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
            ("Bool ::" STRALLFUNC "(Array<T> &, const MaskedArray<T> &)" \
             " - arrays do not conform")); \
    } \
\
    Bool leftDelete; \
    const T *leftStorage = left.getStorage(leftDelete); \
    const T *leftS = leftStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage = \
        right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = True; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*rightmaskS) { \
            foundOne = True; \
            if (! (*leftS OP *rightarrS) ) { \
                retval = False; \
                break; \
            } \
        } \
        leftS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.freeStorage(leftStorage, leftDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRALLFUNC "(const Array<T> &, const MaskedArray<T> &)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


#define MARRLOGI_B_ALLFUNC_MM(ALLFUNC,OP,STRALLFUNC) \
template<class T> \
Bool ALLFUNC (const MaskedArray<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
         ("Bool ::" STRALLFUNC "(const MaskedArray<T> &," \
          " const MaskedArray<T> &)" \
          " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage \
        = left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage \
        = right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = True; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*leftmaskS && *rightmaskS) { \
            foundOne = True; \
            if (! (*leftarrS OP *rightarrS) ) { \
                retval = False; \
                break; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRALLFUNC "(const MaskedArray<T> &," \
          " const MaskedArray<T> &)" \
          " - AND of MaskedArray masks must have at least 1 element")); \
    } \
\
    return retval; \
}


MARRLOGI_B_ALLFUNC_MA ( allLE,  <=, "allLE" )
MARRLOGI_B_ALLFUNC_MA ( allLT,  <,  "allLT" )
MARRLOGI_B_ALLFUNC_MA ( allGE,  >=, "allGE" )
MARRLOGI_B_ALLFUNC_MA ( allGT,  >,  "allGT" )
MARRLOGI_B_ALLFUNC_MA ( allEQ,  ==, "allEQ" )
MARRLOGI_B_ALLFUNC_MA ( allNE,  !=, "allNE" )
MARRLOGI_B_ALLFUNC_MA ( allAND, &&, "allAND" )
MARRLOGI_B_ALLFUNC_MA ( allOR,  ||, "allOR" )

MARRLOGI_B_ALLFUNC_AM ( allLE,  <=, "allLE" )
MARRLOGI_B_ALLFUNC_AM ( allLT,  <,  "allLT" )
MARRLOGI_B_ALLFUNC_AM ( allGE,  >=, "allGE" )
MARRLOGI_B_ALLFUNC_AM ( allGT,  >,  "allGT" )
MARRLOGI_B_ALLFUNC_AM ( allEQ,  ==, "allEQ" )
MARRLOGI_B_ALLFUNC_AM ( allNE,  !=, "allNE" )
MARRLOGI_B_ALLFUNC_AM ( allAND, &&, "allAND" )
MARRLOGI_B_ALLFUNC_AM ( allOR,  ||, "allOR" )

MARRLOGI_B_ALLFUNC_MM ( allLE,  <=, "allLE" )
MARRLOGI_B_ALLFUNC_MM ( allLT,  <,  "allLT" )
MARRLOGI_B_ALLFUNC_MM ( allGE,  >=, "allGE" )
MARRLOGI_B_ALLFUNC_MM ( allGT,  >,  "allGT" )
MARRLOGI_B_ALLFUNC_MM ( allEQ,  ==, "allEQ" )
MARRLOGI_B_ALLFUNC_MM ( allNE,  !=, "allNE" )
MARRLOGI_B_ALLFUNC_MM ( allAND, &&, "allAND" )
MARRLOGI_B_ALLFUNC_MM ( allOR,  ||, "allOR" )


#define MARRLOGI_B_ANYFUNC_MA(ANYFUNC,OP,STRANYFUNC) \
template<class T> \
Bool ANYFUNC (const MaskedArray<T> &left, const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
         ("Bool ::" STRANYFUNC "(const MaskedArray<T> &, const Array<T> &)" \
          " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage = \
        left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    Bool rightDelete; \
    const T *rightStorage = right.getStorage(rightDelete); \
    const T *rightS = rightStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = False; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*leftmaskS) { \
            foundOne = True; \
            if (*leftarrS OP *rightS) { \
	        retval = True; \
	        break; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeStorage(rightStorage, rightDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRANYFUNC "(const MaskedArray<T> &, const Array<T> &)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


#define MARRLOGI_B_ANYFUNC_AM(ANYFUNC,OP,STRANYFUNC) \
template<class T> \
Bool ANYFUNC (const Array<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
            ("Bool ::" STRANYFUNC "(Array<T> &, const MaskedArray<T> &)" \
             " - arrays do not conform")); \
    } \
\
    Bool leftDelete; \
    const T *leftStorage = left.getStorage(leftDelete); \
    const T *leftS = leftStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage = \
        right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = False; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*rightmaskS) { \
            foundOne = True; \
            if (*leftS OP *rightarrS) { \
                retval = True; \
                break; \
            } \
        } \
        leftS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.freeStorage(leftStorage, leftDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRANYFUNC "(const Array<T> &, const MaskedArray<T> &)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


#define MARRLOGI_B_ANYFUNC_MM(ANYFUNC,OP,STRANYFUNC) \
template<class T> \
Bool ANYFUNC (const MaskedArray<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
         ("Bool ::" STRANYFUNC "(const MaskedArray<T> &," \
          " const MaskedArray<T> &)" \
          " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage \
        = left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage \
        = right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = False; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*leftmaskS && *rightmaskS) { \
            foundOne = True; \
            if (*leftarrS OP *rightarrS) { \
                retval = True; \
                break; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRANYFUNC "(const MaskedArray<T> &," \
          " const MaskedArray<T> &)" \
          " - AND of MaskedArray masks must have at least 1 element")); \
    } \
\
    return retval; \
}


MARRLOGI_B_ANYFUNC_MA ( anyLE,  <=, "anyLE" )
MARRLOGI_B_ANYFUNC_MA ( anyLT,  <,  "anyLT" )
MARRLOGI_B_ANYFUNC_MA ( anyGE,  >=, "anyGE" )
MARRLOGI_B_ANYFUNC_MA ( anyGT,  >,  "anyGT" )
MARRLOGI_B_ANYFUNC_MA ( anyEQ,  ==, "anyEQ" )
MARRLOGI_B_ANYFUNC_MA ( anyNE,  !=, "anyNE" )
MARRLOGI_B_ANYFUNC_MA ( anyAND, &&, "anyAND" )
MARRLOGI_B_ANYFUNC_MA ( anyOR,  ||, "anyOR" )

MARRLOGI_B_ANYFUNC_AM ( anyLE,  <=, "anyLE" )
MARRLOGI_B_ANYFUNC_AM ( anyLT,  <,  "anyLT" )
MARRLOGI_B_ANYFUNC_AM ( anyGE,  >=, "anyGE" )
MARRLOGI_B_ANYFUNC_AM ( anyGT,  >,  "anyGT" )
MARRLOGI_B_ANYFUNC_AM ( anyEQ,  ==, "anyEQ" )
MARRLOGI_B_ANYFUNC_AM ( anyNE,  !=, "anyNE" )
MARRLOGI_B_ANYFUNC_AM ( anyAND, &&, "anyAND" )
MARRLOGI_B_ANYFUNC_AM ( anyOR,  ||, "anyOR" )

MARRLOGI_B_ANYFUNC_MM ( anyLE,  <=, "anyLE" )
MARRLOGI_B_ANYFUNC_MM ( anyLT,  <,  "anyLT" )
MARRLOGI_B_ANYFUNC_MM ( anyGE,  >=, "anyGE" )
MARRLOGI_B_ANYFUNC_MM ( anyGT,  >,  "anyGT" )
MARRLOGI_B_ANYFUNC_MM ( anyEQ,  ==, "anyEQ" )
MARRLOGI_B_ANYFUNC_MM ( anyNE,  !=, "anyNE" )
MARRLOGI_B_ANYFUNC_MM ( anyAND, &&, "anyAND" )
MARRLOGI_B_ANYFUNC_MM ( anyOR,  ||, "anyOR" )


#define MARRLOGI_MLA_OP_MA(OP,STROP) \
template<class T> \
MaskedLogicalArray operator OP (const MaskedArray<T> &left, \
                                const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
               ("MaskedLogicalArray ::" STROP \
                "(const MaskedArray<T> &, const Array<T> &)" \
                " - arrays do not conform")); \
    } \
\
    LogicalArray resultarr (left.shape()); \
    resultarr = False; \
    MaskedLogicalArray result (resultarr, left.getMask()); \
\
    Bool resultarrDelete; \
    LogicalArrayElem *resultarrStorage = \
        result.getRWArrayStorage(resultarrDelete); \
    LogicalArrayElem *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool rightDelete; \
    const T *rightStorage = right.getStorage(rightDelete); \
    const T *rightS = rightStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            *resultarrS = (LogicalArrayElem) (*leftarrS OP *rightS); \
        } \
        resultarrS++; \
        resultmaskS++; \
        leftarrS++; \
        rightS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    right.freeStorage(rightStorage, rightDelete); \
\
    return result; \
}


#define MARRLOGI_MLA_OP_AM(OP,STROP) \
template<class T> \
MaskedLogicalArray operator OP (const Array<T> &left, \
                                const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
               ("MaskLogicalArray ::" STROP \
                "(const Array<T> &, const MaskedArray<T> &)" \
                " - arrays do not conform")); \
    } \
\
    LogicalArray resultarr (left.shape()); \
    resultarr = False; \
    MaskedLogicalArray result (resultarr, right.getMask()); \
\
    Bool resultarrDelete; \
    LogicalArrayElem *resultarrStorage = \
        result.getRWArrayStorage(resultarrDelete); \
    LogicalArrayElem *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool leftDelete; \
    const T *leftStorage = left.getStorage(leftDelete); \
    const T *leftS = leftStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            *resultarrS = (LogicalArrayElem) (*leftS OP *rightarrS); \
        } \
        resultarrS++; \
        resultmaskS++; \
        leftS++; \
        rightarrS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    left.freeStorage(leftStorage, leftDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
\
    return result; \
}


#define MARRLOGI_MLA_OP_MM(OP,STROP) \
template<class T> \
MaskedLogicalArray operator OP (const MaskedArray<T> &left, \
                                const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
        throw (ArrayConformanceError \
               ("MaskLogicalArray ::" STROP \
                "(const MaskedArray<T> &, const MaskedArray<T> &)" \
                " - arrays do not conform")); \
    } \
\
    LogicalArray resultarr (left.shape()); \
    resultarr = False; \
    MaskedLogicalArray result (resultarr, \
                               (left.getMask() && right.getMask())); \
\
    Bool resultarrDelete; \
    LogicalArrayElem *resultarrStorage = \
        result.getRWArrayStorage(resultarrDelete); \
    LogicalArrayElem *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            *resultarrS = (LogicalArrayElem) (*leftarrS OP *rightarrS); \
        } \
        resultarrS++; \
        resultmaskS++; \
        leftarrS++; \
        rightarrS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
\
    return result; \
}


MARRLOGI_MLA_OP_MA ( <=, "<=" )
MARRLOGI_MLA_OP_MA ( <, "<" )
MARRLOGI_MLA_OP_MA ( >=, ">=" )
MARRLOGI_MLA_OP_MA ( >, ">" )
MARRLOGI_MLA_OP_MA ( ==, "==" )
MARRLOGI_MLA_OP_MA ( !=, "!=" )
MARRLOGI_MLA_OP_MA ( &&, "&&" )
MARRLOGI_MLA_OP_MA ( ||, "||" )

MARRLOGI_MLA_OP_AM ( <=, "<=" )
MARRLOGI_MLA_OP_AM ( <, "<" )
MARRLOGI_MLA_OP_AM ( >=, ">=" )
MARRLOGI_MLA_OP_AM ( >, ">" )
MARRLOGI_MLA_OP_AM ( ==, "==" )
MARRLOGI_MLA_OP_AM ( !=, "!=" )
MARRLOGI_MLA_OP_AM ( &&, "&&" )
MARRLOGI_MLA_OP_AM ( ||, "||" )

MARRLOGI_MLA_OP_MM ( <=, "<=" )
MARRLOGI_MLA_OP_MM ( <, "<" )
MARRLOGI_MLA_OP_MM ( >=, ">=" )
MARRLOGI_MLA_OP_MM ( >, ">" )
MARRLOGI_MLA_OP_MM ( ==, "==" )
MARRLOGI_MLA_OP_MM ( !=, "!=" )
MARRLOGI_MLA_OP_MM ( &&, "&&" )
MARRLOGI_MLA_OP_MM ( ||, "||" )


template<class T>
MaskedLogicalArray operator ! (const MaskedArray<T> &marray)
{
    MaskedLogicalArray result (marray.copy());

    Bool resultarrDelete;
    LogicalArrayElem *resultarrStorage =
        result.getRWArrayStorage(resultarrDelete);
    LogicalArrayElem *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage =
        result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    Bool marrayarrDelete;
    const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
    const T *marrayarrS = marrayarrStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
            *resultarrS = ((*marrayarrS) ? False : True);
        }
        resultarrS++;
        resultmaskS++;
        marrayarrS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
    marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);

    return result;
}


#define MARRLOGI_B_ALLFUNC_MS(ALLFUNC,OP,STRALLFUNC) \
template<class T> \
Bool ALLFUNC (const MaskedArray<T> &left, const T &right) \
{ \
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage = \
        left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = True; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*leftmaskS) { \
            foundOne = True; \
            if (! (*leftarrS OP right) ) { \
	        retval = False; \
	        break; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRALLFUNC "(const MaskedArray<T> &, const T)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


#define MARRLOGI_B_ALLFUNC_SM(ALLFUNC,OP,STRALLFUNC) \
template<class T> \
Bool ALLFUNC (const T &left, const MaskedArray<T> &right) \
{ \
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage = \
        right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = right.nelements(); \
    Bool retval = True; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*rightmaskS) { \
            foundOne = True; \
            if (! (left OP *rightarrS) ) { \
                retval = False; \
                break; \
            } \
        } \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRALLFUNC "(const T, const MaskedArray<T> &)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


MARRLOGI_B_ALLFUNC_MS ( allLE,  <=, "allLE" )
MARRLOGI_B_ALLFUNC_SM ( allLE,  <=, "allLE" )
MARRLOGI_B_ALLFUNC_MS ( allLT,  <,  "allLT"  )
MARRLOGI_B_ALLFUNC_SM ( allLT,  <,  "allLT"  )
MARRLOGI_B_ALLFUNC_MS ( allGE,  >=, "allGE" )
MARRLOGI_B_ALLFUNC_SM ( allGE,  >=, "allGE" )
MARRLOGI_B_ALLFUNC_MS ( allGT,  >,  "allGT" )
MARRLOGI_B_ALLFUNC_SM ( allGT,  >,  "allGT" )
MARRLOGI_B_ALLFUNC_MS ( allEQ,  ==, "allEQ" )
MARRLOGI_B_ALLFUNC_SM ( allEQ,  ==, "allEQ" )
MARRLOGI_B_ALLFUNC_MS ( allNE,  !=, "allNE" )
MARRLOGI_B_ALLFUNC_SM ( allNE,  !=, "allNE" )


template<class T>
Bool allAND (const MaskedArray<T> &marray, const T &val)
{
    if (!val) {
        return False;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = True;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (! *marrayarrS) {
	            retval = False;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::allAND(const MaskedArray<T> &, const T)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


template<class T>
Bool allAND (const T &val, const MaskedArray<T> &marray)
{
    if (!val) {
        return False;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = True;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (! *marrayarrS) {
	            retval = False;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::allAND(const T, const MaskedArray<T> &)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


template<class T>
Bool allOR (const MaskedArray<T> &marray, const T &val)
{
    if (val) {
        return True;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = True;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (! *marrayarrS) {
	            retval = False;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::allOR(const MaskedArray<T> &, const T)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


template<class T>
Bool allOR (const T &val, const MaskedArray<T> &marray)
{
    if (val) {
        return True;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = True;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (! *marrayarrS) {
	            retval = False;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::allOR(const T, const MaskedArray<T> &)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


#define MARRLOGI_B_ANYFUNC_MS(ANYFUNC,OP,STRANYFUNC) \
template<class T> \
Bool ANYFUNC (const MaskedArray<T> &left, const T &right) \
{ \
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage = \
        left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    Bool retval = False; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*leftmaskS) { \
            foundOne = True; \
            if (*leftarrS OP right) { \
	        retval = True; \
	        break; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRANYFUNC "(const MaskedArray<T> &, const T)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


#define MARRLOGI_B_ANYFUNC_SM(ANYFUNC,OP,STRANYFUNC) \
template<class T> \
Bool ANYFUNC (const T &left, const MaskedArray<T> &right) \
{ \
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage = \
        right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = right.nelements(); \
    Bool retval = False; \
    Bool foundOne = False; \
    while (ntotal--) { \
        if (*rightmaskS) { \
            foundOne = True; \
            if (left OP *rightarrS) { \
                retval = True; \
                break; \
            } \
        } \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    if (!foundOne) { \
        throw (ArrayError( \
          "Bool ::" STRANYFUNC "(const T, const MaskedArray<T> &)" \
          " - MaskedArray must have at least 1 element")); \
    } \
\
    return retval; \
}


MARRLOGI_B_ANYFUNC_MS ( anyLE,  <=, "anyLE" )
MARRLOGI_B_ANYFUNC_SM ( anyLE,  <=, "anyLE" )
MARRLOGI_B_ANYFUNC_MS ( anyLT,  <,  "anyLT"  )
MARRLOGI_B_ANYFUNC_SM ( anyLT,  <,  "anyLT"  )
MARRLOGI_B_ANYFUNC_MS ( anyGE,  >=, "anyGE" )
MARRLOGI_B_ANYFUNC_SM ( anyGE,  >=, "anyGE" )
MARRLOGI_B_ANYFUNC_MS ( anyGT,  >,  "anyGT" )
MARRLOGI_B_ANYFUNC_SM ( anyGT,  >,  "anyGT" )
MARRLOGI_B_ANYFUNC_MS ( anyEQ,  ==, "anyEQ" )
MARRLOGI_B_ANYFUNC_SM ( anyEQ,  ==, "anyEQ" )
MARRLOGI_B_ANYFUNC_MS ( anyNE,  !=, "anyNE" )
MARRLOGI_B_ANYFUNC_SM ( anyNE,  !=, "anyNE" )


template<class T>
Bool anyAND (const MaskedArray<T> &marray, const T &val)
{
    if (!val) {
        return False;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = False;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (*marrayarrS) {
	            retval = True;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::anyAND(const MaskedArray<T> &, const T)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


template<class T>
Bool anyAND (const T &val, const MaskedArray<T> &marray)
{
    if (!val) {
        return False;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = False;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (*marrayarrS) {
	            retval = True;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::anyAND(const T, const MaskedArray<T> &)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


template<class T>
Bool anyOR (const MaskedArray<T> &marray, const T &val)
{
    if (val) {
        return True;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = False;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (*marrayarrS) {
	            retval = True;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::anyOR(const MaskedArray<T> &, const T)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


template<class T>
Bool anyOR (const T &val, const MaskedArray<T> &marray)
{
    if (val) {
        return True;
    } else {

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        Bool marraymaskDelete;
        const LogicalArrayElem *marraymaskStorage =
            marray.getMaskStorage(marraymaskDelete);
        const LogicalArrayElem *marraymaskS = marraymaskStorage;

        uInt ntotal = marray.nelements();
        Bool retval = False;
        Bool foundOne = False;
        while (ntotal--) {
            if (*marraymaskS) {
                foundOne = True;
                if (*marrayarrS) {
	            retval = True;
	            break;
                }
            }
            marrayarrS++;
            marraymaskS++;
        }

        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        if (!foundOne) {
            throw (ArrayError(
              "Bool ::anyOR(const T, const MaskedArray<T> &)"
              " - MaskedArray must have at least 1 element"));
        }

        return retval;
    }
}


#define MARRLOGI_MLA_OP_MS(OP) \
template<class T> \
MaskedLogicalArray operator OP (const MaskedArray<T> &left, \
                                const T &right) \
{ \
    LogicalArray resultarr (left.shape()); \
    resultarr = False; \
    MaskedLogicalArray result (resultarr, left.getMask()); \
\
    Bool resultarrDelete; \
    LogicalArrayElem *resultarrStorage = \
        result.getRWArrayStorage(resultarrDelete); \
    LogicalArrayElem *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            *resultarrS = (LogicalArrayElem) (*leftarrS OP right); \
        } \
        resultarrS++; \
        resultmaskS++; \
        leftarrS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
\
    return result; \
}


#define MARRLOGI_MLA_OP_SM(OP) \
template<class T> \
MaskedLogicalArray operator OP (const T &left, \
                                const MaskedArray<T> &right) \
{ \
    LogicalArray resultarr (right.shape()); \
    resultarr = False; \
    MaskedLogicalArray result (resultarr, right.getMask()); \
\
    Bool resultarrDelete; \
    LogicalArrayElem *resultarrStorage = \
        result.getRWArrayStorage(resultarrDelete); \
    LogicalArrayElem *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            *resultarrS = (LogicalArrayElem) (left OP *rightarrS); \
        } \
        resultarrS++; \
        resultmaskS++; \
        rightarrS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
\
    return result; \
}


MARRLOGI_MLA_OP_MS ( <= )
MARRLOGI_MLA_OP_SM ( <= )
MARRLOGI_MLA_OP_MS ( < )
MARRLOGI_MLA_OP_SM ( < )
MARRLOGI_MLA_OP_MS ( >= )
MARRLOGI_MLA_OP_SM ( >= )
MARRLOGI_MLA_OP_MS ( > )
MARRLOGI_MLA_OP_SM ( > )
MARRLOGI_MLA_OP_MS ( == )
MARRLOGI_MLA_OP_SM ( == )
MARRLOGI_MLA_OP_MS ( != )
MARRLOGI_MLA_OP_SM ( != )


template<class T>
MaskedLogicalArray operator && (const MaskedArray<T> &marray,
                                const T &val)
{
    LogicalArray resultarr (marray.shape());
    resultarr = False;
    MaskedLogicalArray result (resultarr, marray.getMask());

    if (val) {
        Bool resultarrDelete;
        LogicalArrayElem *resultarrStorage =
            result.getRWArrayStorage(resultarrDelete);
        LogicalArrayElem *resultarrS = resultarrStorage;

        Bool resultmaskDelete;
        const LogicalArrayElem *resultmaskStorage =
            result.getMaskStorage(resultmaskDelete);
        const LogicalArrayElem *resultmaskS = resultmaskStorage;

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        uInt ntotal = result.nelements();
        while (ntotal--) {
            if (*resultmaskS) {
                *resultarrS = ((*marrayarrS) ? True : False);
            }
            resultarrS++;
            resultmaskS++;
            marrayarrS++;
        }

        result.putArrayStorage(resultarrStorage, resultarrDelete);
        result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
    }

    return result;
}


template<class T>
MaskedLogicalArray operator && (const T &val,
                                const MaskedArray<T> &marray)
{
    LogicalArray resultarr (marray.shape());
    resultarr = False;
    MaskedLogicalArray result (resultarr, marray.getMask());

    if (val) {
        Bool resultarrDelete;
        LogicalArrayElem *resultarrStorage =
            result.getRWArrayStorage(resultarrDelete);
        LogicalArrayElem *resultarrS = resultarrStorage;

        Bool resultmaskDelete;
        const LogicalArrayElem *resultmaskStorage =
            result.getMaskStorage(resultmaskDelete);
        const LogicalArrayElem *resultmaskS = resultmaskStorage;

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        uInt ntotal = result.nelements();
        while (ntotal--) {
            if (*resultmaskS) {
                *resultarrS = ((*marrayarrS) ? True : False);
            }
            resultarrS++;
            resultmaskS++;
            marrayarrS++;
        }

        result.putArrayStorage(resultarrStorage, resultarrDelete);
        result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
    }

    return result;
}


template<class T>
MaskedLogicalArray operator || (const MaskedArray<T> &marray,
                                const T &val)
{
    LogicalArray resultarr (marray.shape());
    resultarr = False;
    MaskedLogicalArray result (resultarr, marray.getMask());

    if (val) {
        result = True;
    } else {

        Bool resultarrDelete;
        LogicalArrayElem *resultarrStorage =
            result.getRWArrayStorage(resultarrDelete);
        LogicalArrayElem *resultarrS = resultarrStorage;

        Bool resultmaskDelete;
        const LogicalArrayElem *resultmaskStorage =
            result.getMaskStorage(resultmaskDelete);
        const LogicalArrayElem *resultmaskS = resultmaskStorage;

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        uInt ntotal = result.nelements();
        while (ntotal--) {
            if (*resultmaskS) {
                *resultarrS = ((*marrayarrS) ? True : False);
            }
            resultarrS++;
            resultmaskS++;
            marrayarrS++;
        }

        result.putArrayStorage(resultarrStorage, resultarrDelete);
        result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
    }

    return result;
}


template<class T>
MaskedLogicalArray operator || (const T &val,
                                const MaskedArray<T> &marray)
{
    LogicalArray resultarr (marray.shape());
    resultarr = False;
    MaskedLogicalArray result (resultarr, marray.getMask());

    if (val) {
        result = True;
    } else {

        Bool resultarrDelete;
        LogicalArrayElem *resultarrStorage =
            result.getRWArrayStorage(resultarrDelete);
        LogicalArrayElem *resultarrS = resultarrStorage;

        Bool resultmaskDelete;
        const LogicalArrayElem *resultmaskStorage =
            result.getMaskStorage(resultmaskDelete);
        const LogicalArrayElem *resultmaskS = resultmaskStorage;

        Bool marrayarrDelete;
        const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
        const T *marrayarrS = marrayarrStorage;

        uInt ntotal = result.nelements();
        while (ntotal--) {
            if (*resultmaskS) {
                *resultarrS = ((*marrayarrS) ? True : False);
            }
            resultarrS++;
            resultmaskS++;
            marrayarrS++;
        }

        result.putArrayStorage(resultarrStorage, resultarrDelete);
        result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
    }

    return result;
}



} //# NAMESPACE CASACORE - END


#endif
