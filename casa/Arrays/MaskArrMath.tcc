//# MaskArrMath.cc: Simple mathematics done with MaskedArray's.
//# Copyright (C) 1993,1994,1995,1999,2001
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

#ifndef CASA_MASKARRMATH_TCC
#define CASA_MASKARRMATH_TCC

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


#define MARRM_IOP_MA(IOP,STRIOP) \
template<class T> \
const MaskedArray<T> & operator IOP (const MaskedArray<T> &left, \
                                    const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
            ("::operator" STRIOP "(const MaskedArray<T> &, const Array<T> &)" \
             " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    T *leftarrStorage = left.getRWArrayStorage(leftarrDelete); \
    T *leftarrS = leftarrStorage; \
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
    while (ntotal--) { \
        if (*leftmaskS) { \
	    *leftarrS IOP *rightS; \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightS++; \
    } \
\
    left.putArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeStorage(rightStorage, rightDelete); \
\
    return left; \
}


#define MARRM_IOP_AM(IOP,STRIOP) \
template<class T> \
Array<T> & operator IOP (Array<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
              ("::operator" STRIOP "(Array<T> &, const MaskedArray<T> &)" \
               " - arrays do not conform")); \
    } \
\
    Bool leftDelete; \
    T *leftStorage = left.getStorage(leftDelete); \
    T *leftS = leftStorage; \
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
    while (ntotal--) { \
        if (*rightmaskS) { \
	    *leftS IOP *rightarrS; \
        } \
        leftS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.putStorage(leftStorage, leftDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    return left; \
}


#define MARRM_IOP_MM(IOP,STRIOP) \
template<class T> \
const MaskedArray<T> & operator IOP (const MaskedArray<T> &left, \
                                     const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
   ("::operator" STRIOP "(const MaskedArray<T> &, const MaskedArray<T> &)" \
    " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    T *leftarrStorage = left.getRWArrayStorage(leftarrDelete); \
    T *leftarrS = leftarrStorage; \
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
    while (ntotal--) { \
        if (*leftmaskS && *rightmaskS) { \
	    *leftarrS IOP *rightarrS; \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.putArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    return left; \
}

#define MARRM_IOP_MM2(IOP,STRIOP) \
template<class T,class S> \
const MaskedArray<T> & operator IOP (const MaskedArray<T> &left, \
                                     const MaskedArray<S> &right) \
{ \
    if (left.shape()!=right.shape()) { \
	throw (ArrayConformanceError \
   ("::operator" STRIOP "(const MaskedArray<T> &, const MaskedArray<S> &)" \
    " - arrays do not conform")); \
    } \
\
    Bool leftarrDelete; \
    T *leftarrStorage = left.getRWArrayStorage(leftarrDelete); \
    T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage \
        = left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    Bool rightarrDelete; \
    const S *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const S *rightarrS = rightarrStorage; \
\
    Bool rightmaskDelete; \
    const LogicalArrayElem *rightmaskStorage \
        = right.getMaskStorage(rightmaskDelete); \
    const LogicalArrayElem *rightmaskS = rightmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    while (ntotal--) { \
        if (*leftmaskS && *rightmaskS) { \
	    *leftarrS IOP *rightarrS; \
        } \
        leftarrS++; \
        leftmaskS++; \
        rightarrS++; \
        rightmaskS++; \
    } \
\
    left.putArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
    right.freeArrayStorage(rightarrStorage, rightarrDelete); \
    right.freeMaskStorage(rightmaskStorage, rightmaskDelete); \
\
    return left; \
}


#define MARRM_IOP_MS(IOP) \
template<class T> \
const MaskedArray<T> & operator IOP (const MaskedArray<T> &left, \
                                     const T &right) \
{ \
    Bool leftarrDelete; \
    T *leftarrStorage = left.getRWArrayStorage(leftarrDelete); \
    T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage \
        = left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    uInt ntotal = left.nelements(); \
    while (ntotal--) { \
        if (*leftmaskS) { \
	    *leftarrS IOP right; \
        } \
        leftarrS++; \
        leftmaskS++; \
    } \
\
    left.putArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
\
    return left; \
}


MARRM_IOP_MA ( += , "+=" )
MARRM_IOP_MA ( -= , "-=" )
MARRM_IOP_MA ( *= , "*=" )
MARRM_IOP_MA ( /= , "/=" )

MARRM_IOP_AM ( += , "+=" )
MARRM_IOP_AM ( -= , "-=" )
MARRM_IOP_AM ( *= , "*=" )
MARRM_IOP_AM ( /= , "/=" )

MARRM_IOP_MM ( += , "+=" )
MARRM_IOP_MM ( -= , "-=" )
MARRM_IOP_MM ( *= , "*=" )
MARRM_IOP_MM ( /= , "/=" )
MARRM_IOP_MM2 ( /= , "/=" )

MARRM_IOP_MS ( += )
MARRM_IOP_MS ( -= )
MARRM_IOP_MS ( *= )
MARRM_IOP_MS ( /= )


template<class T> MaskedArray<T> operator+ (const MaskedArray<T> &left)
{
    MaskedArray<T> result (left.copy());
    return result;
}

template<class T> MaskedArray<T> operator- (const MaskedArray<T> &left)
{
    MaskedArray<T> result (left.copy());

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage
        = result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS = -(*resultarrS);
        }
        resultarrS++;
        resultmaskS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);

    return result;
}


#define MARRM_OP_MA(OP,IOP,STROP) \
template<class T> \
MaskedArray<T> operator OP (const MaskedArray<T> &left, \
                            const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
          ("::operator" STROP "(const MaskedArray<T> &, const Array<T> &)" \
           " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result (left.copy()); \
\
    result IOP right; \
\
    return result; \
}


#define MARRM_OP_AM(OP,IOP,STROP) \
template<class T> \
MaskedArray<T> operator OP (const Array<T> &left, \
                            const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
          ("::operator" STROP "(const Array<T> &, const MaskedArray<T> &)" \
           " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result (left.copy(), right.getMask()); \
\
    result IOP right; \
\
    return result; \
}


#define MARRM_OP_MM(OP,IOP,STROP) \
template<class T> \
MaskedArray<T> operator OP (const MaskedArray<T> &left, \
                            const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
        ("::operator" STROP "(const MaskedArray<T> &," \
         " const MaskedArray<T> &)" \
         " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result ( left.getArray().copy(), \
                         (left.getMask() && right.getMask()) ); \
\
    result IOP right; \
\
    return result; \
}


#define MARRM_OP_MS(OP,IOP) \
template<class T> \
MaskedArray<T> operator OP (const MaskedArray<T> &left, const T &right) \
{ \
    MaskedArray<T> result (left.copy()); \
\
    result IOP right; \
\
    return result; \
}


#define MARRM_OP_SM(OP,IOP) \
template<class T> \
MaskedArray<T> operator OP (const T &left, const MaskedArray<T> &right) \
{ \
    Array<T> resultarray (right.shape()); \
    resultarray = left; \
\
    MaskedArray<T> result (resultarray, right.getMask()); \
\
    result IOP right; \
\
    return result; \
}


MARRM_OP_MA ( +, += , "+" )
MARRM_OP_MA ( -, -= , "-" )
MARRM_OP_MA ( *, *= , "*" )
MARRM_OP_MA ( /, /= , "/" )

MARRM_OP_AM ( +, += , "+" )
MARRM_OP_AM ( -, -= , "-" )
MARRM_OP_AM ( *, *= , "*" )
MARRM_OP_AM ( /, /= , "/" )

MARRM_OP_MM ( +, += , "+" )
MARRM_OP_MM ( -, -= , "-" )
MARRM_OP_MM ( *, *= , "*" )
MARRM_OP_MM ( /, /= , "/" )

MARRM_OP_MS ( +, += )
MARRM_OP_MS ( -, -= )
MARRM_OP_MS ( *, *= )
MARRM_OP_MS ( /, /= )

MARRM_OP_SM ( +, += )
MARRM_OP_SM ( -, -= )
MARRM_OP_SM ( *, *= )
MARRM_OP_SM ( /, /= )


template<class T> void indgen(MaskedArray<T> &left,
                              T start, T inc)
{
    Bool leftarrDelete;
    T *leftarrStorage = left.getRWArrayStorage(leftarrDelete);
    T *leftarrS = leftarrStorage;

    Bool leftmaskDelete;
    const LogicalArrayElem *leftmaskStorage
        = left.getMaskStorage(leftmaskDelete);
    const LogicalArrayElem *leftmaskS = leftmaskStorage;

    uInt ntotal = left.nelements();
    T ind = start;
    while (ntotal--) {
        if (*leftmaskS) {
	    *leftarrS = ind;
            ind += inc;
        }
        leftarrS++;
        leftmaskS++;
    }

    left.putArrayStorage(leftarrStorage, leftarrDelete);
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete);
}

template<class T> void indgen(MaskedArray<T> &a)
{
    indgen(a, T(0), T(1));
}

template<class T> void indgen(MaskedArray<T> &a, T start)
{
    indgen(a, start, T(1));
}


template<class T, class U>
MaskedArray<T> pow (const MaskedArray<T> &left, const Array<U> &right)
{
//    if (conform2(left, right) == False) {
    if (left.shape() != right.shape()) {
	throw (ArrayConformanceError
               ("::" "pow"
                "(const MaskedArray<T> &, const Array<T> &)"
                " - arrays do not conform"));
    }

    MaskedArray<T> result (left.copy());

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage =
        result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    Bool rightDelete;
    const U *rightStorage = right.getStorage(rightDelete);
    const U *rightS = rightStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS = pow (*resultarrS, *rightS);
        }
        resultarrS++;
        resultmaskS++;
        rightS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
    right.freeStorage(rightStorage, rightDelete);

    return result;
}


template<class T, class U>
MaskedArray<T> pow (const Array<T> &left, const MaskedArray<U> &right)
{
//    if (conform2(left, right) == False) {
    if (left.shape() != right.shape()) {
	throw (ArrayConformanceError
               ("::" "pow"
                "(const Array<T> &, const MaskedArray<U> &)"
                " - arrays do not conform"));
    }

    MaskedArray<T> result (left.copy(), right.getMask());

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage =
        result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    Bool rightarrDelete;
    const U *rightarrStorage = right.getArrayStorage(rightarrDelete);
    const U *rightarrS = rightarrStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS = pow (*resultarrS, *rightarrS);
        }
        resultarrS++;
        resultmaskS++;
        rightarrS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
    right.freeArrayStorage(rightarrStorage, rightarrDelete);

    return result;
}


template<class T, class U>
MaskedArray<T> pow (const MaskedArray<T> &left, const MaskedArray<U> &right)
{
//    if (conform2(left, right) == False) {
    if (left.shape() != right.shape()) {
	throw (ArrayConformanceError
               ("::" "pow"
                "(const MaskedArray<T> &, const MaskedArray<T> &)"
                " - arrays do not conform"));
    }

    MaskedArray<T> result ( left.getArray().copy(),
                         (left.getMask() && right.getMask()) );

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage
        = result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    Bool rightarrDelete;
    const U *rightarrStorage = right.getArrayStorage(rightarrDelete);
    const U *rightarrS = rightarrStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS = pow (*resultarrS, *rightarrS);
        }
        resultarrS++;
        resultmaskS++;
        rightarrS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);
    right.freeArrayStorage(rightarrStorage, rightarrDelete);

    return result;
}


template<class T>
MaskedArray<T> pow (const MaskedArray<T> &left, const Double &right)
{
    MaskedArray<T> result (left.copy());

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage
        = result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS = pow (*resultarrS, right);
        }
        resultarrS++;
        resultmaskS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);

    return result;
}


#define MARRM_FUNC_M(FUNC,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left) \
{ \
    MaskedArray<T> result (left.copy()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
	    *resultarrS = FUNC (*resultarrS); \
        } \
        resultarrS++; \
        resultmaskS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
\
    return result; \
}


#define MARRM_FUNC_MA(FUNC,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left, const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
               ("::" STRFUNC \
                "(const MaskedArray<T> &, const Array<T> &)" \
                " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result (left.copy()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightDelete; \
    const T *rightStorage = right.getStorage(rightDelete); \
    const T *rightS = rightStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
	    *resultarrS = T(FUNC (*resultarrS, *rightS)); \
        } \
        resultarrS++; \
        resultmaskS++; \
        rightS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    right.freeStorage(rightStorage, rightDelete); \
\
    return result; \
}


#define MARRM_FUNC_AM(FUNC,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const Array<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
               ("::" STRFUNC \
                "(const Array<T> &, const MaskedArray<T> &)" \
                " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result (left.copy(), right.getMask()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
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
	    *resultarrS = T(FUNC (*resultarrS, *rightarrS)); \
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


#define MARRM_FUNC_MM(FUNC,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left, \
                     const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
               ("::" STRFUNC\
                "(const MaskedArray<T> &, const MaskedArray<T> &)" \
                " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result ( left.getArray().copy(), \
                         (left.getMask() && right.getMask()) ); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
	    *resultarrS = T(FUNC (*resultarrS, *rightarrS)); \
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


#define MARRM_FUNC_MS(FUNC) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left, const T &right) \
{ \
    MaskedArray<T> result (left.copy()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
	    *resultarrS = FUNC (*resultarrS, right); \
        } \
        resultarrS++; \
        resultmaskS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
\
    return result; \
}


#define MARRM_FUNC_SM(FUNC) \
template<class T> \
MaskedArray<T> FUNC (const T &left, const MaskedArray<T> &right) \
{ \
    Array<T> resultarray (right.shape()); \
    resultarray = left; \
\
    MaskedArray<T> result (resultarray, right.getMask()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
	    *resultarrS = FUNC (*resultarrS, *rightarrS); \
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


MARRM_FUNC_M ( sin, "sin" )
MARRM_FUNC_M ( cos, "cos" )
MARRM_FUNC_M ( tan, "tan" )
MARRM_FUNC_M ( asin, "asin" )
MARRM_FUNC_M ( acos, "acos" )
MARRM_FUNC_M ( atan, "atan" )
MARRM_FUNC_M ( sinh, "sinh" )
MARRM_FUNC_M ( cosh, "cosh" )
MARRM_FUNC_M ( tanh, "tanh" )
MARRM_FUNC_M ( exp, "exp" )
MARRM_FUNC_M ( log, "log" )
MARRM_FUNC_M ( log10, "log10" )
MARRM_FUNC_M ( sqrt, "sqrt" )
MARRM_FUNC_M ( abs, "abs" )
MARRM_FUNC_M ( fabs, "fabs" )
MARRM_FUNC_M ( ceil, "ceil" )
MARRM_FUNC_M ( floor, "floor" )

MARRM_FUNC_MA ( atan2, "atan2" )
MARRM_FUNC_MA ( fmod, "fmod" )

MARRM_FUNC_AM ( atan2, "atan2" )
MARRM_FUNC_AM ( fmod, "fmod" )

MARRM_FUNC_MM ( atan2, "atan2" )
MARRM_FUNC_MM ( fmod, "fmod" )

MARRM_FUNC_MS ( atan2 )
MARRM_FUNC_MS ( fmod )

MARRM_FUNC_SM ( atan2 )
MARRM_FUNC_SM ( fmod )


template<class T>
void minMax(T &minVal, T &maxVal, IPosition &minPos, IPosition &maxPos,
            const MaskedArray<T> &marray)
{
    if ((minPos.nelements() != marray.ndim()) ||
        (maxPos.nelements() != marray.ndim())) {
        throw (ArrayError(
            "void ::minMax("
            "T &minVal, T &maxVal, IPosition &minPos, IPosition &maxPos,"
            " const MaskedArray<T> &marray)"
            " - minPos, maxPos dimensionality inconsistent with marray"));
    }

    Bool marrayarrDelete;
    const T *marrayarrStorage = marray.getArrayStorage(marrayarrDelete);
    const T *marrayarrS = marrayarrStorage;

    Bool marraymaskDelete;
    const LogicalArrayElem *marraymaskStorage
        = marray.getMaskStorage(marraymaskDelete);
    const LogicalArrayElem *marraymaskS = marraymaskStorage;

    uInt ntotal = marray.nelements();
    Bool foundOne = False;
    T minLocal = T();
    T maxLocal = T();
    uInt minNtotal=0;
    uInt maxNtotal=0;

    while (ntotal--) {
        if (*marraymaskS) {
            minLocal = *marrayarrS;
            maxLocal = minLocal;
            minNtotal = ntotal + 1;
            maxNtotal = minNtotal;
            marrayarrS++;
            marraymaskS++;
            foundOne = True;
            break;
        } else {
            marrayarrS++;
            marraymaskS++;
        }
    }

    if (!foundOne) {
        marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
        marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

        throw (ArrayError(
            "void ::minMax("
            "T &minVal, T &maxVal, IPosition &minPos, IPosition &maxPos,"
            " const MaskedArray<T> &marray)"
                         " - MaskedArray must have at least 1 element"));
    }

    while (ntotal--) {
        if (*marraymaskS) {
            if (*marrayarrS < minLocal) { \
                minLocal = *marrayarrS;
                minNtotal = ntotal + 1;
            }
            if (*marrayarrS > maxLocal) { \
                maxLocal = *marrayarrS;
                maxNtotal = ntotal + 1;
            }
        }
        marrayarrS++;
        marraymaskS++;
    }

    marray.freeArrayStorage(marrayarrStorage, marrayarrDelete);
    marray.freeMaskStorage(marraymaskStorage, marraymaskDelete);

    minVal = minLocal;
    maxVal = maxLocal;

    minPos = toIPositionInArray (marray.nelements() - minNtotal,
				 marray.shape());
    maxPos = toIPositionInArray (marray.nelements() - maxNtotal,
				 marray.shape());

    return;
}


template<class T>
void minMax(T &minVal, T &maxVal,
            const MaskedArray<T> &marray)
{
    IPosition minPos (marray.ndim(), 0);
    IPosition maxPos (minPos);

    minMax (minVal, maxVal, minPos, maxPos, marray);

    return;
}


#define MARRM_MINORMAX_M(FUNC,OP,STRFUNC) \
template<class T> T FUNC (const MaskedArray<T> &left) \
{ \
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool leftmaskDelete; \
    const LogicalArrayElem *leftmaskStorage \
        = left.getMaskStorage(leftmaskDelete); \
    const LogicalArrayElem *leftmaskS = leftmaskStorage; \
\
    T result = *leftarrS; \
    uInt ntotal = left.nelements(); \
    Bool foundOne = False; \
\
    while (ntotal--) { \
        if (*leftmaskS) { \
            result = *leftarrS; \
            leftarrS++; \
            leftmaskS++; \
            foundOne = True; \
            break; \
        } else { \
            leftarrS++; \
            leftmaskS++; \
        } \
    } \
\
    if (!foundOne) { \
        left.freeArrayStorage(leftarrStorage, leftarrDelete); \
        left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
\
        throw (ArrayError("T ::" STRFUNC "(const MaskedArray<T> &left) - " \
                         "MaskedArray must have at least 1 element")); \
    } \
\
    while (ntotal--) { \
        if (*leftmaskS) { \
            if (*leftarrS OP result) { \
                result = *leftarrS; \
            } \
        } \
        leftarrS++; \
        leftmaskS++; \
    } \
\
    left.freeArrayStorage(leftarrStorage, leftarrDelete); \
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete); \
\
    return result; \
}


#define MARRM_MINORMAX_MA(FUNC,OP,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left, const Array<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
               ("::" STRFUNC \
                "(const MaskedArray<T> &, const Array<T> &)" \
                " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result (left.copy()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightDelete; \
    const T *rightStorage = right.getStorage(rightDelete); \
    const T *rightS = rightStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            if (*rightS OP *resultarrS) { \
                *resultarrS = *rightS; \
            } \
        } \
        resultarrS++; \
        resultmaskS++; \
        rightS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    right.freeStorage(rightStorage, rightDelete); \
\
    return result; \
}


#define MARRM_MINORMAX_AM(FUNC,OP,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const Array<T> &left, const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
               ("::" STRFUNC \
                "(const Array<T> &, const MaskedArray<T> &)" \
                " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result (left.copy(), right.getMask()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
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
            if (*rightarrS OP *resultarrS) { \
                *resultarrS = *rightarrS; \
            } \
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


#define MARRM_MINORMAX_MM(FUNC,OP,STRFUNC) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left, \
                     const MaskedArray<T> &right) \
{ \
    if (left.conform(right) == False) { \
	throw (ArrayConformanceError \
               ("MaskedArray<T> ::" STRFUNC\
                "(const MaskedArray<T> &, const MaskedArray<T> &)" \
                " - arrays do not conform")); \
    } \
\
    MaskedArray<T> result ( left.getArray().copy(), \
                         (left.getMask() && right.getMask()) ); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            if (*rightarrS OP *resultarrS) { \
                *resultarrS = *rightarrS; \
            } \
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


#define MARRM_MINORMAX_MS(FUNC,OP) \
template<class T> \
MaskedArray<T> FUNC (const MaskedArray<T> &left, const T &right) \
{ \
    MaskedArray<T> result (left.copy()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            if (right OP *resultarrS) { \
                *resultarrS = right; \
            } \
        } \
        resultarrS++; \
        resultmaskS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
\
    return result; \
}


#define MARRM_MINORMAX_SM(FUNC,OP) \
template<class T> \
MaskedArray<T> FUNC (const T &left, const MaskedArray<T> &right) \
{ \
    Array<T> resultarray (right.shape()); \
    resultarray = left; \
\
    MaskedArray<T> result (resultarray, right.getMask()); \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage \
        = result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getArrayStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            if (*rightarrS OP *resultarrS) { \
                *resultarrS = *rightarrS; \
            } \
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


#define MARRM_MINORMAX_AAM(FUNC,OP,STRFUNC) \
template<class T> \
void FUNC (const MaskedArray<T> &result, \
           const Array<T> &left, const Array<T> &right) \
{ \
    if ( ! (result.conform(left) && result.conform(right)) ) { \
	throw (ArrayConformanceError \
            ("void ::" STRFUNC \
             "(const MaskedArray<T> &, const Array<T> &, const Array<T> &)" \
             " - arrays do not conform")); \
    } \
\
    Bool resultarrDelete; \
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete); \
    T *resultarrS = resultarrStorage; \
\
    Bool resultmaskDelete; \
    const LogicalArrayElem *resultmaskStorage = \
        result.getMaskStorage(resultmaskDelete); \
    const LogicalArrayElem *resultmaskS = resultmaskStorage; \
\
    Bool leftarrDelete; \
    const T *leftarrStorage = left.getStorage(leftarrDelete); \
    const T *leftarrS = leftarrStorage; \
\
    Bool rightarrDelete; \
    const T *rightarrStorage = right.getStorage(rightarrDelete); \
    const T *rightarrS = rightarrStorage; \
\
    uInt ntotal = result.nelements(); \
    while (ntotal--) { \
        if (*resultmaskS) { \
            *resultarrS = (*leftarrS OP *rightarrS) ? *leftarrS : *rightarrS; \
        } \
        resultarrS++; \
        resultmaskS++; \
        leftarrS++; \
        rightarrS++; \
    } \
\
    result.putArrayStorage(resultarrStorage, resultarrDelete); \
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete); \
    left.freeStorage(leftarrStorage, leftarrDelete); \
    right.freeStorage(rightarrStorage, rightarrDelete); \
\
    return; \
}


MARRM_MINORMAX_M ( min, <, "min" )
MARRM_MINORMAX_M ( max, >, "max" )

MARRM_MINORMAX_MA ( min, < , "min" )
MARRM_MINORMAX_MA ( max, > , "max" )

MARRM_MINORMAX_AM ( min, < , "min" )
MARRM_MINORMAX_AM ( max, > , "max" )

MARRM_MINORMAX_MM ( min, < , "min" )
MARRM_MINORMAX_MM ( max, > , "max" )

MARRM_MINORMAX_MS ( min, < )
MARRM_MINORMAX_MS ( max, > )

MARRM_MINORMAX_SM ( min, < )
MARRM_MINORMAX_SM ( max, > )

MARRM_MINORMAX_AAM ( min, < , "min" )
MARRM_MINORMAX_AAM ( max, > , "max" )


template<class T> T sum(const MaskedArray<T> &left)
{
    if (left.nelementsValid() < 1) {
        throw (ArrayError("T ::sum(const MaskedArray<T> &left) - "
                         "MaskedArray must have at least 1 element"));
    }

    Bool leftarrDelete;
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete);
    const T *leftarrS = leftarrStorage;

    Bool leftmaskDelete;
    const LogicalArrayElem *leftmaskStorage
        = left.getMaskStorage(leftmaskDelete);
    const LogicalArrayElem *leftmaskS = leftmaskStorage;

    T sum = 0;
    uInt ntotal = left.nelements();
    while (ntotal--) {
        if (*leftmaskS) {
            sum += *leftarrS;
        }
        leftarrS++;
        leftmaskS++;
    }

    left.freeArrayStorage(leftarrStorage, leftarrDelete);
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete);

    return sum;
}

template<class T> T sumsquares(const MaskedArray<T> &left)
{
    if (left.nelementsValid() < 1) {
        throw (ArrayError("T ::sumsquares(const MaskedArray<T> &left) - "
                         "MaskedArray must have at least 1 element"));
    }

    Bool leftarrDelete;
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete);
    const T *leftarrS = leftarrStorage;

    Bool leftmaskDelete;
    const LogicalArrayElem *leftmaskStorage
        = left.getMaskStorage(leftmaskDelete);
    const LogicalArrayElem *leftmaskS = leftmaskStorage;

    T sumsquares = 0;
    uInt ntotal = left.nelements();
    while (ntotal--) {
        if (*leftmaskS) {
            sumsquares += (*leftarrS * *leftarrS);
        }
        leftarrS++;
        leftmaskS++;
    }

    left.freeArrayStorage(leftarrStorage, leftarrDelete);
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete);

    return sumsquares;
}

template<class T> T product(const MaskedArray<T> &left)
{
    if (left.nelementsValid() < 1) {
        throw (ArrayError("T ::product(const MaskedArray<T> &left) - "
                          "MaskedArray must have at least 1 element"));
    }

    Bool leftarrDelete;
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete);
    const T *leftarrS = leftarrStorage;

    Bool leftmaskDelete;
    const LogicalArrayElem *leftmaskStorage
        = left.getMaskStorage(leftmaskDelete);
    const LogicalArrayElem *leftmaskS = leftmaskStorage;

    T product = 1;
    uInt ntotal = left.nelements();
    while (ntotal--) {
        if (*leftmaskS) {
            product *= *leftarrS;
        }
        leftarrS++;
        leftmaskS++;
    }

    left.freeArrayStorage(leftarrStorage, leftarrDelete);
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete);

    return product;
}

template<class T> T mean(const MaskedArray<T> &left)
{
    if (left.nelementsValid() < 1) {
        throw (ArrayError("T ::mean(const MaskedArray<T> &left) - "
                          "MaskedArray must have at least 1 element"));
    }
    return sum(left)/left.nelementsValid();
}

template<class T> T variance(const MaskedArray<T> &left, T mean)
{
    if (left.nelementsValid() < 2) {
        throw (ArrayError("T ::variance(const MaskedArray<T> &, T) - "
                          "MaskedArray must have at least 2 elements"));
    }
    MaskedArray<T> deviations (left - mean);
    deviations *= deviations;
    return sum(deviations)/(left.nelementsValid() - 1);
}

template<class T> T variance(const MaskedArray<T> &left)
{
    return variance(left, mean(left));
}

template<class T> T stddev(const MaskedArray<T> &left)
{
    return sqrt(variance(left));
}

template<class T> T stddev(const MaskedArray<T> &left, T mean)
{
    return sqrt(variance(left, mean));
}

template<class T> T avdev(const MaskedArray<T> &left)
{
    return avdev(left, mean(left));
}

template<class T> T avdev(const MaskedArray<T> &left, T mean)
{
    if (left.nelementsValid() < 1) {
        throw (ArrayError("T ::avdev(const MaskedArray<T> &, T) - "
                          "MaskedArray must have at least 1 element"));
    }
    MaskedArray<T> avdeviations (abs(left - mean));
    return sum(avdeviations)/left.nelementsValid();
}

template<class T> T rms(const MaskedArray<T> &left)
{
    if (left.nelementsValid() < 1) {
        throw (ArrayError("T ::rms(const MaskedArray<T> &left) - "
                          "MaskedArray must have at least 1 element"));
    }
    return T(sqrt(sumsquares(left)/(1.0*left.nelementsValid())));
}

template<class T> T median(const MaskedArray<T> &left, Bool sorted,
			   Bool takeEvenMean)
{
    uInt nelem = left.nelementsValid();
    if (nelem < 1) {
        throw (ArrayError("T ::median(const MaskedArray<T> &) - "
                          "MaskedArray must have at least 1 element"));
    }
    //# Mean does not have to be taken for odd number of elements.
    if (nelem%2 != 0) {
	takeEvenMean = False;
    }
    T medval;

    Bool leftarrDelete;
    const T *leftarrStorage = left.getArrayStorage(leftarrDelete);
    const T *leftarrS = leftarrStorage;

    Bool leftmaskDelete;
    const LogicalArrayElem *leftmaskStorage
        = left.getMaskStorage(leftmaskDelete);
    const LogicalArrayElem *leftmaskS = leftmaskStorage;

    uInt n2 = (nelem - 1)/2;

    if (! sorted) {
	// Make a copy of the masked elements.

	T *copy = new T[nelem];
        T *copyS = copy;

        uInt ntotal = nelem;
        while (ntotal) {
            if (*leftmaskS) {
                *copyS = *leftarrS;
                copyS++;
                ntotal--;
            }
            leftarrS++;
            leftmaskS++;
        }

	// Use a faster algorithm when the array is big enough.
	// If needed take the mean for an even number of elements.
	// Sort a small array in ascending order.
	if (nelem > 50) {
	    if (takeEvenMean) {
		medval = T(0.5 * (GenSort<T>::kthLargest (copy, nelem, n2) +
				  GenSort<T>::kthLargest (copy, nelem, n2+1)));
	    } else {
		medval = GenSort<T>::kthLargest (copy, nelem, n2);
	    }
	} else {
	    GenSort<T>::sort (copy, nelem);
	    if (takeEvenMean) {
		medval = T(0.5 * (copy[n2] + copy[n2+1]));
	    } else {
		medval = copy[n2];
	    }
	}
	delete [] copy;

    } else {
        // Sorted.
	// When mean has to be taken, we need one more element.
        if (takeEvenMean) {
	    n2++;
	}
	const T* prev = 0;
	for (;;) {
	    if (*leftmaskS) {
		if (n2 == 0) break;
		prev = leftarrS;
		n2--;
	    }
	    leftarrS++;
	    leftmaskS++;
	}
        if (takeEvenMean) {
            medval = T(0.5 * (*prev + *leftarrS));
        } else {
            medval = *leftarrS;
        }
    }

    left.freeArrayStorage(leftarrStorage, leftarrDelete);
    left.freeMaskStorage(leftmaskStorage, leftmaskDelete);

    return medval;
}

template<class T> T madfm(const MaskedArray<T> &a, Bool sorted,
                          Bool takeEvenMean)
{
    T med = median(a, sorted, takeEvenMean);
    MaskedArray<T> absdiff = abs(a - med);
    return median(absdiff, False, takeEvenMean);
}


template<class T> MaskedArray<T> square(const MaskedArray<T> &left)
{
    MaskedArray<T> result (left.copy());

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage
        = result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS *= *resultarrS;
        }
        resultarrS++;
        resultmaskS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);

    return result;
}



template<class T> MaskedArray<T> cube(const MaskedArray<T> &left)
{
    MaskedArray<T> result (left.copy());

    Bool resultarrDelete;
    T *resultarrStorage = result.getRWArrayStorage(resultarrDelete);
    T *resultarrS = resultarrStorage;

    Bool resultmaskDelete;
    const LogicalArrayElem *resultmaskStorage
        = result.getMaskStorage(resultmaskDelete);
    const LogicalArrayElem *resultmaskS = resultmaskStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        if (*resultmaskS) {
	    *resultarrS *= (*resultarrS * *resultarrS);
        }
        resultarrS++;
        resultmaskS++;
    }

    result.putArrayStorage(resultarrStorage, resultarrDelete);
    result.freeMaskStorage(resultmaskStorage, resultmaskDelete);

    return result;
}


template <typename T, typename FuncType>
MaskedArray<T> boxedArrayMath (const MaskedArray<T>& array,
			       const IPosition& boxSize,
			       const FuncType& funcObj)
{
  uInt ndim = array.ndim();
  const IPosition& shape = array.shape();
  // Set missing axes to 1.
  IPosition boxsz (boxSize);
  if (boxsz.size() != ndim) {
    uInt sz = boxsz.size();
    boxsz.resize (ndim);
    for (uInt i=sz; i<ndim; ++i) {
      boxsz[i] = 1;
    }
  }
  // Determine the output shape.
  IPosition resShape(ndim);
  for (uInt i=0; i<ndim; ++i) {
    // Set unspecified axes to full length.
    if (boxsz[i] <= 0  ||  boxsz[i] > shape[i]) {
      boxsz[i] = shape[i];
    }
    resShape[i] = (shape[i] + boxsz[i] - 1) / boxsz[i];
  }
  // Need to make shallow copy because operator() is non-const.
  MaskedArray<T> arr (array);
  Array<T> result (resShape);
  Array<Bool> resultMask(resShape);
  T* res = result.data();
  Bool* resMask = resultMask.data();
  // Loop through all data and assemble as needed.
  IPosition blc(ndim, 0);
  IPosition trc(boxsz-1);
  while (True) {
    MaskedArray<T> subarr (arr(blc,trc));
    if (subarr.nelementsValid() == 0) {
      *resMask++ = False;
      *res++ = T();
    } else {
      *resMask++ = True;
      *res++ = funcObj (arr(blc,trc));
    }
    uInt ax;
    for (ax=0; ax<ndim; ++ax) {
      blc[ax] += boxsz[ax];
      if (blc[ax] < shape[ax]) {
	trc[ax] += boxsz[ax];
	if (trc[ax] >= shape[ax]) {
	  trc[ax] = shape[ax]-1;
	}
	break;
      }
      blc[ax] = 0;
      trc[ax] = boxsz[ax]-1;
    }
    if (ax == ndim) {
      break;
    }
  }
  return MaskedArray<T> (result, resultMask);
}

template <typename T, typename FuncType>
Array<T> slidingArrayMath (const MaskedArray<T>& array,
			   const IPosition& halfBoxSize,
			   const FuncType& funcObj,
			   Bool fillEdge)
{
  uInt ndim = array.ndim();
  const IPosition& shape = array.shape();
  // Set full box size (-1) and resize/fill as needed.
  IPosition hboxsz (2*halfBoxSize);
  if (hboxsz.size() != array.ndim()) {
    uInt sz = hboxsz.size();
    hboxsz.resize (array.ndim());
    for (uInt i=sz; i<hboxsz.size(); ++i) {
      hboxsz[i] = 0;
    }
  }
  // Determine the output shape. See if anything has to be done.
  IPosition resShape(ndim);
  for (uInt i=0; i<ndim; ++i) {
    resShape[i] = shape[i] - hboxsz[i];
    if (resShape[i] <= 0) {
      if (!fillEdge) {
	return Array<T>();
      }
      Array<T> res(shape);
      res = T();
      return res;
    }
  }
  // Need to make shallow copy because operator() is non-const.
  MaskedArray<T> arr (array);
  Array<T> result (resShape);
  DebugAssert (result.contiguousStorage(), AipsError);
  T* res = result.data();
  // Loop through all data and assemble as needed.
  IPosition blc(ndim, 0);
  IPosition trc(hboxsz);
  IPosition pos(ndim, 0);
  while (True) {
    *res++ = funcObj (arr(blc,trc));
    uInt ax;
    for (ax=0; ax<ndim; ax++) {
      if (++pos[ax] < resShape[ax]) {
	blc[ax]++;
	trc[ax]++;
	break;
      }
      pos(ax) = 0;
      blc[ax] = 0;
      trc[ax] = hboxsz[ax];
    }
    if (ax == ndim) {
      break;
    }
  }
  if (!fillEdge) {
    return result;
  }
  Array<T> fullResult(shape);
  fullResult = T();
  hboxsz /= 2;
  fullResult(hboxsz, resShape+hboxsz-1) = result;
  return fullResult;
}


} //# NAMESPACE CASACORE - END


#endif
