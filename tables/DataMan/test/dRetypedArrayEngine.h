//# dRetypedArrayEngine.h: Example virtual column engines to handle an arbitrary data type
//# Copyright (C) 1995,1996,1999,2004
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

#ifndef TABLES_DRETYPEDARRAYENGINE_H
#define TABLES_DRETYPEDARRAYENGINE_H


//# Includes
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/tables/DataMan/RetypedArraySetGet.h>

#include <casacore/casa/namespace.h>
//# Forward Declarations
namespace casacore {
class TableRecord;
template<class T> class Array;
template<class T> class Vector;
}

// <summary>
// Example virtual column engines to handle an arbitrary data type.
// </summary>

// <use visibility=local>

// <reviewed reviewer="GvD" date="2004/07/09" tests="">

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> RetypedArrayEngine
// </prerequisite>

// <synopsis> 
// This class is an example of a class that can be used as the source
// type in the virtual engine column
// <src>RetypedArrayEngine<SourceType,TargetType></src>.
// The target type is in this case a float (because that is the type
// of the data in this class). So the actual engine to be used is
// <src>RetypedArrayEngine<RetypedArrayExample,float></src>.
// </synopsis> 

class RetypedArrayEx1
{
public:
    RetypedArrayEx1(): x_p(0), y_p(0) {}
    RetypedArrayEx1(float x, float y) : x_p(x), y_p(y) {}
    RetypedArrayEx1(const RetypedArrayEx1& that): x_p(that.x_p), y_p(that.y_p) {}
    static String dataTypeId()
        { return "RetypedArrayEx1"; }
    static IPosition shape()
	{ return IPosition (1,2); }

    static void* newCopyInfo (const TableRecord&, const IPosition&);
    static void deleteCopyInfo (void*);

    // Alas, the CFront compiler complains about unknown size of
    // RetypedArrayEx1 when instantiating Array<RetypedArrayEx1>.
    // Therefore we have to declare it as a void*.
//#    static void set (void* copyInfo, Array<RetypedArrayEx1>& out,
    static void set (void* copyInfo, void* out,
		     const casacore::Array<casacore::Float>& in,
		     const casacore::IPosition& shape);
    static void get (void* copyInfo, casacore::Array<casacore::Float>& out,
		     const void* in,
		     const casacore::IPosition& shape);

    float x() const
	{ return x_p; }
    float y() const
	{ return y_p; }
    int operator== (const RetypedArrayEx1& that) const
	{ return x_p==that.x_p && y_p==that.y_p; }
    int operator< (const RetypedArrayEx1& that) const
	{ return x_p<that.x_p || (x_p==that.x_p && y_p<that.y_p); }
private:
    float x_p;
    float y_p;
};


class RetypedArrayEx2
{
public:
    RetypedArrayEx2(): I_p(0), Q_p(0), U_p(0), V_p(0) {}
    RetypedArrayEx2(DComplex i, DComplex q, DComplex u, DComplex v)
	: I_p(i), Q_p(q), U_p(u), V_p(v) {}
    RetypedArrayEx2(const RetypedArrayEx2& that): I_p(that.I_p), Q_p(that.Q_p),
                                               U_p(that.U_p), V_p(that.V_p) {}
    RetypedArrayEx2& operator= (const RetypedArrayEx2& that)
        { I_p=that.I_p; Q_p=that.Q_p; U_p=that.U_p; V_p=that.V_p;
          return *this; }
    static String dataTypeId()
	{ return "RetypedArrayEx2"; }
    static IPosition shape()
	{ return IPosition (1,4); }
    int operator== (const RetypedArrayEx2& that) const
	{ return I_p==that.I_p && Q_p==that.Q_p && U_p==that.U_p
              && V_p==that.V_p;}

    static void* newCopyInfo (const TableRecord&, const IPosition&);
    static void deleteCopyInfo (void*);

    class CopyInfo {
    public:
        CopyInfo (const TableRecord&, const IPosition& sourceElementShape);
	~CopyInfo();
	void set (void* out, const Array<DComplex>& in,
		  const IPosition& sourceElementShape);
	void get (Array<DComplex>& out, const void* in,
		  const IPosition& sourceElementShape);
    private:
	Vector<Bool>* mask_p;
	uInt nrTrue_p;
    };

    static void set (void* copyInfo, void* out,
		     const Array<DComplex>& in,
		     const IPosition& shape)
	{ ((CopyInfo*)copyInfo)->set (out, in, shape); }

    static void get (void* copyInfo, Array<DComplex>& out,
		     const void* in,
		     const IPosition& shape)
	{ ((CopyInfo*)copyInfo)->get (out, in, shape); }

    void setElem (const DComplex* data, const IPosition& shape,
		  const void* mask);
    void getElem (DComplex* data, const IPosition& shape,
		  const void* mask) const;
private:
    DComplex I_p, Q_p, U_p, V_p;
};


#endif
