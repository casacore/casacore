//# RigidVector2.cc: explicit instantiations for RigidVector
//# Copyright (C) 1996,1999
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

#include <casacore/scimath/Mathematics/RigidVector.h>
#include <casacore/scimath/Mathematics/SquareMatrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

RigidVector<Complex,4> operator*(const SquareMatrix<Complex,4>& m,
				 const RigidVector<Float,4>& v) {
    Complex v0,v1,v2,v3;
    switch (m.type_p) {
        case SquareMatrix<Float,4>::ScalarId: {
	    v0=v.v_p[0]*m.a_p[0][0];
	    v1=v.v_p[1]*m.a_p[0][0];
	    v2=v.v_p[2]*m.a_p[0][0];
	    v3=v.v_p[3]*m.a_p[0][0];
	}
        break;
	case SquareMatrix<Float,4>::Diagonal: {
	    v0=v.v_p[0]*m.a_p[0][0];
	    v1=v.v_p[1]*m.a_p[1][1];
	    v2=v.v_p[2]*m.a_p[2][2];
	    v3=v.v_p[3]*m.a_p[3][3];
	}
        break;
        case SquareMatrix<Float,4>::General: {
	    v0=m.a_p[0][0]*v.v_p[0]+m.a_p[0][1]*v.v_p[1]+m.a_p[0][2]*v.v_p[2]+
		m.a_p[0][3]*v.v_p[3];
	    v1=m.a_p[1][0]*v.v_p[0]+m.a_p[1][1]*v.v_p[1]+m.a_p[1][2]*v.v_p[2]+
		m.a_p[1][3]*v.v_p[3];
	    v2=m.a_p[2][0]*v.v_p[0]+m.a_p[2][1]*v.v_p[1]+m.a_p[2][2]*v.v_p[2]+
		m.a_p[2][3]*v.v_p[3];
	    v3=m.a_p[3][0]*v.v_p[0]+m.a_p[3][1]*v.v_p[1]+m.a_p[3][2]*v.v_p[2]+
		m.a_p[3][3]*v.v_p[3];
	}
    }
    return RigidVector<Complex,4>(v0,v1,v2,v3);
}


} //# NAMESPACE CASACORE - END

