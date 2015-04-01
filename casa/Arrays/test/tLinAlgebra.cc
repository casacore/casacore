//# tLinAlgebra.cc: This program tests the linear algebra routines
//# Copyright (C) 1994,1995,1996,1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/math.h>

#include <casacore/casa/namespace.h>

int main()
{
  try{
    Vector<int> foo_int1(3);
    foo_int1(0)=1;
    foo_int1(1)=2;
    foo_int1(2)=3;
    Vector<int> foo_int2(3);
    foo_int2(0)=3;
    foo_int2(1)=2;
    foo_int2(2)=1;
    
    AlwaysAssertExit(10==innerProduct(foo_int1,foo_int2));
    
    AlwaysAssertExit(3==norm(foo_int1));
    
    Vector<int> foo_int3(3);
    foo_int3(0)=-4;
    foo_int3(1)=8;
    foo_int3(2)=-4;
    
    AlwaysAssertExit(allEQ(foo_int3,crossProduct(foo_int1,foo_int2)));
    
    Matrix<int> Foo_int(3u,3u);
    
    Foo_int(0,0)=3;
    Foo_int(1,0)=6;
    Foo_int(2,0)=9;
    Foo_int(0,1)=2;
    Foo_int(1,1)=4;
    Foo_int(2,1)=6;
    Foo_int(0,2)=1;
    Foo_int(1,2)=2;
    Foo_int(2,2)=3;

    AlwaysAssertExit(18==normI(Foo_int));
    AlwaysAssertExit(18==norm1(Foo_int));

    Matrix<int> Foo_int_T(3u,3u);

    Foo_int_T(0,0)=3;
    Foo_int_T(1,0)=2;
    Foo_int_T(2,0)=1;
    Foo_int_T(0,1)=6;
    Foo_int_T(1,1)=4;
    Foo_int_T(2,1)=2;
    Foo_int_T(0,2)=9;
    Foo_int_T(1,2)=6;
    Foo_int_T(2,2)=3;

    AlwaysAssertExit(allEQ(Foo_int,transpose(Foo_int_T)));
    
    Matrix<int> itemp(1u,3u);
    itemp.row(0)=foo_int2;
    AlwaysAssertExit(allEQ(Foo_int,product(foo_int1,itemp)));
    
    foo_int3(0)=10;
    foo_int3(1)=20;
    foo_int3(2)=30;
    
    AlwaysAssertExit(allEQ(foo_int3,product(Foo_int, foo_int1)));
    
    Vector<float> foo_float1(3);
    foo_float1(0)=1.0;
    foo_float1(1)=2.0;
    foo_float1(2)=3.0;
    Vector<float> foo_float2(3);
    foo_float2(0)=3.0;
    foo_float2(1)=2.0;
    foo_float2(2)=1.0;
    
    AlwaysAssertExit(10.0==innerProduct(foo_float1,foo_float2));
    
    AlwaysAssertExit(sqrt(13.999)<norm(foo_float1));
    AlwaysAssertExit(sqrt(14.001)>norm(foo_float1));
    
    Vector<float> foo_float3(3);
    foo_float3(0)=-4.0;
    foo_float3(1)=8.0;
    foo_float3(2)=-4.0;
    
    AlwaysAssertExit(allEQ(foo_float3,
			   crossProduct(foo_float1,foo_float2)));
    
    Matrix<float> Foo_float(3u,3u);
    
    Foo_float(0,0)=3.0;
    Foo_float(1,0)=6.0;
    Foo_float(2,0)=9.0;
    Foo_float(0,1)=2.0;
    Foo_float(1,1)=4.0;
    Foo_float(2,1)=6.0;
    Foo_float(0,2)=1.0;
    Foo_float(1,2)=2.0;
    Foo_float(2,2)=3.0;

    AlwaysAssertExit(18==normI(Foo_float));
    AlwaysAssertExit(18==norm1(Foo_float));

    Matrix<float> Foo_float_T(3u,3u);

    Foo_float_T(0,0)=3.0;
    Foo_float_T(1,0)=2.0;
    Foo_float_T(2,0)=1.0;
    Foo_float_T(0,1)=6.0;
    Foo_float_T(1,1)=4.0;
    Foo_float_T(2,1)=2.0;
    Foo_float_T(0,2)=9.0;
    Foo_float_T(1,2)=6.0;
    Foo_float_T(2,2)=3.0;

    AlwaysAssertExit(allEQ(Foo_float,transpose(Foo_float_T)));

    Matrix<float> ftemp(1u,3u);
    ftemp.row(0)=foo_float2;
    AlwaysAssertExit(allEQ(Foo_float,product(foo_float1,ftemp)));
    
    foo_float3(0)=10.0;
    foo_float3(1)=20.0;
    foo_float3(2)=30.0;
    
    AlwaysAssertExit(allEQ(foo_float3,product(Foo_float, foo_float1)));
    
    Vector<double> foo_double1(3);
    foo_double1(0)=1.0;
    foo_double1(1)=2.0;
    foo_double1(2)=3.0;
    Vector<double> foo_double2(3);
    foo_double2(0)=3.0;
    foo_double2(1)=2.0;
    foo_double2(2)=1.0;
    
    AlwaysAssertExit(10.0==innerProduct(foo_double1,foo_double2));
    
    AlwaysAssertExit(sqrt(13.99999999)<norm(foo_double1));
    AlwaysAssertExit(sqrt(14.00000001)>norm(foo_double1));
    
    Vector<double> foo_double3(3);
    foo_double3(0)=-4.0;
    foo_double3(1)=8.0;
    foo_double3(2)=-4.0;
    
    AlwaysAssertExit(allEQ(foo_double3,
			   crossProduct(foo_double1,foo_double2)));
    
    Matrix<double> Foo_double(3u,3u);
    
    Foo_double(0,0)=3.0;
    Foo_double(1,0)=6.0;
    Foo_double(2,0)=9.0;
    Foo_double(0,1)=2.0;
    Foo_double(1,1)=4.0;
    Foo_double(2,1)=6.0;
    Foo_double(0,2)=1.0;
    Foo_double(1,2)=2.0;
    Foo_double(2,2)=3.0;
    
    AlwaysAssertExit(18.0==normI(Foo_double));
    AlwaysAssertExit(18.0==norm1(Foo_double));

    Matrix<double> Foo_double_T(3u,3u);

    Foo_double_T(0,0)=3.0;
    Foo_double_T(1,0)=2.0;
    Foo_double_T(2,0)=1.0;
    Foo_double_T(0,1)=6.0;
    Foo_double_T(1,1)=4.0;
    Foo_double_T(2,1)=2.0;
    Foo_double_T(0,2)=9.0;
    Foo_double_T(1,2)=6.0;
    Foo_double_T(2,2)=3.0;

    AlwaysAssertExit(allEQ(Foo_double,transpose(Foo_double_T)));

    Matrix<double> dtemp(1u,3u);
    dtemp.row(0)=foo_double2;
    AlwaysAssertExit(allEQ(Foo_double,product(foo_double1,dtemp)));

    foo_double3(0)=10.0;
    foo_double3(1)=20.0;
    foo_double3(2)=30.0;
    
    AlwaysAssertExit(allEQ(foo_double3,
			   product(Foo_double, foo_double1)));
    
    Vector<Complex> foo_Complex1(3);
    foo_Complex1(0)=Complex(1.0,3.0);
    foo_Complex1(1)=Complex(2.0,2.0);
    foo_Complex1(2)=Complex(3.0,1.0);
    Vector<Complex> foo_Complex2(3);
    foo_Complex2(0)=Complex(3.0,1.0);
    foo_Complex2(1)=Complex(2.0,2.0);
    foo_Complex2(2)=Complex(1.0,3.0);
    
    AlwaysAssertExit(Complex(20.0,0)==innerProduct(foo_Complex1,foo_Complex2));
    
    AlwaysAssertExit(sqrt(27.99999) < norm(foo_Complex1));
    AlwaysAssertExit(sqrt(28.00001) > norm(foo_Complex1));
    
    Vector<Complex> foo_Complex3(3);
    foo_Complex3(0) = Complex(-8.0,0);
    foo_Complex3(1) = Complex(16.0,0);
    foo_Complex3(2) = Complex(-8.0,0);

    AlwaysAssertExit(allEQ(foo_Complex3,
			   crossProduct(foo_Complex1,foo_Complex2)));
    
    Matrix<Complex> Foo_Complex(3u,3u);
    
    Foo_Complex(0,0)=Complex(0,10.0);  
    Foo_Complex(1,0)=Complex(4.0,8.0); 
    Foo_Complex(2,0)=Complex(8.0,6.0);  
    Foo_Complex(0,1)=Complex(-4.0,8.0);
    Foo_Complex(1,1)=Complex(0,8.0);   
    Foo_Complex(2,1)=Complex(4.0,8.0); 
    Foo_Complex(0,2)=Complex(-8.0,6.0);
    Foo_Complex(1,2)=Complex(-4.0,8.0);
    Foo_Complex(2,2)=Complex(0,10.0);  

    AlwaysAssertExit(28.944271 < normI(Foo_Complex));
    AlwaysAssertExit(28.944272 > normI(Foo_Complex));

    AlwaysAssertExit(28.944271 < norm1(Foo_Complex));
    AlwaysAssertExit(28.944272 > norm1(Foo_Complex));

    Matrix<Complex> Foo_Complex_bar(3u,3u);

    Foo_Complex_bar(0,0)=Complex(0,-10.0);  
    Foo_Complex_bar(1,0)=Complex(-4.0,-8.0); 
    Foo_Complex_bar(2,0)=Complex(-8.0,-6.0); 
    Foo_Complex_bar(0,1)=Complex(4.0,-8.0); 
    Foo_Complex_bar(1,1)=Complex(0,-8.0);   
    Foo_Complex_bar(2,1)=Complex(-4.0,-8.0); 
    Foo_Complex_bar(0,2)=Complex(8.0,-6.0); 
    Foo_Complex_bar(1,2)=Complex(4.0,-8.0); 
    Foo_Complex_bar(2,2)=Complex(0,-10.0);  

    AlwaysAssertExit(allEQ(Foo_Complex,
			   conj(transpose(Foo_Complex_bar))));

    AlwaysAssertExit(allEQ(Foo_Complex,adjoint(Foo_Complex_bar)));

    Matrix<Complex> Ctemp(1u,3u);
    Ctemp.row(0)=foo_Complex2;
    AlwaysAssertExit(allEQ(Foo_Complex,product(foo_Complex1,Ctemp)));

    foo_Complex3(0)=Complex(-84.0,28.0);
    foo_Complex3(1)=Complex(-56.0,56.0);
    foo_Complex3(2)=Complex(-28.0,84.0);

    AlwaysAssertExit(allEQ(foo_Complex3,
			   product(Foo_Complex, foo_Complex1)));

    Vector<DComplex> foo_DComplex1(3);
    foo_DComplex1(0)=DComplex(1.0,3.0);
    foo_DComplex1(1)=DComplex(2.0,2.0);
    foo_DComplex1(2)=DComplex(3.0,1.0);
    Vector<DComplex> foo_DComplex2(3);
    foo_DComplex2(0)=DComplex(3.0,1.0);
    foo_DComplex2(1)=DComplex(2.0,2.0);
    foo_DComplex2(2)=DComplex(1.0,3.0);
    
    AlwaysAssertExit(DComplex(20.0,0)==innerProduct(foo_DComplex1,foo_DComplex2));

    AlwaysAssertExit(sqrt(27.99999) < norm(foo_DComplex1));
    AlwaysAssertExit(sqrt(28.00001) > norm(foo_DComplex1));

    Vector<DComplex> foo_DComplex3(3);
    foo_DComplex3(0) = DComplex(-8.0,0);
    foo_DComplex3(1) = DComplex(16.0,0);
    foo_DComplex3(2) = DComplex(-8.0,0);
    
    AlwaysAssertExit(allEQ(foo_DComplex3,
			   crossProduct(foo_DComplex1,foo_DComplex2)));
    
    Matrix<DComplex> Foo_DComplex(3u,3u);
    
    Foo_DComplex(0,0)=DComplex(0,10.0);  
    Foo_DComplex(1,0)=DComplex(4.0,8.0); 
    Foo_DComplex(2,0)=DComplex(8.0,6.0); 
    Foo_DComplex(0,1)=DComplex(-4.0,8.0);
    Foo_DComplex(1,1)=DComplex(0,8.0);   
    Foo_DComplex(2,1)=DComplex(4.0,8.0); 
    Foo_DComplex(0,2)=DComplex(-8.0,6.0);
    Foo_DComplex(1,2)=DComplex(-4.0,8.0);
    Foo_DComplex(2,2)=DComplex(0,10.0);  

    AlwaysAssertExit(28.9442719099 < normI(Foo_DComplex));
    AlwaysAssertExit(28.94427191 > normI(Foo_DComplex));

    AlwaysAssertExit(28.9442719099 < norm1(Foo_DComplex));
    AlwaysAssertExit(28.94427191 > norm1(Foo_DComplex));

    Matrix<DComplex> Foo_DComplex_bar(3u,3u);

    Foo_DComplex_bar(0,0)=DComplex(0,-10.0);   
    Foo_DComplex_bar(1,0)=DComplex(-4.0,-8.0); 
    Foo_DComplex_bar(2,0)=DComplex(-8.0,-6.0);
    Foo_DComplex_bar(0,1)=DComplex(4.0,-8.0);  
    Foo_DComplex_bar(1,1)=DComplex(0,-8.0);    
    Foo_DComplex_bar(2,1)=DComplex(-4.0,-8.0);
    Foo_DComplex_bar(0,2)=DComplex(8.0,-6.0); 
    Foo_DComplex_bar(1,2)=DComplex(4.0,-8.0); 
    Foo_DComplex_bar(2,2)=DComplex(0,-10.0);  

    AlwaysAssertExit(allEQ(Foo_DComplex,
			   conj(transpose(Foo_DComplex_bar))));

    AlwaysAssertExit(allEQ(Foo_DComplex,adjoint(Foo_DComplex_bar)));

    Matrix<DComplex> DCtemp(1u,3u);
    DCtemp.row(0)=foo_DComplex2;
    AlwaysAssertExit(allEQ(Foo_DComplex,product(foo_DComplex1,DCtemp)));
    
    foo_DComplex3(0)=Complex(-84.0,28.0);
    foo_DComplex3(1)=Complex(-56.0,56.0);
    foo_DComplex3(2)=Complex(-28.0,84.0);

    AlwaysAssertExit(allEQ(foo_DComplex3,
			   product(Foo_DComplex, foo_DComplex1)));

    Matrix<Int> foo_a(2u,2u),foo_b(2u,2u),foo_dPab(4u,4u);
    foo_a(0,0)=1; foo_a(0,1)=2; 
    foo_a(1,0)=3; foo_a(1,1)=4;
    foo_b(0,0)=4; foo_b(0,1)=3; 
    foo_b(1,0)=2; foo_b(1,1)=1;
    
    foo_dPab(0,0)=4;  foo_dPab(0,1)=3; foo_dPab(0,2)=8;  foo_dPab(0,3)=6;
    foo_dPab(1,0)=2;  foo_dPab(1,1)=1; foo_dPab(1,2)=4;  foo_dPab(1,3)=2;
    foo_dPab(2,0)=12; foo_dPab(2,1)=9; foo_dPab(2,2)=16; foo_dPab(2,3)=12;
    foo_dPab(3,0)=6;  foo_dPab(3,1)=3; foo_dPab(3,2)=8;  foo_dPab(3,3)=4;
    AlwaysAssertExit(allEQ(foo_dPab, directProduct(foo_a, foo_b)));

    Double alpha=1, cosa=cos(alpha), sina=sin(alpha);
    Matrix<Double> Rx(3u,3u);
    Rx(0,0) = 1;  Rx(0,1) = 0;    Rx(0,2) = 0;
    Rx(1,0) = 0;  Rx(1,1) = cosa; Rx(1,2) =-sina;
    Rx(2,0) = 0;  Rx(2,1) = sina; Rx(2,2) = cosa;
    AlwaysAssertExit(allEQ(Rx, Rot3D(0,alpha)));

    Matrix<Double> Ry(3u,3u);
    Ry(0,0) = cosa;  Ry(0,1) = 0; Ry(0,2) = sina;
    Ry(1,0) = 0;     Ry(1,1) = 1; Ry(1,2) = 0;
    Ry(2,0) =-sina;  Ry(2,1) = 0; Ry(2,2) = cosa;
    AlwaysAssertExit(allEQ(Ry, Rot3D(1,alpha)));

    Matrix<Double> Rz(3u,3u);
    Rz(0,0) = cosa;  Rz(0,1) =-sina; Rz(0,2) = 0;
    Rz(1,0) = sina;  Rz(1,1) = cosa; Rz(1,2) = 0;
    Rz(2,0) = 0;     Rz(2,1) = 0;    Rz(2,2) = 1;
    AlwaysAssertExit(allEQ(Rz, Rot3D(2,alpha)));


  } catch(AipsError x) {
    cout << "Caught exception : " << x.getMesg() << endl;
    return 1;
  } 

    cout << "OK" << endl;
    return 0;
}
