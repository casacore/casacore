//# tQuantum.cc: test program for Quantum and QC class
//# Copyright (C) 1994,1995,1996,1998,1999,2000,2002
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


#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main ()
{
    Quantity A(5,"m"), B(2.,"yd");
    Quantity C(3.);
    Quantity D(6,"Jy");
    Quantity DEG(5,"deg");
    Quantity l4;
    Quantum<Complex> CQ(7,"ml");
    Vector<Double> V(2),V2(2),V1(2);
    Vector<Int> VI(2);
    V(0)=3; V(1)=4; V2=2; V1=3;
    VI(0)=7; VI(1)=8;
    Quantum<Vector<Double> > VQ(V,"uA");
    Vector<Quantity> QVQ(3);
    QVQ(0)=A; QVQ(1)=D; QVQ(2)=DEG;

try {

    cout << "Test quantity classes (Quantum, QC, Quantity)..." << endl;

    cout << endl << "--------------------------" << endl;
    cout << "Some physical constants:" << endl << endl;

    cout << "    QC::c        " << QC::c << endl;
    cout << "    QC::G        " << QC::G << endl;
    cout << "    QC::h        " << QC::h << endl;
    cout << "    QC::HI       " << QC::HI << endl;
    cout << "    QC::R        " << QC::R << endl;
    cout << "    QC::NA       " << QC::NA << endl;
    cout << "    QC::e        " << QC::e << endl;
    cout << "    QC::mp       " << QC::mp << endl;
    cout << "    QC::mp_me    " << QC::mp_me << endl;
    cout << "    QC::mu0      " << QC::mu0 << endl;
    cout << "    QC::epsilon0 " << QC::epsilon0 << endl;
    cout << "    QC::k        " << QC::k << endl;
    cout << "    QC::F        " << QC::F << endl;
    cout << "    QC::me       " << QC::me << endl;
    cout << "    QC::re       " << QC::re << endl;
    cout << "    QC::a0       " << QC::a0 << endl;
    cout << "    QC::R0       " << QC::R0 << endl;
    cout << "    QC::k2       " << QC::k2 << endl;
    
    cout << endl << "--------------------------" << endl;
    cout << "Manipulate quantities:" << endl << endl;
    UnitMap::clearCache();
    
    cout << "QVQ         = Vector(A,D,DEG)        = " << QVQ << endl;

    cout << "A           = Quantity(5,\"m\")      = " << A << endl;
    cout << "B           = Quantity(2.,\"yd\")    = " << B << endl;
    cout << "C           = Quantity(3.)           = " << C << endl;
    cout << "D           = Quantity(6,\"Jy\")     = " << D << endl;
    cout << "DEG         = Quantity(5,\"deg\")    = " << DEG << endl;
    cout << "CQ          = Complex(7,\"ml\")      = " << CQ << endl;
    cout << "VQ          = Vector((3,4),\"uA\")   = " << VQ << endl;
    cout << "V2          = Vector(2,2)            = " << V2 << endl;
    cout << "V1          = Vector(3)              = " << V1 << endl;
    Quantity *E=new Quantity(10,"L");
    cout << "*E          = new Quantity(10,\"L\") = "<< *E << endl;
    delete E;

    cout << "-QVQ        = " << -QVQ << endl;
    cout << "A*QVQ       = " << A*QVQ << endl;
    cout << "A*B         = " << A*B << endl;
    cout << "-B          = " << -B << endl;
    cout << "2.*B        = " << 2.*B << endl;
    cout << "B*2.        = " << B*2. << endl;
    cout << "B/2.        = " << B/2. << endl;
    cout << "2*CQ        = " << Complex(2)*CQ << endl;
    cout << "V2*VQ       = " << V2*VQ << endl;
    cout << "V1*VQ       = " << V1*VQ << endl;
    cout << "A*C         = " << A*C << endl;
    cout << "A/B         = " << A/B << endl;
    cout << "A/C         = " << A/C << endl;
    cout << "(A/B)^-5    = " << pow((A/B),-5) << endl;
    cout << "A+B         = " << A+B << endl;
    cout << "A-B         = " << A-B << endl;
    cout << "B+A         = " << B+A << endl;
    cout << "B-A         = " << B-A << endl;
    cout << "abs(A)      = " << abs(A) << endl;
    cout << "abs(CQ)     = " << abs(CQ) << endl;
    cout << "abs(VQ)     = " << abs(VQ) << endl;
    cout << "ceil(A)     = " << ceil(A) << endl;
    cout << "ceil(CQ)    = " << ceil(CQ) << endl;
    cout << "ceil(VQ)    = " << ceil(VQ) << endl;
    cout << "floor(A)    = " << floor(A) << endl;
    cout << "floor(CQ)   = " << floor(CQ) << endl;
    cout << "floor(VQ)   = " << floor(VQ) << endl;
    cout << "sin(DEG)    = " << sin(DEG) << endl;
    cout << "asin()      = " << asin(sin(DEG)) << endl;
    cout << "atan2(1,5)  = " << atan2(Quantity(1),Quantity(5)) << endl;
    cout << "log()       = " << log(Quantity(2)) << endl;
    cout << "log10()     = " << log10(Quantity(2)) << endl;
    cout << "exp()       = " << exp(Quantity(2)) << endl;
    cout << "sqrt()      = " << sqrt(Quantity(2)) << endl;
    cout << "sqrt(m2.s4) = " << sqrt(Quantity(2, "m2.s4")) << endl;
    cout << "sqrt(Jy2)   = " << sqrt(Quantity(2, "Jy2")) << endl;
    cout << "sqrt(Jy2)^2 = " <<
      sqrt(Quantity(2, "Jy2")) * sqrt(Quantity(2, "Jy2")) << endl;
    cout << "B==A        = " << (B==A) << endl;
    cout << "VQ==V2      = " << (VQ==V2) << endl;
    cout << "VQ!=V1      = " << (VQ==V1) << endl;
    cout << "B!=A        = " << (B!=A) << endl;
    cout << "D==A        = " << (D==A) << endl;
    cout << "D!=A        = " << (D!=A) << endl;
    cout << "B<A         = " << (B<A) << endl;
    cout << "B>A         = " << (B>A) << endl;
    cout << "B<=A        = " << (B<=A) << endl;
    cout << "B>=A        = " << (B>=A) << endl;

    cout << endl << "--------------------------" << endl;
    cout << "Convert      " << "A*B" << " to:" << endl;

    cout << "    m:         " << (A*B).get("m") << endl;
    cout << "    yd:        " << (A*B).get("yd") << endl;
    cout << "    m2:        " << (A*B).get("m2") << endl;
    cout << "    yd2:       " << (A*B).get("yd2") << endl;
    cout << "    unit:      " << (A*B).getUnit() << endl;
    cout << "    canonical: " << (A*B).get() << endl;
    cout << "    can val:   " << (A*B).get().getValue() << endl;
    cout << "    can unit:  " << (A*B).get().getUnit() << endl;
    cout << "    km2 val:   " << (A*B).get("km2").getValue() << endl;
    
    cout << endl << "--------------------------" << endl;
    cout << "Other conversions:" << endl;

    cout << "A to B        " << (A.convert(B),A) << endl;
    cout << "D to Watt     " << D.get("W") << endl;
    cout << "A value to 9  " << (A.setValue(9),A) << endl;
    cout << "A unit to km  " << (A.setUnit("km"),A) << endl;
    cout << "scale A by 3  " << (A.scale(3),A) << endl;
    
    cout << endl << "--------------------------" << endl;
    cout << "Check types" << endl << endl;

    cout << "    Are dam's Mpc's? " << Quantity(1,"dam").isConform("Mpc") <<
      endl;
    cout << "    Are Jy's Watts?  " << Quantity(1,"Jy").isConform("W") << endl;
    
    cout << endl << "--------------------------" << endl;
    cout << "List contents of Cache" << endl << endl;
    
    UnitMap::listCache();
    
    cout << endl << "--------------------------" << endl;
    
    Quantum<Int> ll5(5,Quantum<Double>(7.,"mm/s"));
    cout << "Mixed Quantity/Quantum<Int>  " << ll5 << endl;
    
} catch (AipsError x) {
  cout << x.getMesg() << endl;
} 
    
    cout << endl << "--------------------------" << endl;
    cout << "Try illegal operations" << endl << endl;
    
    try {
	Quantity loc(5,"KpH");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
	Quantity loc(A+D);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
        A<D;
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
	l4=pow(A,200);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
	A.convert("JY");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
	l4 = sin(A);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
	l4 = log(A);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    try {
	l4 = sqrt(A);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    AlwaysAssert(max(A, B) == A, AipsError);
    AlwaysAssert(max(B, A) == A, AipsError);
    AlwaysAssert(min(B, A) == B, AipsError);
    AlwaysAssert(min(A, B) == B, AipsError);

    AlwaysAssert(
    	nearAbs(
    		Quantity(4, "km"), Quantity(4000.001, "m"),
    		Quantity(1, "cm")
    	), AipsError
    );
    AlwaysAssert(
    	nearAbs(
    		Quantity(4.00001, "km"), Quantity(4000, "m"),
    		Quantity(1, "cm")
        ), AipsError
    );
    AlwaysAssert(
    	! nearAbs(
    		Quantity(4, "km"), Quantity(4000.001, "m"),
    		Quantity(0.5, "mm")
        ), AipsError
    );
    AlwaysAssert(
    	! nearAbs(
    		Quantity(4.00001, "km"), Quantity(4000, "m"),
    		Quantity(0.5, "mm")
    	), AipsError
    );
    cout << endl << "--------------------------" << endl;
    return 0;
}
