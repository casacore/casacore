//# tMathFunc.cc: This program tests MathFunc objects
//# Copyright (C) 1993,1994,1995,1997,1998,1999,2000,2001
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

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Mathematics/MathFunc.h>

#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>

int main()
{
try{
   cout << "\nMathFunc<Float> constants : " << endl;
   cout << "defcutoff() : " << MathFunc<Float>::defcutoff() << endl;
   cout << "defwidth() : " << MathFunc<Float>::defwidth() << endl;
   cout << "defKBwidth() : " << MathFunc<Float>::defKBwidth() << endl;
   cout << "defKBparm() : " << MathFunc<Float>::defKBparm() << endl;
   cout << "defmodKBparm() : " << MathFunc<Float>::defmodKBparm() << endl;
   cout << "defSphcutoff() : " << MathFunc<Float>::defSphcutoff() << endl;
   cout << "defSincparm() : " << MathFunc<Float>::defSincparm() << endl;
   cout << "defSphparm() : " << MathFunc<Float>::defSphparm() << endl;
   cout << "defExpPower() : " << MathFunc<Float>::defExpPower() << endl;
   cout << "defExpScale() : " << MathFunc<Float>::defExpScale() << endl;

   MathFunc<float> hh(new GaussianConv<float>());
   cout << "\ncreate a Gaussian hh by means of call to new GaussianConv\n";
   cout << " hh.sup_value() = " << hh.sup_value() << endl;
   cout << "\nGaussian computation\n";

   Float tmp[5] = {1, 0.193867, 0.00141258, 3.86835e-07, 3.9815e-12};

   for(int k = 0; k <5; k++)
     {
	 float result = hh.value((float) k);
	 AlwaysAssertExit(abs(result-tmp[k]) < 1.e-5); 
     cout << result <<"\n";
     }
   cout << "  " <<"\n";

   MathFunc<float> *p1;
   MathFunc<float> *p2;
   
   p1= MathFunc<float>::newMathFunc(GaussianConv<float>());

   cout << "\ncreate a Gaussian pointed to by p1 by call to newMathFunc\n";
   cout << " p1->sup_value() = " << p1->sup_value() << endl;
   int j;
   for(j = 0; j <5; j++)
     {

        float result = p1->value( (float) j);
	AlwaysAssertExit(abs(result-tmp[j]) < 1.e-5);
        cout << result <<"\n";
     }
   cout << "  " <<"\n";

   p2= MathFunc<float>::newMathFunc(Mod_KB_Conv<float>());
   cout << "\na Mod Kaiser Bessel pointed to by p2 by call to newMathFunc\n";
   cout << " p2->sup_value() = " << p2->sup_value() << endl;
   cout << "\nMod_KB \n";
   Float tmp1[5] = {1,0.315995,0.00662331,0.0268752,0.0072334};

   for(j = 0; j <5; j++)
     {
	 float result = p2->value((float) j);
	 AlwaysAssertExit(abs(result-tmp1[j]) < 1.e-5); 
     cout << result <<"\n";
     }
   cout << "  " <<"\n";
 
   MathFunc<float> a(MOD_KB);


   MathFunc<float> aa(MOD_KB);
   MathFunc<float> c(GAUSSIAN);
   MathFunc<float> cc(GAUSSIAN);
   MathFunc<float> d(SPHEROIDAL);
   MathFunc<float> dd(SPHEROIDAL);
   MathFunc<float> ee(GAUSSIAN, 2.0, 1.4);
   MathFunc<float> ff(GAUSSIAN, 2.1, 1.3);
   MathFunc<float> gg(MOD_KB, 2.1, 1.3,2.5,3.0);

   MathFunc<float>  *p; 

   p = &aa; 

   aa = a;
   cc = c;
   dd = d;

   cout << " Gaussian ee should have support width 2 " << endl;
   cout << " ee.sup_value() = " << ee.sup_value() << endl;
   cout << "\nGaussian computation\n";
   Float tmp2[5] = {1,0.243026,0.00348829,2.95718e-06,1.48064e-10};

   for(j = 0; j <5; j++)
     {
	 float result = ee.value((float) j);
	 AlwaysAssertExit(abs(result-tmp2[j]) < 1.e-5); 
     cout << result <<"\n";
     }

   cout << endl << endl;

   cout << " Gaussian ff should have support width 2.1 " << endl;
   cout << " ff.value(0.0) = " << ff.value(0.0) << endl;
   cout << " ff.sup_value() = " << ff.sup_value() << endl;
   cout << "\nGaussian computation (ff)\n";
   Float tmp3[5] = {
1,
0.193867,
0.00141258,
3.86835e-07,
3.9815e-12};


   for(j = 0; j <5; j++)
     {
	 float result = ff.value((float) j);
	 AlwaysAssertExit(abs(result-tmp3[j]) < 1.e-5); 
     cout << result <<"\n";
     }

   cout << endl << endl;

   cout << " Modified Kaiser Bessel gg  should have support width 2.1 " << endl;
   cout << " gg.value(0.0) = " << gg.value(0.0) << endl;
   cout << " gg.sup_value() = " << gg.sup_value() << endl;
   cout << "Modified Kaiser-Bessel computation(gg) \n";
   Float tmp4[5] = {1,
0.0807079,
0.142924,
0.0458648,
0.000233285};

   for(j = 0; j <5; j++)
     {
	 float result = gg.value((float) j);
	 AlwaysAssertExit(abs(result-tmp4[j]) < 1.e-5); 	 
     cout << result <<"\n";
     }
   cout << endl << endl << endl;

   cout << " aa.value(0.0) = " << aa.value(0.0) << endl;
   cout << " p->value(0.0) = " << p->value(0.0) << endl;

   cout << " aa.sup_value() = " << aa.sup_value() << endl;
   cout << " cc.sup_value() = " << cc.sup_value() << endl;
   cout << " dd.sup_value() = " << dd.sup_value() << endl;
   cout << endl;

   cout << "Modified Kaiser-Bessel computation\n";
   Float tmp5[5] = {
1,
0.315995,
0.00662331,
0.0268752,
0.0072334};

   int i;
   for(i = 0; i <5; i++)
     {
	 float result = aa.value((float) i);
	 AlwaysAssertExit(abs(result-tmp5[i]) < 1.e-5); 	 
     cout << result <<"\n";
     }

   Float tmp6[5] = {
     1,
0.193867,
0.00141258,
3.86835e-07,
3.9815e-12};

   cout << "\nGaussian computation\n";
   for(i = 0; i <5; i++)
     {
	 float result = cc.value((float) i);
	 AlwaysAssertExit(abs(result-tmp6[i]) < 1.e-5); 	 
     cout << result <<"\n";
     }

   Float tmp7[4] = {
1,
0.573245,
0.0826234,
0};

   cout << "\nSpheroidal computation\n";
   for(i = 0; i <4; i++)
     {
	 float result = dd.value((float) i);
	 AlwaysAssertExit(abs(result-tmp7[i]) < 1.e-5); 	 
     cout << result <<"\n";
     }
   cout << endl << "Unary function:" <<endl;
   MathFunc<float> foo(UNARY);
   for(i = 0; i <4; i++)
     {
	 float result = foo.value((float) i);
	 AlwaysAssertExit(abs(result-1.0) < 1.e-5); 	 
     cout << result <<"\n";
     }

   cout << "\nExponential*Sinc computation\n";
   MathFunc<float> expsinc(EXP_SINC, 3.0, 1.2, 3.0, 2.0);
   cout << " expsinc.sup_value() = " << expsinc.sup_value() << endl;
   Float tmp8[4] =
     {1,
0.170902,
-0.10605,
0.0468399};

   for (i = 0; i<4; i++)
       {
	   float result = expsinc.value((float) i);
	   AlwaysAssertExit(abs(result-tmp8[i]) < 1.e-5); 	 
	   cout << result << "\n";
       }

   delete p1;
   delete p2;

 } catch(AipsError x) {
   cout << "Unexpected exception: " << x.getMesg() << endl;
   return 1;
  } 

  cout << "OK" << endl; 
  return 0;
}
/*
This program should produce output similar to the following:

create a Gaussian hh by means of call to new GaussianConv
 hh.sup_value() = 2

Gaussian computation
1
0.193867
0.00141258
3.86835e-07
3.9815e-12
  

create a Gaussian pointed to by p1 by call to newMathFunc
 p1->sup_value() = 2
1
0.193867
0.00141258
3.86835e-07
3.9815e-12
  

a Mod Kaiser Bessel pointed to by p2 by call to newMathFunc
 p2->sup_value() = 2

Mod_KB 
1
0.315995
0.00662331
0.0268752
0.0072334
  
 Gaussian ee should have support width 2 
 ee.sup_value() = 2

Gaussian computation
1
0.243026
0.00348829
2.95718e-06
1.48064e-10


 Gaussian ff should have support width 2.1 
 ff.value(0.0) = 1
 ff.sup_value() = 2.1

Gaussian computation (ff)
1
0.193867
0.00141258
3.86835e-07
3.9815e-12


 Modified Kaiser Bessel gg  should have support width 2.1 
 gg.value(0.0) = 1
 gg.sup_value() = 2.1
Modified Kaiser-Bessel computation(gg) 
1
0.0807079
0.142924
0.0458648
0.000233285






 aa.value(0.0) = 1
 p->value(0.0) = 1
 aa.sup_value() = 2
 cc.sup_value() = 2
 dd.sup_value() = 3

Modified Kaiser-Bessel computation
1
0.315995
0.00662331
0.0268752
0.0072334

Gaussian computation
1
0.193867
0.00141258
3.86835e-07
3.9815e-12

Spheroidal computation
1
0.573245
0.0826234
0

Unary function:
1
1
1
1

Exponential*Sinc computation
 expsinc.sup_value() = 3
1
0.170902
-0.10605
0.0468399

*/
