//# MeasTable.cc: MeasTable provides Measure computing database data
//# Copyright (C) 1995,1996,1997,1998
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

//# Includes
#ifdef __GNUG__
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_MeasTable_bug1;
#endif
#include <aips/Measures/MeasTable.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/Euler.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasIERS.h>
#include <aips/Measures/MeasJPL.h>
#include <aips/Quanta/MUString.h>
#include <aips/OS/Time.h>
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Logging.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableRow.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Containers/RecordField.h>

//# Constants

//# Data
Bool MeasTable::obsNeedInit = True;
Vector<String> MeasTable::obsNams(0);
Vector<MPosition> MeasTable::obsPos(0);
Bool MeasTable::srcNeedInit = True;
Vector<String> MeasTable::srcNams(0);
Vector<MDirection> MeasTable::srcPos(0);
Double MeasTable::timeIGRF = -1e6;
Double MeasTable::dtimeIGRF = 0;
Double MeasTable::time0IGRF = -1e6;
Double MeasTable::firstIGRF = 0;
Double MeasTable::lastIGRF = 0;
Vector<Double> MeasTable::coefIGRF(0);
Vector<Double> MeasTable::dIGRF(0);
Vector<Double> MeasTable::resIGRF(0);

//# Member functions
void MeasTable::
        precessionCoef(Double T,
		   Polynomial<Double> result[3]) {
    static const Double PCOEF[3][6] = {
        {+2306.2181,+1.39656,-0.000139,+0.30188,-0.000344,+0.017998},
        {+2004.3109,-0.85330,-0.000217,-0.42665,-0.000217,-0.041833},
        {+2306.2181,+1.39656,-0.000139,+1.09468,-0.000066,+0.018203}
    };
    calcPrecesCoef(T, result, &PCOEF[0]);
}

void MeasTable::precessionCoef1950(Double T,
				  Polynomial<Double> result[3]) {
    static const Double PCOEF[3][6] = {
        {2303.5545,+1.39720,0.000060,+0.30240,-0.000270,+0.017995},
        {2005.1120,-0.85290,-0.00037,-0.42650,-0.000370,-0.041800},
        {2303.5545,+1.39720,0.000060,+1.09480,+0.000390,+0.018325}
    };
    calcPrecesCoef(T, result, &PCOEF[0]);
}

void MeasTable::calcPrecesCoef(Double T, Polynomial<Double> result[3],
			       const Double coef[3][6]) {
    Int i,j,k,l; Int m=1;
    for (i=0; i<3; i++) {
	m = -m;
	l = 0;
	for (j=0; j<3; j++) {
	    Polynomial<Double> poly(2-j);
	    for (k=0; k<3-j; k++, l++) {
		poly.setCoefficient(k,coef[i][l]);
	    }
	    result[i].setCoefficient(j+1,m*poly(T) * C::arcsec);
	}
    }
}

const Polynomial<Double> &MeasTable::fundArg(uInt which) {
    static Bool needInit = True;
    static Polynomial<Double> polyArray[6];
    static const Double FUND[6][4] = {
        {84381.448,   -46.8150,       -0.0059, 0.001813}, 
        {485866.733,  1717915922.633, 31.310,  0.064}, 
        {1287099.804, 129596581.224,  -0.577,  -0.012}, 
        {335778.877,  1739527263.137, -13.257, 0.011}, 
        {1072261.307, 1602961601.328, -6.891,  0.019}, 
        {450160.280,  -6962890.539,   7.455,   0.008}
    };
    calcFundArg(needInit, polyArray, &FUND[0]);
    DebugAssert(which < 6, AipsError);
    return polyArray[which];
}

const Polynomial<Double> &MeasTable::fundArg1950(uInt which) {
    static Bool needInit = True;
    static Polynomial<Double> polyArray[6];
    static const Double FUND[6][4] = {
        {84428.26,   -46.846,       -0.0059, 0.00181},
        {1065976.59, 1717915856.79, 33.09,   0.0518},
        {1290513.0,  129596579.1,   -0.54,   -0.0120},
        {40503.2,    1739527290.54, -11.56,  -0.0012},
        {1262654.95, 1602961611.18, -5.17,   0.0068},
        {933059.79,  -6962911.23,   7.48,    0.0080}
    };
    calcFundArg(needInit, polyArray, &FUND[0]);
    DebugAssert(which < 6, AipsError);
    return polyArray[which];
}

void MeasTable::calcFundArg(Bool &need, 
			   Polynomial<Double> result[3],
			   const Double coeff[6][4]) {
    if (need) {
	need = False;
	Int i,j;
	for (i=0; i<6; i++) {
	    result[i] = Polynomial<Double>(3);
	    for (j=0; j<4; j++) {
		result[i].setCoefficient(j, coeff[i][j]*C::arcsec);
	    };
	};
    };
}    

const Vector<Char> &MeasTable::mulArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[106];
    static const Char ARG[106][5] = {
    {0	,0	,0	,0	,1	},
    {0	,0	,0	,0	,2	},
    {-2	,0	,2	,0	,1	},
    {2	,0	,-2	,0	,0	},
    {-2	,0	,2	,0	,2	},

    {1	,-1	,0	,-1	,0	},
    {0	,-2	,2	,-2	,1	},
    {2	,0	,-2	,0	,1	},
    {0	,0	,2	,-2	,2	},
    {0	,1	,0	,0	,0	},

    {0	,1	,2	,-2	,2	},
    {0	,-1	,2	,-2	,2	},
    {0	,0	,2	,-2	,1	},
    {2	,0	,0	,-2	,0	},
    {0	,0	,2	,-2	,0	},

    {0	,2	,0	,0	,0	},
    {0	,1	,0	,0	,1	},
    {0	,2	,2	,-2	,2	},
    {0	,-1	,0	,0	,1	},
    {-2	,0	,0	,2	,1	},

    {0	,-1	,2	,-2	,1	},
    {2	,0	,0	,-2	,1	},
    {0	,1	,2	,-2	,1	},
    {1	,0	,0	,-1	,0	},
    {2	,1	,0	,-2	,0	},

    {0	,0	,-2	,2	,1	},
    {0	,1	,-2	,2	,0	},
    {0	,1	,0	,0	,2	},
    {-1	,0	,0	,1	,1	},
    {0	,1	,2	,-2	,0	},

    {0	,0	,2	,0	,2	},
    {1	,0	,0	,0	,0	},
    {0	,0	,2	,0	,1	},
    {1	,0	,2	,0	,2	},
    {1	,0	,0	,-2	,0	},

    {-1	,0	,2	,0	,2	},
    {0	,0	,0	,2	,0	},
    {1	,0	,0	,0	,1	},
    {-1	,0	,0	,0	,1	},
    {-1	,0	,2	,2	,2	},

    {1	,0	,2	,0	,1	},
    {0	,0	,2	,2	,2	},
    {2	,0	,0	,0	,0	},
    {1	,0	,2	,-2	,2	},
    {2	,0	,2	,0	,2	},

    {0	,0	,2	,0	,0	},
    {-1	,0	,2	,0	,1	},
    {-1	,0	,0	,2	,1	},
    {1	,0	,0	,-2	,1	},
    {-1	,0	,2	,2	,1	},

    {1	,1	,0	,-2	,0	},
    {0	,1	,2	,0	,2	},
    {0	,-1	,2	,0	,2	},
    {1	,0	,2	,2	,2	},
    {1	,0	,0	,2	,0	},

    {2	,0	,2	,-2	,2	},
    {0	,0	,0	,2	,1	},
    {0	,0	,2	,2	,1	},
    {1	,0	,2	,-2	,1	},
    {0	,0	,0	,-2	,1	},

    {1	,-1	,0	,0	,0	},
    {2	,0	,2	,0	,1	},
    {0	,1	,0	,-2	,0	},
    {1	,0	,-2	,0	,0	},
    {0	,0	,0	,1	,0	},
    
    {1	,1	,0	,0	,0	},
    {1	,0	,2	,0	,0	},
    {1	,-1	,2	,0	,2	},
    {-1	,-1	,2	,2	,2	},
    {-2	,0	,0	,0	,1	},

    {3	,0	,2	,0	,2	},
    {0	,-1	,2	,2	,2	},
    {1	,1	,2	,0	,2	},
    {-1	,0	,2	,-2	,1	},
    {2	,0	,0	,0	,1	},

    {1	,0	,0	,0	,2	},
    {3	,0	,0	,0	,0	},
    {0	,0	,2	,1	,2	},
    {-1	,0	,0	,0	,2	},
    {1	,0	,0	,-4	,0	},

    {-2	,0	,2	,2	,2	},
    {-1	,0	,2	,4	,2	},
    {2	,0	,0	,-4	,0	},
    {1	,1	,2	,-2	,2	},
    {1	,0	,2	,2	,1	},

    {-2	,0	,2	,4	,2	},
    {-1	,0	,4	,0	,2	},
    {1	,-1	,0	,-2	,0	},
    {2	,0	,2	,-2	,1	},
    {2	,0	,2	,2	,2	},
    
    {1	,0	,0	,2	,1	},
    {0	,0	,4	,-2	,2	},
    {3	,0	,2	,-2	,2	},
    {1	,0	,2	,-2	,0	},
    {0	,1	,2	,0	,1	},

    {-1	,-1	,0	,2	,1	},
    {0	,0	,-2	,0	,1	},
    {0	,0	,2	,-1	,2	},
    {0	,1	,0	,2	,0	},
    {1	,0	,-2	,-2	,0	},

    {0	,-1	,2	,0	,1	},
    {1	,1	,0	,-2	,1	},
    {1	,0	,-2	,2	,0	},
    {2	,0	,0	,2	,0	},
    {0	,0	,2	,4	,2	},

    {0	,1	,0	,1	,0	}
    };
    calcMulArg(needInit, argArray, &ARG[0], 106);
    DebugAssert(which < 106, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulArg1950(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[69];
    static const Char ARG[69][5] = {
    {0	,0	,0	,0	,1	},
    {0	,0	,0	,0	,2	},
    {-2	,0	,2	,0	,1	},
    {2	,0	,-2	,0	,0	},
    {0  ,-2     ,2	,-2	,1	},

    {-2	,0	,2	,0	,2	},
    {1	,-1	,0	,-1	,0	},
    {0	,0	,2	,-2	,2	},
    {0	,1	,0	,0	,0	},
    {0	,1	,2	,-2	,2	},

    {0	,-1	,2	,-2	,2	},
    {0	,0	,2	,-2	,1	},
    {2	,0	,0	,-2	,0	},
    {0	,0	,2	,-2	,0	},
    {0	,2	,0	,0	,0	},

    {0	,1	,0	,0	,1	},
    {0	,2	,2	,-2	,2	},
    {0	,-1	,0	,0	,1	},
    {-2	,0	,0	,2	,1	},
    {0	,-1	,2	,-2	,1	},

    {2	,0	,0	,-2	,1	},
    {0	,1	,2	,-2	,1	},
    {1	,0	,0	,-1	,0	},
    {0	,0	,2	,0	,2	},
    {1	,0	,0	,0	,0	},

    {0	,0	,2	,0	,1	},
    {1	,0	,2	,0	,2	},
    {1	,0	,0	,-2	,0	},
    {-1	,0	,2	,0	,2	},
    {0	,0	,0	,2	,0	},

    {1	,0	,0	,0	,1	},
    {-1	,0	,0	,0	,1	},
    {-1	,0	,2	,2	,2	},
    {1	,0	,2	,0	,1	},
    {0	,0	,2	,2	,2	},

    {2	,0	,0	,0	,0	},
    {1	,0	,2	,-2	,2	},
    {2	,0	,2	,0	,2	},
    {0	,0	,2	,0	,0	},
    {-1	,0	,2	,0	,1	},

    {-1	,0	,0	,2	,1	},
    {1	,0	,0	,-2	,1	},
    {-1	,0	,2	,2	,1	},
    {1	,1	,0	,-2	,0	},
    {0	,1	,2	,0	,2	},

    {1	,0	,0	,2	,0	},
    {0	,0	,0	,2	,1	},
    {0	,-1	,2	,0	,2	},
    {1	,0	,2	,2	,2	},
    {2	,0	,2	,-2	,2	},

    {0	,0	,0	,-2	,1	},
    {0	,0	,2	,2	,1	},
    {1	,0	,2	,-2	,1	},
    {0	,0	,0	,1	,0	},
    {0	,1	,0	,-2	,0	},

    {1	,-1	,0	,0	,0	},
    {1	,0	,-2	,0	,0	},
    {2	,0	,2	,0	,1	},
    {1	,0	,2	,0	,0	},
    {1	,1	,0	,0	,0	},

    {1	,-1	,2	,0	,2	},
    {-2	,0	,0	,0	,1	},
    {-1	,0	,2	,-2	,1	},
    {2	,0	,0	,0	,1	},
    {-1	,-1	,2	,2	,2	},
    
    {0	,-1	,2	,2	,2	},
    {1	,0	,0	,0	,2	},
    {1	,1	,2	,0	,2	},
    {3	,0	,2	,0	,2	}
    };
    calcMulArg(needInit, argArray, &ARG[0], 69);
    DebugAssert(which < 69, AipsError);
    return argArray[which];
}

void MeasTable::calcMulArg(Bool &need, Vector<Char> result[],
			  const Char coeff[][5], Int row){
    if (need) {
	need = False;
	Int i,j;
	for (i=0; i<row; i++) {
	    result[i].resize(5);
	    result[i].makePermanent();
	    for (j=0; j<5; j++) {
		result[i](j) = coeff[i][j];
	    }
	}
    }
}

const Vector<Double> &MeasTable::mulSC(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[106];
    static Polynomial<Double> polyArray[30];
    static const Long MULTD[15][5] = {
    {0  ,-171996 ,-1742 ,92025 ,89},
    {1  ,2062    ,2     ,-895  ,5},
    {8  ,-13187  ,-16   ,5736  ,-31},
    {9  ,1426    ,-34   ,54    ,-1},
    {10 ,-517    ,12    ,224   ,-6},
    {11 ,217     ,-5    ,-95   ,3},
    {12 ,129     ,1     ,-70   ,0},
    {15 ,17      ,-1    ,0     ,0},
    {17 ,-16     ,1     ,7     ,0},
    {30 ,-2274   ,-2    ,977   ,-5},
    {31 ,712     ,1     ,-7    ,0},
    {32 ,-386    ,-4    ,200   ,0},
    {33 ,-301    ,0     ,129   ,-1},
    {37 ,63      ,1     ,-33   ,0},
    {38 ,-58     ,-1    ,32    ,0}
};
    static const Short MULSC[106][2] = {
    {0	,0	},
    {0	,0	},
    {46	,-24	},
    {11	,0	},
    {-3	,1	},

    {-3	,0	},
    {-2	,1	},
    {1	,0	},
    {0	,0	},
    {0	,0	},

    {0	,0	},
    {0	,0	},
    {0	,0	},
    {48	,1	},
    {-22	,0	},

    {0	,0	},
    {-15	,9	},
    {0	,0	},
    {-12	,6	},
    {-6	,3	},

    {-5	,3	},
    {4	,-2	},
    {4	,-2	},
    {-4	,0	},
    {1	,0	},

    {1	,0	},
    {-1	,0	},
    {1	,0	},
    {1	,0	},
    {-1	,0	},

    {0	,0	},
    {0	,0	},
    {0	,0	},
    {0	,0	},
    {-158	,-1	},

    {123	,-53	},
    {63	,-2	},
    {0	,0	},
    {0	,0	},
    {-59	,26	},

    {-51	,27	},
    {-38	,16	},
    {29	,-1	},
    {29	,-12	},
    {-31	,13	},

    {26	,-1	},
    {21	,-10	},
    {16	,-8	},
    {-13	,7	},
    {-10	,5	},

    {-7	,0	},
    {7	,-3	},
    {-7	,3	},
    {-8	,3	},
    {6	,0	},

    {6	,-3	},
    {-6	,3	},
    {-7	,3	},
    {6	,-3	},
    {-5	,3	},

    {5	,0	},
    {-5	,3	},
    {-4	,0	},
    {4	,0	},
    {-4	,0	},
    
    {-3	,0	},
    {3	,0	},
    {-3	,1	},
    {-3	,1	},
    {-2	,1	},

    {-3	,1	},
    {-3	,1	},
    {2	,-1	},
    {-2	,1	},
    {2	,-1	},

    {-2	,1	},
    {2	,0	},
    {2	,-1	},
    {1	,-1	},
    {-1	,0	},

    {1	,-1	},
    {-2	,1	},
    {-1	,0	},
    {1	,-1	},
    {-1	,1	},

    {-1	,1	},
    {1	,0	},
    {1	,0	},
    {1	,-1	},
    {-1	,0	},
    
    {-1	,0	},
    {1	,0	},
    {1	,0	},
    {-1	,0	},
    {1	,0	},

    {1	,0	},
    {-1	,0	},
    {-1	,0	},
    {-1	,0	},
    {-1	,0	},

    {-1	,0	},
    {-1	,0	},
    {-1	,0	},
    {1	,0	},
    {-1	,0	},

    {1	,0}
};
    calcMulSC(needInit, checkT, T, argArray, 106, polyArray,
	      &MULTD[0], 15, &MULSC[0]);
    DebugAssert(which < 106, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulSC1950(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[69];
    static Polynomial<Double> polyArray[26];
    static const Long MULTD[13][5] = {
    {0  ,-172327 ,-1737 ,92100 ,91},
    {1  ,2088    ,2     ,-904  ,4},
    {7  ,-12729  ,-13   ,5522  ,-29},
    {8  ,1261    ,-31   ,0     ,0},
    {9  ,-497    ,12    ,216   ,-6},
    {10 ,214     ,-5    ,-93   ,3},
    {11 ,124     ,1     ,-66   ,0},
    {14 ,16      ,-1    ,0     ,0},
    {16 ,-15     ,1     ,7     ,0},
    {23 ,-2037   ,-2    ,884   ,-5},
    {24 ,675     ,1     ,0     ,0},
    {25 ,-342    ,-4    ,183   ,0},
    {26 ,-261    ,0     ,113   ,-1}
};
    static const Short MULSC[69][2] = {
    {0	,0	},
    {0	,0	},
    {45	,-24	},
    {10	,0	},
    {-4	,2	},

    {-3	,2	},
    {-2	,0	},
    {0	,0	},
    {0	,0	},
    {0	,0	},

    {0	,0	},
    {0	,0	},
    {45	,0	},
    {-21	,0	},
    {0	,0	},

    {-15	,8	},
    {0	,0	},
    {-10	,5	},
    {-5	,3	},
    {-5	,3	},

    {4	,-2	},
    {3	,-2	},
    {-3	,0	},
    {0	,0	},
    {0	,0	},

    {0	,0	},
    {0	,0	},
    {-149	,0	},
    {114	,-50	},
    {60	,0	},

    {58	,-31	},
    {-57	,30	},
    {-52	,22	},
    {-44	,23	},
    {-32	,14	},

    {28	,0	},
    {26	,-11	},
    {-26	,11	},
    {25	,0	},
    {19	,-10	},

    {14	,-7	},
    {-13	,7	},
    {-9	,5	},
    {-7	,0	},
    {7	,-3	},

    {6	,0	},
    {-6	,3	},
    {-6	,3	},
    {-6	,3	},
    {6	,-2	},

    {-5	,3	},
    {-5	,3	},
    {5	,-3	},
    {-4	,0	},
    {-4	,0	},

    {4	,0	},
    {4	,0	},
    {-4	,2	},
    {3	,0	},
    {-3	,0	},
    
    {-3	,0	},
    {-2	,0	},
    {-2	,0	},
    {2	,0	},
    {-2	,0	},

    {-2	,0	},
    {-2	,0	},
    {2	,0	},
    {-2	,0	}
};
    calcMulSC(needInit, checkT, T, argArray, 69, polyArray,
	      &MULTD[0], 13, &MULSC[0]);
    DebugAssert(which < 69, AipsError);
    return argArray[which];
}

void MeasTable::calcMulSC(Bool &need, Double &check, Double T,
			 Vector<Double> result[], Int resrow,
			 Polynomial<Double> poly[],
			 const Long coeffTD[][5], Int TDrow,
			 const Short coeffSC[][2]) {
    if (need) {
	need = False;
	Int i,j;
	for (i=0; i<TDrow; i++) {
	    for (j=0; j<2; j++) {
		poly[2*i+j] = Polynomial<Double>(2);
		poly[2*i+j].setCoefficient(0,
				      coeffTD[i][1+2*j]*C::arcsec*1e-4);
		poly[2*i+j].setCoefficient(1,
				      coeffTD[i][2+2*j]*C::arcsec*1e-5);
	    }
	}
	for (i=0; i<resrow; i++) {
	    result[i].resize(4);
	    result[i].makePermanent();
	    for (j=0; j<2; j++) {
		result[i](j) = coeffSC[i][j] * C::arcsec*1e-4;
	    }
	    for (j=2; j<4; j++) {
		result[i](j) = 0;
	    }
	}
    }
    if (check != T) {
	check = T;
	Int i; Long j;
	for (i=0; i<TDrow; i++) { // get fundamental argument coefficients
	    j = coeffTD[i][0];
	    result[j](0) = poly[2*i+0](T);
	    result[j](1) = poly[2*i+1](T);
	    result[j](2) = (poly[2*i+0].derivative())(T);
	    result[j](3) = (poly[2*i+1].derivative())(T);
	}
    }
}

Double MeasTable::dPsiEps(uInt which, Double T) {
  static Bool msgDone = False;
  DebugAssert(which < 2, AipsError);
  Double r = 0;
  switch (which) {
  case 1:
    if (!MeasIERS::get(r, MeasIERS::MEASURED, MeasIERS::dEps,
		       T)) {
      if (!msgDone) {
	msgDone = True;
	LogIO os(LogOrigin("MeasTable",
			   String("dPsiEps(uInt, Double)"),
			   WHERE));
	os << LogIO::WARN <<
	  String("No requested nutation data available from IERS tables. "
		 "\nProceeding with probably less precision.") <<
	  LogIO::POST;
      };
    };
    break;
  default:
    if (!MeasIERS::get(r, MeasIERS::MEASURED, MeasIERS::dPsi,
		       T)) {
      if (!msgDone) {
	msgDone = True;
	LogIO os(LogOrigin("MeasTable",
			   String("dPsiEps(uInt, Double)"),
			   WHERE));
	os << LogIO::WARN <<
	  String("No requested nutation data available from IERS tables. "
		 "\nProceeding with probably less precision.") <<
	  LogIO::POST;
      };
    };
    break;
  };
  return (r * C::arcsec);
}

// Planetary data
const Vector<Double> &MeasTable::Planetary(MeasTable::Types which, 
					   Double T) {
  static Vector<Double> res(6);
  static Bool needInit = True;
  static MeasJPL::Files fil(MeasJPL::DE200);
  static String tnam[2] = { "DE200", "DE405"};
  if (needInit) {
    needInit = False;
    res.makePermanent();
    uInt t;
    Aipsrc::find (t, String("measures.jpl.ephemeris"), 2, tnam,
		  String("DE200"));
    fil = (MeasJPL::Files)t;
  };
  if (!MeasJPL::get(res, fil, (MeasJPL::Types)which,
		    MVEpoch(T))) {
    LogIO os(LogOrigin("MeasTable",
		       String("Planetary(MeasTable::Types, Double)"),
		       WHERE));
    os << String("Cannot find the planetary data table ") +
		 tnam[fil] << LogIO::EXCEPTION;
  };
  return res;
}

// Planetary constants
const Double &MeasTable::Planetary(MeasTable::JPLconst what) {
  static Bool needInit = True;
  static Double cn[MeasTable::N_JPLconst];
  static MeasJPL::Files fil(MeasJPL::DE200);
  static String tnam[2] = { "DE200", "DE405"};
  if (needInit) {
    needInit = False;
    uInt t;
    Aipsrc::find (t, String("measures.jpl.ephemeris"), 2, tnam,
		  String("DE200"));
    fil = (MeasJPL::Files)t;
    for (uInt i=0; i<MeasTable::N_JPLconst; i++) {
      if (!MeasJPL::getConst(cn[i], fil, 
			     (MeasJPL::Codes) i)) {
	LogIO os(LogOrigin("MeasTable",
			   String("Planetary(MeasTable::JPLconst)"),
			   WHERE));
	os << String("Cannot find the planetary data table ") +
		     tnam[fil] << LogIO::EXCEPTION;
      };
    };
  };
  return cn[what];
}

// Observatory data
void MeasTable::initObservatories() {
  if (obsNeedInit) {
    obsNeedInit = False;
    Table t;
    ROTableRow row;
    TableRecord kws;
    String rfn[3] = {"Long", "Lat", "Height"};
    RORecordFieldPtr<Double> rfp[3];
    Double dt;
    String vs;	
    if (!MeasIERS::getTable(t, kws, row, rfp, vs, dt, 3, rfn, "Observatories",
		  "measures.observatory.directory",
		  "aips/Measures")) {
      LogIO os(LogOrigin("MeasTable",
			 String("initObservatories()"),
			 WHERE));
      os << "Cannot read table of Observatories" << LogIO::EXCEPTION;
    };
    Int N = t.nrow();
    if (N<1) {
     LogIO os(LogOrigin("MeasTable",
			String("initObservatories()"),
			WHERE));
      os << "No entries in table of Observatories" << LogIO::EXCEPTION;
    };
    obsNams.resize(N); obsNams.makePermanent();
    obsPos.resize(N); obsPos.makePermanent();
    MPosition::Ref mr;
    MPosition tmp;
    for (Int i=0; i<N; i++) {
      row.get(i);
      obsNams(i) = *RORecordFieldPtr<String>(row.record(), "Name");
      if (!tmp.giveMe(*RORecordFieldPtr<String>(row.record(), "Type"), mr)) {
	LogIO os(LogOrigin("MeasTable",
			   String("initObservatories()"),
			   WHERE));
	os << "Illegal position type in Observatories" << LogIO::EXCEPTION;
      };
      obsPos(i) = MPosition(MVPosition(Quantity(*(rfp[2]), "m"),
				       Quantity(*(rfp[0]), "deg"),
				       Quantity(*(rfp[1]), "deg")), mr);
    };
  };
}

const Vector<String> &MeasTable::Observatories() {
  MeasTable::initObservatories();
  return MeasTable::obsNams;
}

const Bool MeasTable::Observatory(MPosition &obs, const String &nam) {
  MeasTable::initObservatories();
  Int i=MUString::minimaxNC(nam, MeasTable::obsNams);
  if (i < MeasTable::obsNams.nelements()) {
    obs = MeasTable::obsPos(i);
    return True;
  };
  return False;
}

// Source data
void MeasTable::initSources() {
  if (srcNeedInit) {
    srcNeedInit = False;
    Table t;
    ROTableRow row;
    TableRecord kws;
    String rfn[2] = {"Long", "Lat"};
    RORecordFieldPtr<Double> rfp[2];
    Double dt;
    String vs;	
    if (!MeasIERS::getTable(t, kws, row, rfp, vs, dt, 2, rfn, "Sources",
		  "measures.sources.directory",
		  "aips/Measures")) {
      LogIO os(LogOrigin("MeasTable",
			 String("initSources()"),
			 WHERE));
      os << "Cannot read table of Sources" << LogIO::EXCEPTION;
    };
    Int N = t.nrow();
    if (N<1) {
     LogIO os(LogOrigin("MeasTable",
			String("initSources()"),
			WHERE));
      os << "No entries in table of Sources" << LogIO::EXCEPTION;
    };
    srcNams.resize(N); srcNams.makePermanent();
    srcPos.resize(N); srcPos.makePermanent();
    MDirection::Ref mr;
    MDirection tmp;
    for (Int i=0; i<N; i++) {
      row.get(i);
      srcNams(i) = *RORecordFieldPtr<String>(row.record(), "Name");
      if (!tmp.giveMe(*RORecordFieldPtr<String>(row.record(), "Type"), mr)) {
	LogIO os(LogOrigin("MeasTable",
			   String("initSources()"),
			   WHERE));
	os << "Illegal direction type in Sources" << LogIO::EXCEPTION;
      };
      srcPos(i) = MDirection(MVDirection(Quantity(*(rfp[0]), "deg"),
					 Quantity(*(rfp[1]), "deg")), mr);
    };
  };
}

const Vector<String> &MeasTable::Sources() {
  MeasTable::initSources();
  return MeasTable::srcNams;
}

const Bool MeasTable::Source(MDirection &obs, const String &nam) {
  MeasTable::initSources();
  Int i=MUString::minimaxNC(nam, MeasTable::srcNams);
  if (i < MeasTable::srcNams.nelements()) {
    obs = MeasTable::srcPos(i);
    return True;
  };
  return False;
}

// Magnetic field (IGRF) function
const Vector<Double> &MeasTable::IGRF(Double tm) {
  if (time0IGRF < 0 || (tm-time0IGRF > 1830 && time0IGRF < lastIGRF) ||
      (tm-time0IGRF < 0 && time0IGRF >= firstIGRF)) {
    Table t;
    TableRecord kws;
    ROTableRow row;
    String rfn[1] = {"MJD"};
    RORecordFieldPtr<Double> rfp[1];
    Double dt;
    String vs;	
    if (!MeasIERS::getTable(t, kws, row, rfp, vs, dt, 1, rfn, "IGRF",
		  "measures.igrf.directory",
		  "aips/Measures")) {
      LogIO os(LogOrigin("MeasTable",
			 String("IGRF(Double)"),
			 WHERE));
      os << "Cannot read table of IGRF models" << LogIO::EXCEPTION;
    };
    Int N = t.nrow();
    if (N<10 || !kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000 ||
	!kws.isDefined("dMJD") || kws.asDouble("dMJD") < 300) {
      LogIO os(LogOrigin("MeasTable",
			 String("IGRF(Double)"),
			 WHERE));
      os << "Incorrect entries in table of IGRF models" << LogIO::EXCEPTION;
    };
    Double m0 = kws.asDouble("MJD0");
    dtimeIGRF= kws.asDouble("dMJD");
    Int indx = Int((tm-m0)/dtimeIGRF);
    indx = max(1, min(indx, N)) - 1;
    row.get(0);
    firstIGRF = *(rfp[0]);
    row.get(N-1);
    lastIGRF = *(rfp[0]);
    row.get(indx);
    time0IGRF = *(rfp[0]);
    ROArrayColumn<Double> acc, accd;
    acc.attach(t, "COEF");
    accd.attach(t, "dCOEF");
    coefIGRF = acc(indx);
    dIGRF = accd(indx);
  };
  if (abs(tm-timeIGRF) > 5) {
    resIGRF = coefIGRF.ac() + dIGRF.ac() * (5*(tm-time0IGRF)/dtimeIGRF);
    timeIGRF = tm;
  };
  return resIGRF;
}

// Aberration function
const Polynomial<Double> &MeasTable::aberArg(uInt which) {
    static Bool needInit = True;
    static Polynomial<Double> polyArray[13];
    static const Double ABERFUND[13][2] = {
    {4.4026088,	2608.7903142},
    {3.1761467,	1021.3285546},
    {1.7534703,	 628.3075849},
    {6.2034809,	 334.0612431},
    {0.5995465,	  52.9690965},
    {0.8740168,	  21.3299095},
    {5.4812939,	   7.4781599},
    {5.3118863,	   3.8133036},
    {3.8103444,	8399.6847337},
    {5.1984667,	7771.3771486},
    {2.3555559,	8328.6914289},
    {6.2400601,	 628.3019553},
    {1.6279052,	8433.4661601}
};
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<13; i++) {
	    polyArray[i] = Polynomial<Double>(1);
	    for (j=0; j<2; j++) {
		polyArray[i].setCoefficient(j,
					    ABERFUND[i][j]);
	    }
	}
    }
    DebugAssert(which < 13, AipsError);
    return polyArray[which];
}

const Polynomial<Double> &MeasTable::aber1950Arg(uInt which) {
    static Bool needInit = True;
    static Polynomial<Double> polyArray[12];
    static const Double ABERFUND[12][4] = {
    {1065976.59, 1717915856.79, 33.09,   0.0518},
    {1290513.0,  129596579.1,   -0.54,   -0.0120},
    {40503.2,    1739527290.54, -11.56,  -0.0012},
    {1262654.95, 1602961611.18, -5.17,   0.0068},
    {933059.79,  -6962911.23,   7.48,    0.0080},
    {764820.00,	210662974.800,	0,	0},
    {1150495.2,	68903917.200,	0,	0},
    {811011.60,	10924498.800,	0,	0},
    {632145.60,	4398458.400,	0,	0},
    {0,		8128.800,	0,	0},
    {260701.20,	1542164.400,	0,	0},
    {135831.60,	786459.600,	0,	0}
    };
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<12; i++) {
	    polyArray[i] = Polynomial<Double>(3);
	    for (j=0; j<4; j++) {
		polyArray[i].setCoefficient(j,
					  ABERFUND[i][j]*C::arcsec);
	    }
	}
    }
    DebugAssert(which < 12, AipsError);
    return polyArray[which];
}

const Vector<Char> &MeasTable::mulAberArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[80];
    static const Char ABERARG[80][6] = {
{	0,	0,	1,	0,	0,	0},
{	0,	0,	2,	0,	0,	0},
{	0,	0,	3,	0,	0,	0},
{	0,	0,	2,	0,	-1,	0},
{	0,	0,	3,	-8,	3,	0},
{	0,	0,	5,	-8,	3,	0},
{	0,	1,	0,	0,	0,	0},
{	0,	2,	-1,	0,	0,	0},
{	0,	0,	1,	0,	-2,	0},
{	0,	0,	1,	0,	1,	0},
{	0,	2,	-2,	0,	0,	0},
{	0,	0,	1,	0,	-1,	0},
{	0,	0,	4,	0,	0,	0},
{	0,	0,	3,	0,	-2,	0},
{	0,	1,	-2,	0,	0,	0},
{	0,	2,	-3,	0,	0,	0},
{	0,	2,	-4,	0,	0,	0},
{	0,	0,	3,	-2,	0,	0},
{	0,	8,	-12,	0,	0,	0},
{	0,	8,	-14,	0,	0,	0},
{	0,	0,	0,	2,	0,	0},
{	0,	3,	-4,	0,	0,	0},
{	0,	0,	2,	0,	-2,	0},
{	0,	3,	-3,	0,	0,	0},
{	0,	0,	2,	-2,	0,	0},
{	0,	3,	-6,	0,	0,	0},
{	0,	0,	0,	0,	1,	0},
{	0,	0,	9,	-16,	4,	5},
{	0,	0,	7,	-16,	4,	5},
{	0,	0,	1,	0,	-3,	0},
{	0,	0,	2,	0,	-3,	0},
{	0,	4,	-5,	0,	0,	0},
{	0,	0,	1,	-4,	0,	0},
{	0,	0,	3,	0,	-3,	0},
{	0,	0,	3,	-4,	0,	0},
{	0,	3,	-2,	0,	0,	0},
{	0,	0,	4,	-4,	0,	0},
{	0,	0,	2,	0,	0,	-1},
{	0,	0,	3,	-3,	0,	0},
{	0,	0,	3,	0,	-1,	0}, //40
{	0,	0,	1,	0,	0,	1},
{	0,	0,	0,	0,	2,	0},
{	0,	0,	2,	-1,	0,	0},
{	0,	0,	1,	0,	0,	-1},
{	0,	5,	-6,	0,	0,	0},
{	0,	0,	1,	-3,	0,	0},
{	0,	3,	-6,	4,	0,	0},
{	0,	3,	-8,	4,	0,	0},
{	0,	0,	4,	-5,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	0,	3,	-5,	0,	0,	0},
{	0,	6,	-7,	0,	0,	0},
{	0,	10,	-9,	0,	0,	0},
{	0,	0,	2,	-8,	3,	0},
{	0,	0,	6,	-8,	3,	0},
{	0,	0,	1,	-2,	0,	0},
{	0,	0,	9,	-15,	0,	0},
{	0,	0,	1,	0,	-2,	5},
{	0,	0,	1,	0,	2,	-5},
{	0,	0,	1,	0,	0,	-2}, //60
{	0,	0,	0,	1,	0,	0},
{	0,	0,	7,	-15,	0,	0},
{	0,	2,	0,	0,	0,	0},
{	0,	0,	2,	0,	2,	-5},
{	2,	0,	-2,	0,	0,	0},
{	0,	0,	9,	-19,	0,	3},
{	0,	0,	11,	-19,	0,	3},
{	0,	0,	2,	-5,	0,	0},
{	0,	5,	-9,	0,	0,	0},
{	0,	11,	-10,	0,	0,	0},
{	0,	4,	-4,	0,	0,	0},
{	0,	0,	2,	0,	-4,	0},
{	0,	0,	5,	-6,	0,	0},
{	0,	5,	-5,	0,	0,	0},
{	0,	0,	4,	0,	-3,	0},
{	0,	4,	-6,	0,	0,	0},
{	0,	5,	-7,	0,	0,	0},
{	0,	0,	4,	0,	-2,	0},
{	0,	0,	3,	0,	-4,	0},
{	0,	7,	-8,	0,	0,	0}
};
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<80; i++) {
	    argArray[i].resize(6);
	    argArray[i].makePermanent();
	    for (j=0; j<6; j++) {
		argArray[i](j) = ABERARG[i][j];
	    }
	}
    }
    DebugAssert(which < 80, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulAber1950Arg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[132];
    static const Char ABERARG[132][12] = {
    { 0, 0, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0,-1,-1, 1,-1, 0, 0, 1, 0, 0, 0, 0},
    { 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    { 1, 0,-1,-2,-1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 1,-1, 1,-1,-1, 0, 0, 0, 0, 0, 0},
//10
    { 0, 4, 1,-1, 1, 0,-8, 3, 0, 0, 0, 0},
    { 0,-4, 1,-1, 1, 0, 8,-3, 0, 0, 0, 0},
    { 0, 2,-1, 1,-1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 2,-1, 1,-1, 0, 0,-2, 0, 0, 0, 0},
    { 0,-4, 1,-1, 1, 0, 8,-3, 0, 0, 0, 0},
    { 0, 4, 1,-1, 1, 0,-8, 3, 0, 0, 0, 0},
    { 0, 1,-1, 1,-1,-1, 0, 0, 0, 0, 0, 0},
    { 0, 0, 1,-1, 1, 0, 0, 1, 0, 0, 0, 0},
    { 0, 2,-1, 1,-1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0},
//20
    { 0,-3,-1, 1,-1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 0,-1, 1,-1, 0, 0, 1, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1, 0, 0,-2, 0, 0, 0, 0},
    { 0, 3,-1, 1,-1,-2, 0, 0, 0, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 2, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1,-1, 0, 0, 0, 0, 0, 0},
    { 0, 3,-1, 1,-1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1, 0,-2, 0, 0, 0, 0, 0},
    { 0,-1, 1,-1, 1, 0, 2, 0, 0, 0, 0, 0},
//30
    { 0, 3, 1,-1, 1,-2, 0, 0, 0, 0, 0, 0},
    { 0,-1,-1, 1,-1, 0, 0, 2, 0, 0, 0, 0},
    { 0, 4,-1, 1,-1,-3, 0, 0, 0, 0, 0, 0},
    { 0, 3, 1,-1, 1,-2, 0, 0, 0, 0, 0, 0},
    { 0,13, 1,-1, 1,-8, 0, 0, 0, 2, 0, 0},
    {0,-13, 1,-1, 1, 8, 0, 0, 0,-2, 0, 0},
    { 0,13, 1,-1, 1,-8, 0, 0, 0, 2, 0, 0},
    {0,-13, 1,-1, 1, 8, 0, 0, 0,-2, 0, 0},
    { 0, 0,-1, 2,-1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1,-2, 0, 0, 0, 0, 0, 0},
//40
    { 0, 1, 1,-1, 1,-1, 0, 0, 0, 0, 0, 0},
    { 0, 0,-1, 2, 0, 0, 0, 0, 0, 0, 0, 0},
    { 0, 3, 1,-1, 1,-3, 0, 0, 0, 0, 0, 0},
    { 0,-1, 1,-1, 1, 0, 0, 1, 0, 0, 0, 0},
    { 0, 0, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 5, 1,-1, 1,-3, 0, 0, 0, 0, 0, 0},
    { 0, 3,-1, 1,-1, 0, 0,-3, 0, 0, 0, 0},
    { 0,-5, 1,-1, 1, 3, 0, 0, 0, 0, 0, 0},
    { 0, 4, 1,-1, 1,-4, 0, 0, 0, 0, 0, 0},
    { 0, 4, 0, 0, 0,-3, 0, 0, 0, 0, 0, 0},
//50
    { 0, 5, 1,-1, 1,-3, 0, 0, 0, 0, 0, 0},
    { 0,-5, 1,-1, 1, 3, 0, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1, 0, 0,-2, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 3, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 0, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0,-1, 1,-1, 1, 0, 2, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1, 0,-2, 0, 0, 0, 0, 0},
    { 0, 3, 1,-1, 1, 0,-4, 0, 0, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 2, 0, 0, 0, 0, 0},
//60
    { 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    { 0,-1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 3, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 4, 0, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1, 0,-4, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1, 0, 0, 0,-1, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 0, 3, 0, 0, 0, 0},
    { 0, 3,-1, 1,-1,-3, 0, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 1, 0, 0, 0, 0},
    { 0, 4, 1,-1, 1,-4, 0, 0, 0, 0, 0, 0},
//70
    { 0, 0, 1,-1, 1, 0, 0, 1, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 2, 0, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 0, 1, 0, 0, 0, 0},
    { 0, 5, 1,-1, 1,-5, 0, 0, 0, 0, 0, 0},
    { 0,-1, 1,-1, 1, 0, 0, 2, 0, 0, 0, 0},
    { 0, 4,-1, 1,-1,-3, 0, 0, 0, 0, 0, 0},
    {-1, 0,-1, 2,-1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 5, 1,-1, 1,-4, 0, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 0, 2, 0, 0, 0},
    { 0, 0, 1,-1, 1, 0, 0, 0, 1, 0, 0, 0},
//80
    { 0, 0, 1,-1, 1, 0, 0, 0,-1, 0, 0, 0},
    { 0, 5,-1,-1, 1,-5, 0, 0, 0, 0, 0, 0},
    { 0, 6, 1,-1, 1,-6, 0, 0, 0, 0, 0, 0},
    { 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0,-7, 1,-1, 1, 3, 4, 0, 0, 0, 0, 0},
    { 0, 7, 1,-1, 1,-3,-4, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 4, 0, 0, 0, 0, 0},
    { 0, 2, 1,-1, 1, 0,-4, 0, 0, 0, 0, 0},
    { 0, 4, 1,-1, 1,-3, 0, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 3, 0, 0, 0, 0, 0},
//90
    { 0, 1, 1,-1, 1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 1,-1, 1,-1,-2, 0, 0, 0, 0, 0, 0},
    { 1, 0,-1, 2,-1, 0, 0, 0, 0, 0, 0, 0},
    { 0,-1, 1,-1, 1, 0, 0, 0, 1, 0, 0, 0},
    { 0,-3, 1,-1, 1, 0, 0, 2, 0, 0, 0, 0},
    { 0,-3,-1, 1,-1, 0, 0, 2, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 2, 0, 0, 0, 0, 0},
    { 0,-3, 1,-1, 1, 0, 3, 0, 0, 0, 0, 0},
    { 0,-3,-1, 1,-1, 0, 3, 0, 0, 0, 0, 0},
    { 0,-1, 1,-1, 1, 0, 0, 3, 0, 0, 0, 0},
//100
    { 0,-1,-1, 1,-1, 0, 0, 3, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 1,-1, 1,-1,-2, 0, 0, 0, 0, 0, 0},
    { 0, 7, 1,-1, 1,-5, 0, 0, 0, 0, 0, 0},
    { 0, 7,-1, 1,-1,-5, 0, 0, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 4, 0, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 0, 4, 0, 0, 0, 0},
    { 0,-2, 1,-1, 1, 0, 0, 3, 0, 0, 0, 0},
    { 0,-2,-1, 1,-1, 0, 0, 3, 0, 0, 0, 0},
//                  g2,g3,g4,g5, v, g6,g7
    { 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
//110
    { 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1},
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1},
    { 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0},
//120
    { 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0},
    { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0},
//130
    { 0, 0, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
    { 0, 1, 1,-1, 1, 0, 0, 0, 0, 0, 0, 0},
};
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<132; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<12; j++) {
		argArray[i](j) = ABERARG[i][j];
	    }
	}
    }
    DebugAssert(which < 132, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulAberSunArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[17];
    static const Char ABERSUNARG[17][7] = {
{	0,	0,	0,	1,	0,	0,	0},
{	0,	0,	0,	0,	1,	0,	0},
{	0,	0,	0,	2,	0,	0,	0},
{	0,	0,	0,	0,	0,	1,	0},
{	0,	0,	0,	0,	0,	0,	1},
{	0,	0,	0,	0,	2,	0,	0},
{	1,	0,	0,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0,	0},
{	0,	0,	0,	3,	0,	0,	0},
{	0,	0,	0,	1,	-5,	0,	0},
{	0,	0,	0,	3,	-5,	0,	0},
{	1,	0,	0,	0,	0,	0,	-2},
{	0,	0,	0,	0,	3,	0,	0},
{	0,	0,	0,	2,	-6,	0,	0},
{	0,	0,	0,	2,	-4,	0,	0},
{	0,	0,	0,	0,	0,	2,	0},
{	0,	0,	0,	1,	0,	0,	-2}
};
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<17; i++) {
	    argArray[i].resize(7);
	    argArray[i].makePermanent();
	    for (j=0; j<7; j++) {
		argArray[i](j) = ABERSUNARG[i][j];
	    }
	}
    }
    DebugAssert(which < 17, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulAberEarthArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[17];
    static const Char ABEREARTHARG[17][5] = {
{	1,	0,	0,	0,	0},
{	0,	0,	0,	0,	1},
{	1,	0,	1,	0,	0},
{	1,	2,	-1,	0,	0},
{	1,	-2,	0,	0,	0},
{	1,	2,	0,	0,	0},
{	0,	0,	1,	0,	1},
{	1,	-2,	1,	0,	0},
{	1,	0,	2,	0,	0},
{	0,	2,	0,	0,	-1},
{	1,	0,	0,	0,	-2},
{	1,	0,	0,	1,	0},
{	1,	0,	0,	-1,	0},
{	1,	4,	-2,	0,	0},
{	1,	-2,	2,	0,	0},
{	1,	2,	1,	0,	0},
{	0,	2,	-1,	0,	1}
};
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<17; i++) {
	    argArray[i].resize(5);
	    argArray[i].makePermanent();
	    for (j=0; j<5; j++) {
		argArray[i](j) = ABEREARTHARG[i][j];
	    }
	}
    }
    DebugAssert(which < 17, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulAber(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[80];
    static Polynomial<Double> polyArray[18];
    static Double factor = 0;
    static const Long MABERTD[3][18] = {
{ -1719919,	-2,	0,	-25,	0,	0,
	25,	-13,	-1, 1578094,	156,	0,
        10,	32,	1,   684187,	-358,	0},
{	6434,	141,	0,	28007,	-107,	-1,
	25697,	-95,	-1,	-5904,	-130,	0,
	11141,	-48,	0,	-2559,	-55,	0},
{	486,	-5,	0,	-236,	-4,	0,
	-216,	-4,	0,	-446,	5,	0,
	-94,	-2,	0,	-193,	2,	0}
};
    static const Short MABER[80][6] = {
{	0,	0,	0,	0,	0,	0},
{	0,	0,	0,	0,	0,	0},
{	0,	0,	0,	0,	0,	0},
{	31,	1,	1,	-28,	0,	-12},
{	8,	-28,	25,	8,	11,	3},
{	8,	-28,	-25,	-8,	-11,	-3},
{	-25,	0,	0,	23,	0,	10},
{	21,	0,	0,	-19,	0,	-8},
{	16,	0,	0,	15,	1,	7},
{	11,	-1,	-1,	-10,	-1,	-5},
{	0,	-11,	-10,	0,	-4,	0},
{	-11,	-2,	-2,	9,	-1,	4},
{	-7,	-8,	-8,	6,	-3,	3},
{	-10,	0,	0,	9,	0,	4},
{	-9,	0,	0,	-9,	0,	-4},
{	-9,	0,	0,	-8,	0,	-4},
{	0,	-9,	8,	0,	3,	0},
{	8,	0,	0,	-8,	0,	-3},
{	-4,	-7,	-6,	4,	-3,	2},
{	-4,	-7,	6,	-4,	3,	-2},
// 21:
{	-6,	-5,	-4,	5,	-2,	2},
{	-1,	-1,	-2,	-7,	1,	-4},
{	4,	-6,	-5,	-4,	-2,	-2},
{	0,	-7,	-6,	0,	-3,	0},
{	5,	-5,	-4,	-5,	-2,	-2},
{	4,	-1,	1,	4,	0,	2},
{	-4,	0,	0,	3,	0,	1},
{	-1,	-3,	-3,	1,	-1,	0},
{	-1,	-3,	3,	-1,	1,	0},
{	3,	1,	0,	3,	0,	1},
{	3,	-1,	-1,	1,	0,	1},
{	-2,	0,	0,	-3,	0,	-1},
{	1,	-2,	2,	1,	1,	1},
{	-2,	-1,	0,	2,	0,	1},
{	1,	-2,	-2,	-1,	-1,	0},
{	2,	0,	0,	-2,	0,	-1},
{	2,	-1,	-1,	-2,	0,	-1},
{	2,	0,	0,	-2,	0,	-1},
{	2,	-1,	-1,	-1,	0,	-1},
{	0,	-2,	-1,	0,	-1,	0},
// 41:
{	0,	-1,	-1,	0,	-1,	0},
{	-1,	-1,	-1,	1,	-1,	0},
{	1,	0,	0,	-1,	0,	-1},
{	0,	-1,	-1,	0,	-1,	0},
{	-2,	0,	0,	-1,	0,	0},
{	1,	-1,	1,	1,	0,	0},
{	-1,	1,	1,	1,	0,	0},
{	-1,	1,	-1,	-1,	0,	0},
{	1,	-1,	-1,	0,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	0,	-1,	1,	0,	0,	0},
{	-1,	0,	0,	-1,	0,	0},
{	1,	0,	0,	-1,	0,	0},
{	1,	0,	0,	1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
{	1,	0,	0,	1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
{	1,	0,	0,	-1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
{	1,	0,	0,	1,	0,	0},
// 61:
{	-1,	0,	0,	1,	0,	0},
{	-1,	0,	0,	-1,	0,	0},
{	0,	-1,	-1,	0,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	0,	1,	-1,	0,	0,	0},
{	0,	1,	-1,	0,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	0,	-1,	1,	0,	0,	0},
{	0,	1,	-1,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	0,	-1,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	0,	-1,	0,	0,	0,	0},
{	0,	0,	1,	0,	0,	0},
{	0,	0,	1,	0,	0,	0},
{	0,	0,	0,	1,	0,	0},
{	0,	0,	0,	-1,	0,	0}
};
    if (needInit) {
	needInit = False;
	UnitVal AUperDay(1e-8,"AU/d");
	factor = AUperDay.getFac();
	Int i,j,k;
	for (i=0; i<3; i++) {
	    for (j=0; j<6; j++) {
		polyArray[6*i+j] = Polynomial<Double>(2);
		for (k=0; k<3; k++) {
		    polyArray[6*i+j].setCoefficient(k,
				      MABERTD[i][k+3*j]*factor);
		}
	    }
	}
	for (i=0; i<80; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<6; j++) {
		argArray[i](j) = MABER[i][j] * factor;
	    }
	    for (j=6; j<12; j++) {
		argArray[i](j) = 0;
	    }
	}
    }
    if (checkT != T) {
	checkT = T;
	Int i, j;
	for (i=0; i<3; i++) {	// get fundamental argument coefficients
	    for (j=0; j<6; j++) {
		argArray[i](j) = polyArray[6*i+j](T);
		argArray[i](j+6) = (polyArray[6*i+j].derivative())(T);
	    }
	}
    }
    DebugAssert(which < 80, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulAber1950(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[132];
    static Double factor = 0;
    static const Short MABER[130][6] = {
// Order: sin(x), cos(x), sin(y), cos(y), sin(z), cos(z)
{	1,	0,	0,	-157,	0,	358},
{	715,	0,	0,	-656,	0,	-285},
{	543,	0,	0,	-498,	0,	-216},
{	-72,	0,	0,	63,	0,	35},
{	-60,	0,	0,	55,	0,	24},
{	38,	0,	0,	-35,	0,	-15},
{	0,	-31,	28,	0,	12,	0},
{	0,	0,	0,	26,	0,	-59},
{	-26,	0,	0,	-24,	0,	-10},
{	-22,	0,	0,	-20,	0,	-9},
//10
{	22,	0,	0,	-20,	0,	-9},
{	-22,	0,	0,	20,	0,	9},
{	0,	-18,	17,	0,	7,	0},
{	16,	0,	0,	15,	0,	6},
{	0,	16,	14,	0,	6,	0},
{	0,	16,	14,	0,	6,	0},
{	0,	12,	-1,	0,	-5,	0},
{	-12,	0,	0,	11,	0,	5},
{	11,	0,	0,	10,	0,	4},
{	11,	0,	0,	-10,	0,	-4},
//20
{	-11,	0,	0,	-10,	0,	-4},
{	-10,	0,	0,	-9,	0,	-4},
{	-10,	0,	0,	9,	0,	4},
{	0,	0,	8,	-8,	0,	-3},
{	0,	0,	8,	-8,	0,	-3},
{	-8,	0,	0,	7,	0,	3},
{	-8,	0,	0,	-7,	0,	-3},
{	0,	8,	7,	0,	3,	0},
{	0,	-7,	-6,	0,	-3,	0},
{	0,	-7,	-6,	0,	-3,	0},
//30
{	0,	7,	6,	0,	3,	0},
{	7,	0,	0,	6,	0,	3},
{	0,	6,	-6,	0,	-3,	0},
{	-6,	0,	6,	0,	3,	0},
{	6,	0,	0,	-5,	0,	-2},
{	-6,	0,	0,	5,	0,	2},
{	0,	5,	5,	0,	2,	0},
{	0,	5,	5,	0,	2,	0},
{	-5,	0,	0,	-5,	0,	-2},
{	-5,	0,	0,	4,	0,	2},
//40
{	0,	5,	4,	0,	2,	0},
{	0,	0,	0,	0,	0,	-2},
{	0,	4,	4,	0,	2,	0},
{	0,	-4,	-3,	0,	-1,	0},
{	0,	-4,	-3,	0,	-1,	0},
{	0,	3,	3,	0,	1,	0},
{	0,	3,	-3,	0,	-1,	0},
{	0,	3,	3,	0,	1,	0},
{	0,	3,	3,	0,	1,	0},
{	0,	0,	0,	0,	-1,	0},
//50
{	-3,	0,	0,	3,	0,	1},
{	3,	0,	0,	-3,	0,	-1},
{	0,	-3,	-3,	0,	-1,	0},
{	-3,	0,	0,	3,	0,	1},
{	-3,	0,	0,	2,	0,	1},
{	0,	-3,	2,	0,	1,	0},
{	-3,	0,	0,	2,	0,	1},
{	3,	0,	0,	-2,	0,	-1},
{	-3,	0,	0,	2,	0,	1},
{	-2,	0,	0,	-2,	0,	0},
//60
{	0,	0,	0,	1,	0,	-3},
{	0,	0,	0,	0,	0,	1},
{	0,	2,	-2,	0,	0,	0},
{	0,	2,	2,	0,	0,	0},
{	0,	2,	2,	0,	0,	0},
{	-2,	0,	0,	2,	0,	0},
{	2,	0,	0,	2,	0,	0},
{	0,	-2,	2,	0,	0,	0},
{	0,	2,	2,	0,	0,	0},
{	2,	0,	0,	-2,	0,	0},
//70
{	0,	-2,	-2,	0,	0,	0},
{	0,	-2,	-2,	0,	0,	0},
{	0,	-2,	2,	0,	0,	0},
{	2,	0,	0,	-2,	0,	0},
{	2,	0,	0,	-2,	0,	0},
{	-2,	0,	0,	-2,	0,	0},
{	2,	0,	0,	1,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	1,	0,	0,	-1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
//80
{	1,	0,	0,	-1,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	1,	0,	0,	-1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
{	0,	-1,	-1,	0,	0,	0},
{	0,	-1,	-1,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
//90
{	0,	1,	0,	0,	0,	0},
{	0,	-1,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
//100
{	1,	0,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
{	0,	-1,	0,	0,	0,	0},
{	-1,	0,	0,	0,	0,	0},
{	1,	0,	0,	0,	0,	0},
{	0,	-1,	0,	0,	0,	0},
{	0,	1,	0,	0,	0,	0},
//
{	701,	0,	0,	-642,	0,	-280},
//110
{	0,	158,	152,	0,	48,	0},
{	0,	159,	147,	0,	61,	0},
{	34,	0,	0,	-31,	0,	-14},
{	0,	20,	18,	0,	8,	0},
{	-17,	0,	0,	16,	0,	7},
{	0,	12,	11,	0,	4,	0},
{	11,	0,	0,	-10,	0,	-4},
{	0,	9,	8,	0,	3,	0},
{	0,	8,	7,	2,	0,	0},
{	-5,	0,	0,	5,	0,	2},
//120
{	-5,	0,	0,	4,	0,	2},
{	0,	4,	3,	2,	0,	0},
{	-3,	0,	0,	3,	0,	1},
{	0,	3,	2,	0,	1,	0},
{	-3,	0,	0,	5,	0,	-5},
{	2,	0,	0,	-2,	0,	-1},
{	-1,	0,	0,	0,	0,	1},
{	0,	1,	1,	0,	0,	0},
{	0,	1,	1,	0,	0,	0},
{	0,	-1,	0,	0,	0,	0},
//130
};
    static const Short ABERT1T[10] = {
    0,3,44,54,55,113,119,120,128,129
};
    static const Short ABERT2T[2] = {
    44,55
};
    static const Short ABERT3T[1] = {
    55
};
    static const Double ABERSPEC[2][6] = {
    {1719971.0,	0,	0,	-1577888.0,	0,	-684523.0},
    {28809.0,	0,	0,	-26429.0,	0,	-11466.0}
};

    if (needInit) {
	needInit = False;
	UnitVal AUperDay(1e-8,"AU/d");
	factor = AUperDay.getFac();
	Int i, j;
	for (i=0; i<130; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<6; j++) {
		argArray[i](j) = MABER[i][j] * factor;
	    }
	    for (j=6; j<12; j++) {
		argArray[i](j) = 0;
	    }
	}
	for (i=0; i<2; i++) {
	    argArray[130+i].resize(12);
	    for (j=0; j<6; j++) {
		argArray[130+i](j) = ABERSPEC[i][j] * factor;
	    }
	    for (j=6; j<12; j++) {
		argArray[130+i](j) = 0;
	    }
	}
    }
    if (checkT != T) {
	checkT = T;
	Int i, j, k;
	for (i=0; i<10; i++) {	// get fundamental argument coefficients
	    k = ABERT1T[i];
	    for (j=0; j<6; j++) {
		argArray[k](j) = MABER[k][j] * factor * T;
		argArray[k](j+6) = MABER[k][j] * factor;
	    }
	}
	for (i=0; i<2; i++) {	// get fundamental argument coefficients
	    k = ABERT2T[i];
	    for (j=0; j<6; j++) {
		argArray[k](j) *= T;
		argArray[k](j+6) *= 2*T;
	    }
	}
	for (i=0; i<1; i++) {	// get fundamental argument coefficients
	    k = ABERT3T[i];
	    for (j=0; j<6; j++) {
		argArray[k](j) *= T;
		argArray[k](j+6) *= 1.5*T;
	    }
	}
    }
    DebugAssert(which < 132, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulSunAber(uInt which) {
    static Bool needInit = True;
    static Vector<Double> argArray[17];
    static Double factor = 0;
    static const Short MSUNABER[17][6] = {
{	719,	0,	6,	-660,	-15,	-283},
{	159,	0,	2,	-147,	-6,	-61},
{	34,	-9,	-8,	-31,	-4,	-13},
{	17,	0,	0,	-16,	0,	-7},
{	16,	0,	1,	-15,	-3,	-6},
{	0,	-9,	-8,	0,	-3,	1},
{	6,	0,	0,	-6,	0,	-2},
{	5,	0,	0,	-5,	0,	-2},
{	2,	-1,	-1,	-2,	0,	-1},
{	-2,	0,	0,	-2,	0,	-1},
{	-2,	0,	0,	2,	0,	1},
{	-1,	0,	0,	-1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
{	1,	0,	0,	1,	0,	0},
{	1,	0,	0,	-1,	0,	0},
{	-1,	0,	0,	1,	0,	0},
{	1,	0,	0,	0,	0,	0}
};
    if (needInit) {
	needInit = False;
	UnitVal AUperDay(1e-8,"AU/d");
	factor = AUperDay.getFac();
	Int i,j;
	for (i=0; i<17; i++) {
	    argArray[i].resize(6);
	    argArray[i].makePermanent();
	    for (j=0; j<6; j++) {
		argArray[i](j) = MSUNABER[i][j] * factor;
	    }
	}
    }
    DebugAssert(which < 17, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulEarthAber(uInt which) {
    static Bool needInit = True;
    static Vector<Double> argArray[17];
    static Double factor = 0;
    static const Short MEARTHABER[17][3] = {
{	715,	-656,	-285},
{	0,	26,	-59},
{	39,	-36,	-16},
{	8,	-7,	-3},
{	5,	-5,	-2},
{	4,	-4,	-2},
{	0,	1,	-3},
{	-2,	2,	1},
{	2,	-2,	-1},
{	0,	1,	-2},
{	-1,	1,	1},
{	-1,	1,	0},
{	1,	-1,	0},
{	1,	-1,	0},
{	-1,	1,	0},
{	1,	0,	0},
{	0,	0,	-1}
};
    if (needInit) {
	needInit = False;
	UnitVal AUperDay(1e-8,"AU/d");
	factor = AUperDay.getFac();
	Int i,j;
	for (i=0; i<17; i++) {
	    argArray[i].resize(3);
	    argArray[i].makePermanent();
	    for (j=0; j<3; j++) {
		argArray[i](j) = MEARTHABER[i][j] * factor;
	    }
	}
    }
    DebugAssert(which < 17, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::AberETerm(uInt which) {
    static Bool needInit = True;
    static Vector<Double> termArray[2];
    static const Double TERM[2][3] = {
        -1.62557,	-0.31919,	-0.13843,
        +1.245,		-1.580,		-0.659
    };
    if (needInit) {
        needInit = False;
	Int i;
	for (i=0; i<2; i++) {
	    termArray[i].resize(3);
	    termArray[i].makePermanent();
        }; 
	for (i=0; i<3; i++) {
	    termArray[0](i) = TERM[0][i] * 1e-6;
	};
	for (i=0; i<3; i++) {
	    termArray[1](i) = TERM[1][i] * 1e-3;
	};
    };
    DebugAssert(which < 2, AipsError);
    return termArray[which];
}

// Diurnal Aberration factor
Double MeasTable::diurnalAber(Double radius, Double T) {
    static Double res;
    res = C::_2pi * radius / MeasData::SECinDAY /
	MeasTable::UTtoST(T)/C::c;
    return res;
}

// LSR velocity (kinematical)
const Vector<Double> &MeasTable::velocityLSRK(uInt which) {
    static Bool needInit = True;
    static Vector<Double> argArray[2];
    static const Double LSR[2][3] = {
        0.0145021, -0.865863, 0.500071,
        0.00724658, -0.865985, 0.500018
    };
    if (needInit) {
        needInit = False;
	Double v = 19.5*1000.;
	for (Int i=0; i<2; i++) {
	    argArray[i].resize(3);
	    argArray[i].makePermanent();
	    for (Int j=0; j<3; j++) {
		argArray[i](j) = v * LSR[i][j];
	    };
	};
    };
    DebugAssert(which < 2, AipsError);
    return argArray[which];
}
// LSR velocity (dynamical)
const Vector<Double> &MeasTable::velocityLSR(uInt which) {
    static Bool needInit = True;
    static Vector<Double> argArray[2];
    static const Double LSR[2][3] = {
        -0.0385568, -0.881138, 0.471285,
        -0.0461164, -0.880664, 0.471491
    };
    if (needInit) {
        needInit = False;
	Double v = sqrt(81.+144.+49.)*1000.;
	for (Int i=0; i<2; i++) {
	    argArray[i].resize(3);
	    argArray[i].makePermanent();
	    for (Int j=0; j<3; j++) {
		argArray[i](j) = v * LSR[i][j];
	    };
	};
    };
    DebugAssert(which < 2, AipsError);
    return argArray[which];
}
 
// LSR velocity wrt galctic centre
const Vector<Double> &MeasTable::velocityLSRGal(uInt which) {
    static Bool needInit = True;
    static Vector<Double> argArray[2];
    static const Double LSR[2][3] = {
        0.494109, -0.44483, 0.746982,
        0.492728, -0.450347, 0.744585
    };
    if (needInit) {
        needInit = False;
	Double v = 220.*1000.;
	for (Int i=0; i<2; i++) {
	    argArray[i].resize(3);
	    argArray[i].makePermanent();
	    for (Int j=0; j<3; j++) {
		argArray[i](j) = v * LSR[i][j];
	    };
	};
    };
    DebugAssert(which < 2, AipsError);
    return argArray[which];
}
    

// Earth and Sun position
const Polynomial<Double> &MeasTable::posArg(uInt which) { 
    static Bool needInit = True;
    static Polynomial<Double> polyArray[12];
    static const Double POSFUND[12][2] = {
 252.25,	149472.67,			//Q
 181.9798,	 58517.8157,			//V
 100.46644851,	 35999.37285186,		//E
 355.43327,	 19140.29933,			//M
  34.351484,	  3034.905675,			//J
  50.077471,	  1222.113794,			//S
 314.055005,	   428.466998,			//U
 304.348665,	   218.486200,			//N
 238.47,	   145.28,			//P
 297.850206,	445267.111519,			//D
  93.27210,	483202.01753,			//F
 134.9634,	477198.8676			//l
};
    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<12; i++) {
	    polyArray[i] = Polynomial<Double>(1);
	    for (j=0; j<2; j++) {
		polyArray[i].setCoefficient(j,
					  POSFUND[i][j]*C::degree);
	    }
	}
    }
    DebugAssert(which < 12, AipsError);
    return polyArray[which];
}

const Vector<Char> &MeasTable::mulPosEarthXYArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[189];
    static const Char POSXYARG[189][12] = {
//X,Y(ecliptic) factors
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  1,  0,  0,
0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5, -8,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -8,  3,  0,  0,  0,  0,  0,  0,  0,
0,  1, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//11:
0,  0,  1,  0, -2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0, -1,  0,  0,  0,  0,  0,  0,  0,
0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1, -2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  2, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//21:
0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  8,-14,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  8,-12,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  1,  0, -1,
0,  0,  3, -2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -6,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -3,  0,  0,  0,  0,  0,  0,  0,
//31:
0,  0,  2,  0, -2,  0,  0,  0,  0,  0,  0,  0,
0,  3, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0, -2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  9,-16,  4,  5,  0,  0,  0,  0,  0,  0,
0,  0,  7,-16,  4,  5,  0,  0,  0,  0,  0,  0,
0,  0,  2, -4,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -4,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
0,  0,  1, -4,  0,  0,  0,  0,  0,  0,  0,  0,
//41:
0,  0,  1, -3,  0,  0,  0,  0,  0,  0,  0,  0,
0,  4, -5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  1,  0,  1,
0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0, -3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -3,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4, -4,  0,  0,  0,  0,  0,  0,  0,  0,
//51:
0,  3, -8,  4,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -6,  4,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -5,  0,  0,  0,  0,  0,  0,
0,  0,  2, -5,  0,  0,  0,  0,  0,  0,  0,  0,
0,  4, -6,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -6,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  0, -1,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -2,  5,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0, -1,  0,  1,
0,  0,  1,  0,  2, -5,  0,  0,  0,  0,  0,  0,
//61:
0,  0,  7,-15,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,
0,  5, -8,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  9,-15,  0,  0,  0,  0,  0,  0,  0,  0,
0,  4, -7,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0, -2,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0, -3,  0,  0,  0,  0,  0,  0,  0,
0,  5, -6,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  5, -9,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//71:
0,  0,  4, -5,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,
0,  5, -7,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0, 11,-19,  0,  3,  0,  0,  0,  0,  0,  0,
0,  0,  9,-19,  0,  3,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -4,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -5,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -7,  0,  0,  0,  0,  0,  0,  0,  0,
0,  1, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//81:
0,  0,  4, -6,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -8,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  6, -8,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0, -1,  0,  0,
0,  8,-13,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0, -1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  2, -5,  0,  0,  0,  0,  0,  0,
0,  0,  5, -7,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5, -6,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -6,  0,  0,  0,  0,  0,  0,  0,  0,
//91:
0,  0,  2,  0, -4,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  3,  0, -1,
0,  6, -7,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4, -7,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4, -8,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4, -9,  0,  0,  0,  0,  0,  0,  0,  0,
0,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1, -1,  0,  0,  0,  0,  0,  0,  0,  0,
//101:
0,  1, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  0, -2,  0,  0,  0,  0,  0,  0,
0,  0,  6, -9,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4, -3,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -8,  1,  5,  0,  0,  0,  0,  0,  0,
0,  0,  5, -8,  1,  5,  0,  0,  0,  0,  0,  0,
0,  4, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5,-11,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  6,-13,  0,  0,  0,  0,  0,  0,  0,  0,
//111:
0,  2, -5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -6,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5, -9,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  8,-13,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -4,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0, -4,  0,  0,  0,  0,  0,  0,  0,
0,  7, -8,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  7,-11,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0, -3,  0,  0,  0,  0,  0,  0,
//121:
0,  0,  4, -6,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -6,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5, -5,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4,  0,  0,  0,  0,  0,  0,  0,
0,  4, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  6, -8,  0,  0,  0,  0,  0,  0,  0,  0,
0,  8,-15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0,  0, -2,  0,  0,  0,  0,  0,  0,
0,  0,  5, -8,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0, 10,-17,  0,  0,  0,  0,  0,  0,  0,  0,
//131:
0,  0,  4,  0, -3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -5,  0,  0,  0,  0,  0,  0,
0,  6,-11,  0,  3,  0,  0,  0,  0,  0,  0,  0,
0,  6, -9,  0,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  8,-17,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -5,  0,  0,  0,  0,  0,  0,
0,  0,  8,-15,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -2,  0,  0,  0,  0,  0,  0,
0,  6, -8,  0,  0,  0,  0,  0,  0,  0,  0,  0,
1,  0, -5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//141:
0,  0,  4,  0, -2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5,-10,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0, -5,  0,  0,  0,  0,  0,  0,
0,  0,  6,-11,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -2,  2,  0,  0,  0,  0,  0,  0,
1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  4, -2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  5, -7, -4,  0,  0,  0,  0,  0,  0,  0,  0,
0,  5, -5, -4,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  1, -2,  0,
//151:
0,  0,  1,  0, -4,  5,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  3,  0,  0,
0,  0,  1,  2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,
1,  0, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
1,  0, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  2, -2,  0,  0,  0,  0,  0,  0,
0,  6,-10,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -1,  2,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  1, -2,  0,  0,  0,  0,  0,  0,
//161:
0,  8, -9,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,
0,  0,  6, -7,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  0,  0,  0,  0,  0,  1,  0,  0,
0,  0,  1,  0, -3,  5,  0,  0,  0,  0,  0,  0,
0,  0,  7,-13,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  3, -5,  0,  0,  0,  0,  0,  0,
0,  0,  3, -8,  0,  0,  0,  0,  0,  0,  0,  0,
0,  6, -9,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  2,  0,  0,  0,  0,  0,  0,
//171:
0,  0,  1,  0, -5,  0,  0,  0,  0,  0,  0,  0,
0,  3, -7,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  2,  0, -2,  0,  0,  0,  0,  0,  0,
0,  0,  2, -2,  0,  2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  5, -8,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  3, -8,  3,  0,  0,  0,  0,  0,  0,  0,
//181:
0,  2, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  8,-14,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  8,-12,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<189; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<12; j++) {
		argArray[i](j) = POSXYARG[i][j];
	    }
	}
    }
    DebugAssert(which < 189, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulPosEarthZArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[32];
    static const Char POSZARG[32][12] = {
//Z(ecliptic) factors
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,
0,  3, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0, -2,  0,  0,  0,  0,  0,  0,  0,
0,  1, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1, -1,
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0, -2,  0,  0,  0,  0,  0,  0,
0,  4, -5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//11:
0,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  2, -1,  0,
0,  0,  1,  0, -3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  1, -1,  0,
0,  0,  1,  0, -1,  0,  0,  0,  0,  0,  0,  0,
0,  5, -7,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,
0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  2, -5,  0,  0,  0,  0,  0,  0,
//21:
0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  3, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  2, -4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  8,-12,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,
0,  1, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  5, -6,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
//31:
0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<32; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<12; j++) {
		argArray[i](j) = POSZARG[i][j];
	    }
	}
    }
    DebugAssert(which < 32, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulPosSunXYArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[98];
    static const Char POSXYARG[98][12] = {
//X,Y(ecliptic) factors
0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -6,  0,  0,  0,  0,  0,  0,
//11:
0,  0,  0,  0,  3, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1, -1,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1, -3,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2, -2,  0,  0,  0,  0,
0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,
//21:
0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -1,  0,  0,  0,  0,  0,  0,
0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1, -2,  0,  0,  0,  0,
0,  0,  0,  0,  2, -2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -7,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4, -5,  0,  0,  0,  0,  0,  0,
//31:
0,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  3, -2,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2, -3,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1, -3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1, -2,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,
//41:
0,  0,  0,  0,  0,  0,  1,  2,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2, -5,  0,  0,  0,  0,
0,  0,  0,  0,  4,-11,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1, -4,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  3, -4,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2, -4,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1, -4,  0,  0,  0,  0,
0,  0,  0,  0,  2, -7,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -5,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  2, -3,  0,  0,  0,  0,  0,
//51:
0,  0,  0,  0,  4,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1, -1,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1,  1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  5,-10,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -6,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -6,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4, -9,  0,  0,  0,  0,  0,  0,
//61:
0,  0,  0,  0,  3,-10,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4, -2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4,-10,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  5, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4, -4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4,-12,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  3, -3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  4,-10,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1,  2,  0,  0,  0,  0,  0,  0,
//71:
0,  0,  0,  0,  3, -8,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -7,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -8,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -6,  2,  0,  0,  0,  0,  0,
1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2, -1,  0,  0,  0,  0,
0,  0,  0,  0,  2, -6,  4,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  4, -2,  0,  0,  0,  0,
0,  0,  0,  0,  5,-11,  3,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3,-11,  3,  0,  0,  0,  0,  0,
//81:
0,  0,  0,  0,  0,  0,  0,  3,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  4, -4,  0,  0,  0,  0,
0,  0,  0,  0,  3, -1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  2, -2,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -6,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -5,  0,  0,  0,  0,  0,  0,
//91:
0,  0,  0,  0,  1, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -7,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0
};

    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<98; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<12; j++) {
		argArray[i](j) = POSXYARG[i][j];
	    }
	}
    }
    DebugAssert(which < 98, AipsError);
    return argArray[which];
}

const Vector<Char> &MeasTable::mulPosSunZArg(uInt which) {
    static Bool needInit = True;
    static Vector<Char> argArray[29];
    static const Char POSZARG[29][12] = {
//Z(ecliptic) factors
0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -4,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -6,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3, -5,  0,  0,  0,  0,  0,  0,
//11:
0,  0,  0,  0,  1, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1, -1,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  1, -3,  0,  0,  0,  0,
0,  0,  0,  0,  2, -5,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,
0,  0,  0,  0,  1, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -3,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1, -1,  0,  0,  0,  0,  0,  0,
//21:
0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,
0,  0,  0,  0,  0,  0,  2, -2,  0,  0,  0,  0,
0,  0,  0,  0,  1, -2,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  2, -7,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

    if (needInit) {
	needInit = False;
	Int i,j;
	for (i=0; i<29; i++) {
	    argArray[i].resize(12);
	    argArray[i].makePermanent();
	    for (j=0; j<12; j++) {
		argArray[i](j) = POSZARG[i][j];
	    }
	}
    }
    DebugAssert(which < 29, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulPosEarthXY(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[189];
    static Double factor = 0;
    static Double fac1 = 0;
    static const Double MPOSXY[189][4] = {
// X,Y Heliocentric Earth, ecliptic
 90.00087234,	.9998292882e10,	359.99912749,	.9998921102e10,
 90.00000000,	  56114420,	270.0000000,	 244269903,
347.0626587,	  83525730,	257.0614968,	  83529232,
244.12566,	   1046663,	154.12489,	   1046697,
 90.0000,	    311084,	  0.0000,	    311084,
 89.0578,	    255249,	359.3759,	    257033,
 90.0271,	    213728,	179.9916,	    214746,
196.7828,	    168118,	106.7816,	    168129,
 16.7819,	    167995,	106.7836,	    168006,
269.9806,	    144524,	  0.0522,	    144026,
//11:
269.6489,	109101,	  0.3652,	113511,
271.4276,	 93443,	181.4222,	 93454,
 89.9756,	 89914,	  0.0162,	 90056,
 90.8784,	 73446,	179.5547,	 74492,
  0.8110,	 68144,	 90.8196,	 68133,
 98.7707,	 68441,	  9.3357,	 63930,
263.7081,	 61124,	173.6982,	 61135,
144.633,	 56652,	 54.610,	 56711,
270.242,	 54701,	180.200,	 54635,
  1.314,	 54095,	 91.322,	 54125,
//21:
180.963,	52049,	 91.427,	50708,
327.392,	45226,	 57.394,	45229,
228.731,	45184,	139.361,	45042,
147.393,	44981,	 57.415,	45025,
130.279,	40626,	 40.291,	40632,
151.268,	 7729,	172.890,	55138,
269.997,	25618,	180.003,	25613,
269.273,	25582,	179.260,	25583,
 75.848,	22789,	165.848,	22791,
280.086,	22588,	 10.214,	22779,
//31:
215.722,	21496,	126.916,	21950,
180.759,	20902,	 90.897,	20623,
 90.619,	19997,	  0.625,	20002,
162.681,	18226,	 72.680,	18227,
342.679,	18226,	 72.681,	18227,
 61.086,	17811,	150.582,	17925,
214.749,	16119,	118.365,	14976,
141.189,	15545,	 51.188,	15545,
 89.725,	15170,	359.704,	15278,
 32.362,	12894,	122.345,	12898,
//41:
 60.693,	12810,	150.613,	12823,
 95.556,	 6774,	176.881,	11874,
 90.000,	 8587,	  0.000,	 8587,
 89.374,	 8296,	359.942,	 8349,
182.488,	 7920,	 92.452,	 8073,
255.788,	 9449,	 23.725,	 5809,
240.578,	 7780,	151.084,	7841,
177.781,	 7560,	 87.769,	 7562,
141.375,	 7348,	 51.367,	 7352,
239.074,	 6535,	149.568,	 6569,
//51:
237.645,	6324,	327.648,	6324,
 57.641,	6298,	327.645,	6297,
156.246,	6213,	243.587,	6363,
 31.449,	6004,	121.422,	6008,
182.297,	5734,	271.031,	6113,
 31.69,		5371,	121.63,		5375,
270.56,		5131,	180.56,		5130,
243.06,		5109,	153.05,		5110,
269.99,		5065,	180.01,		5063,
117.64,		5049,	 27.28,		5064,
//61:
246.74,	4962,	336.74,	4962,
270.52,	4896,	180.48,	4893,
270.98,	4727,	358.98,	4934,
257.23,	4811,	347.17,	4842,
 67.00,	4808,	336.61,	4791,
 75.42,	4735,	165.45,	4734,
270.82,	4757,	359.00,	3972,
100.50,	4339,	  9.99,	4214,
 86.90,	5232,	186.38,	2571,
161.82,	3954,	251.82,	3955,
//71:
215.93,	4114,	116.98,	3753,
164.09,	3799,	 74.07,	3795,
327.42,	2048,	258.18,	4839,
271.81,	3688,	181.44,	3638,
341.29,	3612,	251.28,	3612,
161.29,	3611,	251.28,	3612,
290.01,	3255,	 20.06,	3260,
238.87,	3256,	332.92,	3114,
  2.39,	3034,	 92.35,	3035,
 54.87,	3248,	350.61,	2674,
//81:
193.66,	3255,	 78.16,	2484,
119.77,	2807,	209.76,	2807,
 95.54,	2772,	  5.54,	2773,
269.94,	2611,	180.06,	2600,
 65.10,	2493,	155.06,	2504,
177.63,	2325,	 87.61,	2323,
336.33,	2164,	246.33,	2164,
188.68,	2286,	 85.72,	2001,
213.94,	2187,	117.66,	2051,
  3.91,	2106,	 93.92,	2107,
//91:
273.05,	2169,	 19.52,	1751,
 90.00,	1888,	  0.00,	1888,
 89.73,	1922,	180.34,	1800,
210.03,	1857,	303.56,	1786,
  2.52,	1791,	 92.58,	1790,
333.37,	1711,	 63.34,	1711,
346.35,	1505,	256.42,	1508,
168.39,	1438,	 78.35,	1436,
266.20,	1433,	176.17,	1433,
 59.17,	1428,	330.85,	1410,
//101:
306.36,	1391,	 36.18,	1383,
152.50,	1361,	 62.47,	1360,
195.97,	1325,	105.86,	1337,
159.31,	1384,	 57.19,	1221,
 95.48,	1242,	  5.63,	1244,
  9.14,	1192,	 99.16,	1192,
189.20,	1192,	 99.18,	1192,
  3.28,	1113,	272.48,	1256,
304.43,	1160,	 34.43,	1161,
275.54,	1160,	  5.55,	1160,
//111:
102.72,	1099,	192.84,	1098,
268.97,	1051,	358.97,	1050,
181.30,	1053,	274.16,	1021,
 97.55,	1050,	  3.85,	1012,
 88.97,	 985,	358.97,	 985,
207.30,	 259,	  8.96,	1355,
 89.87,	 980,	180.15,	 954,
128.65,	 994,	 30.32,	 912,
305.27,	 905,	215.06,	 902,
  4.49,	 915,	 91.47,	 860,
//121:
241.17,	886,	151.21,	887,
 61.20,	861,	151.20,	861,
 66.00,	853,	333.65,	830,
133.29,	790,	 43.29,	790,
270.16,	780,	178.88,	747,
189.53,	823,	 83.47,	698,
 70.27,	755,	160.31,	756,
 90.04,	753,	  0.18,	753,
175.69,	906,	 24.30,	534,
 38.25,	746,	307.34,	738,
//131:
 79.50,	743,	349.79,741,	
100.84,	726,	190.47,	732,
 23.06,	720,	113.06,	720,
203.09,	715,	113.08,	714,
217.94,	701,	307.96,	702,
278.31,	693,	186.68,	692,
 94.95,	696,	186.28,	686,
269.39,	655,	  0.69,	698,
193.70,	403,	266.57,	855,
300.05,	665,	 30.03, 666,
//141:
344.58,	641,	254.72,	639,
333.46,	637,	 63.56,	636,
276.48,	623,	  7.36,	649,
152.71,	637,	144.59,	625,
 87.41,	640,	  4.96,	620,
 89.57,	626,	  0.36,	633,
167.54,	622,	 77.49,	623,
 99.41,	621,	189.40,	621,
279.42,	620,	189.41,	621,
 90.00,	620,	  0.00,	620,
//151:
 77.23,	634,	174.53,	605,
 90.00,	616,	  0.00,	616,
 52.74,	614,	322.76,	615,
 73.20,	581,	343.28,	582,
229.31,	575,	318.52,	584,
101.4,	435,	 41.26,	686,
268.9,	571,	178.9,	571,
162.3,	572,	252.7,	559,
 95.8,	542,	  5.8,	542,
265.2,	540,	174.8,	540,
//161:
 89.9,	535,	180.1,	528,
194.0,	499,	101.9,	539,
 29.8,	474,	310.0,	538,
167.1,	486,	 77.1,	486,
  4.2,	480,	275.8,	484,
123.8,	464,	215.4,	456,
354.9,	437,	264.9,	437,
335.8,	425,	 65.8,	425,
263.4,	307,	345.9,	501,
 78.2,	412,	348.2,	412,
//171:
299.7,	406,	 29.8,	407,
178.9,	402,	268.9,	402,
 56.2,	402,	326.2,	402,
303.9,	399,	213.8,	400,
//175: (T terms)
 90.00000,	1234019,	 90.00000,	930472,
232.9938,	 515000,	142.9903,	515065,
130.051,	  12907,	 40.049,	 12908,
105.014,	  10686,	323.41,		  4646,
271.93,		   1999,	181.93,		  1999,
 91.93,		   1997,	181.93,		  1997,
//181:
107.16,	620,	197.23,	620,
182.69,	599,	272.68,	599,
  2.66,	596,	272.70,	596,
107.6,	486,	197.8,	488,
288.1,	461,	197.2,	464,
 46.3,	427,	316.4,	426,
//187: (T^2 terms)
270.00,	4147,	 90.00,	5032,
140.87,	2164,	 50.89,	2166,
  1.41,	 996,	255.23,	1021
};

    if (needInit) {
	needInit = False;
	factor = 1e-10;
	fac1 = C::degree;
	for (Int i=0; i<189; i++) {
	    argArray[i].resize(8);
	    argArray[i].makePermanent();
	    argArray[i](0) = MPOSXY[i][0] * fac1;
	    argArray[i](2) = MPOSXY[i][2] * fac1;
	    argArray[i](1) = MPOSXY[i][1] * factor;
	    argArray[i](3) = MPOSXY[i][3] * factor;
	    for (Int j=4; j<8; j++) {
		argArray[i](j) = 0;
	    };
	}
    }
    if (checkT != T) {
	checkT = T;
	Int i;
	for (i=174; i<189; i++) { // get fundamental argument coefficients
	    argArray[i](1) = MPOSXY[i][1] * factor * T;
	    argArray[i](3) = MPOSXY[i][3] * factor * T;
	    argArray[i](5) = MPOSXY[i][1] * factor;
	    argArray[i](7) = MPOSXY[i][3] * factor;
	};
	for (i=186; i<189; i++) { // get fundamental argument coefficients
	    argArray[i](1) *= T;
	    argArray[i](3) *= T;
	    argArray[i](5) *= 2*T;
	    argArray[i](7) *= 2*T;
	}
    }
    DebugAssert(which < 189, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulPosEarthZ(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[32];
    static Double factor = 0;
    static Double fac1 = 0;
    static const Double MPOSZ[32][2] = {
//Z factors(ecliptic, helio Earth)
180.000,	27962,
256.611,	10164,
280.555,	 8046,
256.72,		 4386,
256.62,		 3187,
  0.00,		 2272,
275.12,		 1816,
293.93,		 1640,
 76.55,		 1447,
103.42,		 1431,
//11:
103.26,	1121,
 74.37,	1090,
180.00,	1036,
291.79,	 972,
180.19,	 914,
278.89,	 880,
169.36,	 834,
180.00,	 770,
263.31,	 720,
 59.06,	 692,
//21:
180.5,	526,
103.2,	520,
344.8,	503,
280.6,	475,
333.7,	453,
162.3,	429,
353.9,	406,
 76.2,	402,
//29: (T terms)
185.12558,	2278227,
 90.000,	  54293,
//31:
 82.189,	  19032,
//32: (T^2 terms)
284.741,	   9722
};

    if (needInit) {
	needInit = False;
	factor = 1e-10;
	fac1 = C::degree;
	Int i;
	for (i=0; i<32; i++) {
	    argArray[i].resize(4);
	    argArray[i].makePermanent();
	    argArray[i](0) = MPOSZ[i][0] * fac1;
	    argArray[i](1) = MPOSZ[i][1] * factor;
	    argArray[i](2) = 0;
	    argArray[i](3) = 0;
	}
    }
    if (checkT != T) {
	checkT = T;
	Int i;
	for (i=28; i<32; i++) { // get fundamental argument coefficients
	    argArray[i](1) = MPOSZ[i][1] * factor * T;
	    argArray[i](3) = MPOSZ[i][1] * factor;
	}
	for (i=31; i<32; i++) { // get fundamental argument coefficients
	    argArray[i](1) *= T;
	    argArray[i](3) *= 2*T;
	}
    }
    DebugAssert(which < 189, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulPosSunXY(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[98];
    static Double factor = 0;
    static Double fac1 = 0;
    static const Double MPOSXY[98][4] = {
// XY, Sun(eclip)  
 89.996243,	49567508,	  0.007122,	49553856,
 90.023036,	27184119,	359.978260,	27222470,
 90.013001,	15543566,	359.986984,	15544429,
 89.998760,	 8346116,	359.994071,	 8342410,
270.00000,	 2938943,	270.00000,	 3386226,
 75.67877,	 1201314,	345.68424,	 1201189,
355.57060,	  757834,	265.52664,	  758691,
 99.6017,	  194166,	  8.6359,	  196403,
276.7769,	  193296,	186.7746,	  193244,
278.7363,	  188909,	  8.7801,	  189177,
//11:
279.0072,	143422,	189.2057,	143684,
 98.7637,	140637,	188.7514,	140598,
 65.8075,	118366,	335.7846,	118334,
 86.7821,	 81367,	356.7759,	 81306,
266.7770,	 76708,	356.8019,	 76713,
266.5938,	 62227,	176.6625,	 62623,
 61.365,	 43663,	331.368,	 43663,
156.244,	 37923,	247.016,	 38293,
344.302,	 31543,	251.387,	 31756,
262.697,	 30883,	172.654,	 30923,
//21:
 90.000,	30399,	  0.000,	30401,
103.163,	27884,	193.183,	28740,
278.234,	22716,	352.523,	26991,
258.215,	21619,	167.571,	21346,
 89.978,	17674,	  0.022,	17702,
 89.962,	13472,	179.980,	13750,
 80.376,	11698,	350.371,	11275,
 88.031,	10909,	358.047,	10900,
 12.883,	10542,	102.926,	10555,
196.373,	 9798,	106.615,	 9800,
//31:
 66.707,	6958,	336.709,	6959,
267.217,	6612,	177.247,	6607,
268.524,	6295,	178.506,	6287,
 89.718,	6288,	359.697,	6285,
 32.99,		5688,	302.71,		5664,
 90.03,		4897,	359.97,		4891,
 97.86,		3784,	  8.61,		3476,
200.32,		3055,	290.29,		3024,
206.86,		3104,	 79.86,		2759,
 90.39,		2829,	359.63,		2926,
//41:
280.03,	2862,	190.03,	2862,
280.50,	2858,	 10.52,	2858,
107.89,	2378,	197.94,	2382,
  9.32,	2317,	 99.33,	2316,
280.92,	2264,	190.71,	2336,
273.69,	2236,	  3.69,	2218,
 86.85,	2188,	176.81,	2187,
 98.81,	2003,	188.93,	2005,
279.68,	1968,	189.63,	1969,
 11.17,	1908,	279.02,	1855,
//51:
 47.07,	1881,	317.07,	1881,
260.37,	1832,	350.33,	1832,
346.50,	1729,	257.13,	1742,
174.63,	1637,	 84.58,	1638,
153.30,	1658,	 68.45,	1555,
127.97,	1574,	 38.01,	1569,
108.53,	1549,	 18.95,	1555,
278.95,	1159,	  8.95,	1159,
 98.80,	1144,	  8.74,	1143,
290.91,	1123,	197.58,	1160,
//61:
288.14,	1083,	 18.12,	1083,
 74.74,	 832,	344.74,	 832,
 62.37,	 792,	332.33,	 791,
346.10,	 749,	 77.01,	 757,
176.20,	 738,	 86.29,	 738,
204.53,	 723,	115.05,	 724,
309.70,	 718,	 39.74,	 720,
122.93,	 712,	 32.92,	 715,
130.84,	 701,	 40.48,	 703,
299.69,	 695,	209.57,	 696,
//71:
315.60,	687,	 45.65,	690,
101.30,	640,	185.30,	704,
105.72,	660,	195.76,	661,
281.71,	622,	 11.40,	630,
 89.65,	622,	  0.34,	629,
271.69,	609,	181.66,	608,
101.35,	585,	 11.31,	586,
 88.9,	516,	358.9,	516,
310.2,	507,	220.2,	508,
130.4,	498,	220.4,	497,
//81:
351.2,	472,	261.2,	472,
274.9,	458,	184.9,	455,
251.9,	435,	161.9,	436,
165.0,	428,	 75.5,	442,
//85: (T terms)
 90.000,	12965,	270.000,	  63,
234.527,	 8975,	144.488,	8989,
196.650,	 7770,	106.330,	7815,
 16.208,	 7537,	106.256,	7550,
 27.402,	 6060,	297.410,	6056,
 16.47,		 5726,	286.565,	5733,
//91:
196.27,	5615,	286.26,	5613,
252.66,	1011,	344.13,	1029,
 86.80,	 875,	356.72,	 873,
140.69,	 726,	 50.67,	 727,
115.80,	 537,	205.8,	 538,
103.44,	 574,	343.3,	 473,
 13.1,	 441,	283.1,	 440,
291.2,	 321,	165.5,	 446
};

    if (needInit) {
	needInit = False;
	factor = 1e-10;
	fac1 = C::degree;
	for (Int i=0; i<98; i++) {
	    argArray[i].resize(8);
	    argArray[i].makePermanent();
	    argArray[i](0) = MPOSXY[i][0] * fac1;
	    argArray[i](2) = MPOSXY[i][2] * fac1;
	    argArray[i](1) = MPOSXY[i][1] * factor;
	    argArray[i](3) = MPOSXY[i][3] * factor;
	    for (Int j=4; j<8; j++) {
		argArray[i](j) = 0;
	    };
	};
    }
    if (checkT != T) {
	checkT = T;
	for (Int i=84; i<98; i++) { // get fundamental argument coefficients
	    argArray[i](1) = MPOSXY[i][1] * factor * T;
	    argArray[i](3) = MPOSXY[i][3] * factor * T;
	    argArray[i](5) = MPOSXY[i][1] * factor;
	    argArray[i](7) = MPOSXY[i][3] * factor;
	};
    }
    DebugAssert(which < 98, AipsError);
    return argArray[which];
}

const Vector<Double> &MeasTable::mulPosSunZ(uInt which, Double T) {
    static Bool needInit = True;
    static Double checkT = -1e30;
    static Vector<Double> argArray[29];
    static Double factor = 0;
    static Double fac1 = 0;
    static const Double MPOSZ[29][2] = {
// Z Sun(eclip)
246.32367,	1181234,
259.53511,	1127775,
228.2177,	 480205,
 90.0000,	 114995,
285.8981,	 112657,
152.407,	  32986,
245.217,	  27333,
254.184,	   9425,
122.335,	   8186,
 83.42,		   4079,
//11:
288.63,	3169,
112.58,	2595,
224.87,	2453,
127.99,	2329,
  3.20,	2180,
202.72,	1973,
295.15,	1452,
 59.99,	1358,
146.90,	1050,
 55.63,	1050,
//21:
283.32,	1047,
230.88,	 993,
249.34,	 872,
106.62,	 800,
114.3, 	 544,
216.2, 	 461,
//27: (T terms)
323.28,	5444,
143.14,	3882,
270.00,	1334
};

    if (needInit) {
	needInit = False;
	factor = 1e-10;
	fac1 = C::degree;
	for (Int i=0; i<29; i++) {
	    argArray[i].resize(4);
	    argArray[i].makePermanent();
	    argArray[i](0) = MPOSZ[i][0] * fac1;
	    argArray[i](1) = MPOSZ[i][1] * factor;
	    argArray[i](2) = 0;
	    argArray[i](3) = 0;
	};
    };
    if (checkT != T) {
	checkT = T;
	Int i;
	for (i=26; i<29; i++) { // get fundamental argument coefficients
	    argArray[i](1) = MPOSZ[i][1] * factor * T;
	    argArray[i](3) = MPOSZ[i][1] * factor;
	}
    }
    DebugAssert(which < 29, AipsError);
    return argArray[which];
}

const RotMatrix &MeasTable::posToRect() {
    static Bool needInit = True;
    static RotMatrix rot;
    if (needInit) {
        needInit = False;
	Euler ang(+84381.4091 * C::arcsec, 1, -0.0930 * C::arcsec, 3);
	rot = RotMatrix(ang);
    }
    return rot;
}

const RotMatrix &MeasTable::rectToPos() {
    static Bool needInit = True;
    static RotMatrix rot;
    if (needInit) {
        needInit = False;
	rot = MeasTable::posToRect();
	rot.transpose();
    };
    return rot;
}

const RotMatrix &MeasTable::galToSupergal() {
    static Bool needInit = True;
    static RotMatrix rot;
    if (needInit) {
        needInit = False;
	Euler ang( -90*C::degree, 3, -85*C::degree, 2, -45*C::degree, 3);
	rot = RotMatrix(ang);
    };
    return rot;
}

// Position related routines
Double MeasTable::WGS84(uInt which) {
    static const Double data[2] = {
    6378137,	298.257223563};

    DebugAssert(which < 2, AipsError);
    return data[which];
}

// Polar motion related routines
const Euler &MeasTable::polarMotion(Double ut) {
  static Bool msgDone = False;
  static Euler res(0.0, 2, 0.0, 1, 0.0, 3);
  static Double checkT = -1e6;
  if ( !nearAbs(ut, checkT, 0.04)) {
    checkT = ut;
    if (!MeasIERS::get(res(0), MeasIERS::MEASURED, MeasIERS::X,
		       ut) ||
	!MeasIERS::get(res(1), MeasIERS::MEASURED, MeasIERS::Y,
		       ut)) {
      if (!msgDone) {
	msgDone = True;
	LogIO os(LogOrigin("MeasTable",
			   String("PolarMotion(Double)"),
			   WHERE));
	os << LogIO::WARN <<
	  String("No requested polar motion data available from IERS tables. "
		 "\nProceeding with probably less precision.") <<
	  LogIO::POST;
      };
    };
    res(0) *= -C::arcsec;
    res(1) *= -C::arcsec;
  };
  return res;
}

// Time functions
Double MeasTable::dUTC(Double utc) {
  static Int N = 0;
  static Double (*LEAP)[4];
  if (N == 0) {
    Table t;
    ROTableRow row;
    TableRecord kws;
    String rfn[4] = {"MJD", "dUTC", "Offset", "Multiplier"};
    RORecordFieldPtr<Double> rfp[4];
    Double dt;
    String vs;
    if (!MeasIERS::getTable(t, kws, row, rfp, vs, dt, 4, rfn, "TAI_UTC",
		  "measures.tai_utc.directory",
		  "aips/Measures")) {
      LogIO os(LogOrigin("MeasTable",
			 String("dUTC(Double)"),
			 WHERE));
      os << "Cannot read leap second table TAI_UTC" << LogIO::EXCEPTION;
    };
    N = t.nrow();
    if (N < 35) {
      LogIO os(LogOrigin("MeasTable",
			 String("dUTC(Double)"),
			 WHERE));
      os << "Leap second table TAI_UTC corrupted" << LogIO::EXCEPTION;
    };
    if (Time().modifiedJulianDay() - dt > 180) {
      LogIO os(LogOrigin("MeasTable",
			 String("dUTC(Double)"),
			 WHERE));
      os << LogIO::SEVERE <<
	String("Leap second table TAI_UTC seems out-of-date: ") +
	"regenerate" << LogIO::POST;
    };
    LEAP = (Double (*)[4])(new Double[4*N]);
    for (Int i=0; i < N; i++) {
      row.get(i);
      for (Int j=0; j < 4; j++) {
	LEAP[i][j] = *(rfp[j]);
      };
    };
  }; 
  Double val;
  if (utc < LEAP[0][0]) {
    val = LEAP[0][1] + (utc - LEAP[0][2])*LEAP[0][3];
    ///  } else if (utc >= LEAP[N-1][0]) {
    ///    val = LEAP[N-1][1];
  } else {
    for (Int i = N-1; i >= 0; i--) {
      if (utc >= LEAP[i][0]) {
	val = LEAP[i][1];
	if (LEAP[i][3] != 0) {
	  val += (utc - LEAP[i][2])*LEAP[i][3];
	};
	break;
      };
    };
  };
  return val;
}

Double MeasTable::dTAI(Double tai) {
    return (32.184);
}

Double MeasTable::dTDT(Double ut1) {
    Double g = (357.53 + 0.9856003*(ut1-MeasData::MJD2000))*C::degree;
    return (0.001658*sin(g) + 0.000014*sin(2*g));
}

Double MeasTable::dTDB(Double tai) {
    return(1.550505e-8*86400*(tai-43144.0));
}

Double MeasTable::dTCG(Double tai) {
    return(6.969291e-10*86400*(tai-43144.0));
}

Double MeasTable::GMST0(Double ut1) {
    static Bool needInit = True;
    static Polynomial<Double> stPoly(3);
    if (needInit) {
	needInit = False;
	stPoly.setCoefficient(0, 24110.54841);	
	stPoly.setCoefficient(1, 8640184.812866);
	stPoly.setCoefficient(2, 0.093104);	
	stPoly.setCoefficient(3, -6.2e-6);
    };
    return (stPoly((ut1-MeasData::MJD2000)/MeasData::JDCEN));
}

Double MeasTable::GMUT0(Double gmst1) {
    static Bool needInit = True;
    static Polynomial<Double> stPoly(3);
    if (needInit) {
	needInit = False;
	stPoly.setCoefficient(0, -0.65830845056254866847);
	stPoly.setCoefficient(1, -235.90946916710752);
	stPoly.setCoefficient(2, -0.00000252822553597972);
	stPoly.setCoefficient(3, 0.0000000001679);
    };
    return (stPoly((gmst1-MeasData::MJD2000-6713.)/MeasData::JDCEN));
}

Double MeasTable::UTtoST(Double ut1) {
    static Bool needInit = True;
    static Polynomial<Double> UTSTPoly(2);
    if (needInit) {
	needInit = False;
	UTSTPoly.setCoefficient(0, 1.002737909350795);
	UTSTPoly.setCoefficient(1, +5.9006e-11);
	UTSTPoly.setCoefficient(2, -5.9e-15);	
    }
    return(UTSTPoly((ut1-MeasData::MJD2000)/MeasData::JDCEN));
}

Double MeasTable::dUT1(Double utc) {
  static msgDone = False;
  static Double res = 0;
  static Double checkT = -1e6;
  if ( !nearAbs(utc, checkT, 0.04)) {
    checkT = utc;
    if (!MeasIERS::get(res, MeasIERS::MEASURED, MeasIERS::dUT1,
		       utc)) {
      if (!msgDone) {
	msgDone = True;
	LogIO os(LogOrigin("MeasTable",
			   String("dUT1(Double)"),
			   WHERE));
	os << LogIO::WARN <<
	  String("No requested dUT1 data available from IERS tables. "
		 "\nProceeding with probably less precision.") <<
	  LogIO::POST;
      };
    };
  };
  return res;
}
