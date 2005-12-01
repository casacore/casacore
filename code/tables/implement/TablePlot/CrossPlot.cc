//# CrossPlot.cc: Basic table access class for the TablePlot (tableplot) tool
//#               to plot across rows for an array-column.
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#include <cmath>

#include <casa/Exceptions.h>

#include <tables/Tables/TableParse.h>
#include <tables/Tables/TableGram.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableLock.h>
#include <tables/Tables/TableIter.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/MatrixMath.h>
#include <casa/Arrays/ArrayError.h>

#include <tables/Tables/ExprMathNode.h>
#include <tables/Tables/ExprMathNodeArray.h>
#include <tables/Tables/ExprDerNode.h>
#include <tables/Tables/ExprDerNodeArray.h>
#include <tables/Tables/ExprFuncNode.h>
#include <tables/Tables/ExprFuncNodeArray.h>
#include <tables/Tables/ExprLogicNode.h>
#include <tables/Tables/ExprLogicNodeArray.h>
#include <tables/Tables/ExprNodeArray.h>
#include <tables/Tables/ExprNodeSet.h>
#include <tables/Tables/ExprNodeRep.h>
#include <tables/Tables/ExprNodeRecord.h>
#include <tables/Tables/ExprRange.h>
#include <tables/Tables/RecordGram.h>

#include <casa/Utilities/DataType.h>

#include <tables/TablePlot/CrossPlot.h>

namespace casa { //# NAMESPACE CASA - BEGIN

#define TMR(a) "[User: " << a.user() << "] [System: " << a.system() << "] [Real: " << a.real() << "]"
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))


/* Default Constructor */
template<class T> CrossPlot<T>::CrossPlot() : BasePlot<T>()
{
	dbg=0;	ddbg=0;	adbg=0;
	if(adbg)cout << "CrossPlot constructor" << endl;
	//nip_p=0;
	nflagmarks_p=0;
	//xtens_p.resize(0); 
	ytens_p.resize(0);
	Pind_p.resize(0,0); Tsize_p.resize(0,0);
	//colnames_p.resize(3);
	ipslice_p.resize(0);
	//IndCnt_p.resize(0);
	locflagmarks_p.resize(0);
	xprange_p.resize(0,0); yprange_p.resize(0,0);
	//FlagColName_p = "FLAG"; fcol_p = False;
	//FlagRowName_p = "FLAG_ROW"; frcol_p = False;
	xptr_p=0; yptr_p=0;

	pType_p = CROSSPLOT; 
}

/*********************************************************************************/

/* Destructor */
template<class T> CrossPlot<T>::~CrossPlot()
{
	if(adbg)cout << "CrossPlot destructor" << endl;
}

/*********************************************************************************/


/*********************************************************************************/
template<class T> Int CrossPlot<T>::createXTENS(Vector<String> &datastr)
{
	if(adbg) cout << "CrossPlot : createXTENS -> do nothing" << endl;
	return 0;
}
/*********************************************************************************/

/*********************************************************************************/

#if 1
/* Extract X data from the table */
template<class T> Int CrossPlot<T>::getXData(TableExprId &tid)
{
	Int  rc=0;
	
	Slicer tsl;
	Int cc=0;
	
	rc = tid.rownr();
	
	if(rc==0)
	{
		xptr_p=0;

		for(Int z=0;z<nTens_p;z++)
		{
			Tsize_p(z,0) = 0;
			//xptr_p+= Tsize_p(z,0); 
		}

		xplotdata_p.resize(1,yptr_p);	xplotdata_p.set((T)0);
		xpd_p = xplotdata_p.shape();

		// fill in the correct numbers here - from ipslice..
		
		cc=0;
		for(Int z=0;z<nTens_p;z++)
		{
			tsl = ipslice_p(z);
			
			for(Int j=(tsl.start())[1];j<=(tsl.end())[1];j+=(tsl.stride())[1]) 
			{
				xplotdata_p(0,cc++) = j+1;
				if(ddbg)cout << " " << j+1 << " " ;
			}
		}
		if(ddbg)cout << endl;
		if(cc != yptr_p) cout << "AAAaah : filled xplotdata_p upto size " << cc << " but yptr is " << yptr_p << endl;
		

		if(ddbg) cout << "xplotdata_p has shape : " << xpd_p << endl;

		
	}
	else
	{
		// do nothing - xplotdata_p has already been filled.
	}

	return 0;
}

#endif

/*********************************************************************************/

/* Compute the combined plot range */
template<class T> Int CrossPlot<T>::setPlotRange(T &xmin, T &xmax, T &ymin, T &ymax, Int useflags)
{
	if(adbg)cout << "CrossPlot :: Set Plot Range for this table " << endl;
	xprange_p.resize(NPlots_p,2);
	yprange_p.resize(NPlots_p,2);

	Bool abc = False;
	abc = (useflags==2)?True:False;
	
	if(ddbg)cout << "PIND : " << Pind_p << endl;
	
	/* compute min and max for each Plot */
	for(int i=0;i<NPlots_p;i++)
	{
		xprange_p(i,0) = 1e+30;
		xprange_p(i,1) = -1e+30;
		yprange_p(i,0) = 1e+30;
		yprange_p(i,1) = -1e+30;
		
		for(int rc=0;rc<NRows_p;rc++)
		{
			if((theflags_p(Pind_p(i,1),rc)==(Bool)useflags) || abc) 
			{
				if(xplotdata_p(Pind_p(i,0),Pind_p(i,1)) < xprange_p(i,0)) xprange_p(i,0) = xplotdata_p(Pind_p(i,0),Pind_p(i,1));
				if(xplotdata_p(Pind_p(i,0),Pind_p(i,1)) >= xprange_p(i,1)) xprange_p(i,1) = xplotdata_p(Pind_p(i,0),Pind_p(i,1));
				if(yplotdata_p(Pind_p(i,1),rc) < yprange_p(i,0)) yprange_p(i,0) = yplotdata_p(Pind_p(i,1),rc);
				if(yplotdata_p(Pind_p(i,1),rc) >= yprange_p(i,1)) yprange_p(i,1) = yplotdata_p(Pind_p(i,1),rc);
			}
		}

	}
	
	xmin=0;xmax=0;ymin=0;ymax=0;
	xmin = xprange_p(0,0);
	xmax = xprange_p(0,1);
	ymin = yprange_p(0,0);
	ymax = yprange_p(0,1);
	
	if(ddbg) cout << " initial Ranges : [" << xmin << "," << xmax << "] [" << ymin << "," << ymax << "]" << endl;

	/* get a comnined min,max */

	for(int qq=1;qq<NPlots_p;qq++)
	{
		xmin = MIN(xmin,xprange_p(qq,0));
		xmax = MAX(xmax,xprange_p(qq,1));
	}
	for(int qq=1;qq<NPlots_p;qq++)
	{
		ymin = MIN(ymin,yprange_p(qq,0));
		ymax = MAX(ymax,yprange_p(qq,1));
	}
	
	if(ddbg) cout << " Final Ranges : [" << xmin << "," << xmax << "] [" << ymin << "," << ymax << "]" << endl;

	//cout << Pind_p << endl;

	return 0;
}

/*********************************************************************************/
#if 1

/* Fill up 'theflags_p' array and optionally write flags to disk */
template<class T> Int CrossPlot<T>::flagData(Int diskwrite, Int rowflag)
{
	if(adbg) cout << "CrossPlot :: Flag Data and optionally write to disk" << endl;
	
	if(nflagmarks_p>0)
	{
	for(Int nf=0;nf<nflagmarks_p;nf++)
	{
		if(ddbg)cout << "*******" << endl;
		if(ddbg)cout << (locflagmarks_p[nf])[0] << "," << (locflagmarks_p[nf])[1] << "," << (locflagmarks_p[nf])[2] << "," << (locflagmarks_p[nf])[3] << endl;
	}
	
	for(int nr=0;nr<NRows_p;nr++)
	{
		for(int np=0;np<NPlots_p;np++)
		{
			for(int nf=0;nf<nflagmarks_p;nf++)
				if(xplotdata_p(Pind_p(np,0),Pind_p(np,1))>(locflagmarks_p[nf])[0] &&
				   xplotdata_p(Pind_p(np,0),Pind_p(np,1))<=(locflagmarks_p[nf])[1] && 
				   yplotdata_p(Pind_p(np,1),nr)>(locflagmarks_p[nf])[2] && 
				   yplotdata_p(Pind_p(np,1),nr)<=(locflagmarks_p[nf])[3]) 
					{
						theflags_p(Pind_p(np,1),nr) = True;
					}
		}
	}
	
	if(diskwrite) setFlags(rowflag);
	}

	return 0;
}
#endif

/*********************************************************************************/
/** Make this return a flipped value for crossplot **/

template<class T> T CrossPlot<T>::getXVal(Int pnum, Int col)
{
	if(adbg)cout << "CrossPlot :: Get Xval" << endl;
	return xplotdata_p(Pind_p(pnum,0),Pind_p(pnum,1));
}

/*********************************************************************************/

/*********************************************************************************/
/*********************************************************************************/

} //# NAMESPACE CASA - END 
