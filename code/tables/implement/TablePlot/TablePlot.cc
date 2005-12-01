//# TablePlot.cc: Implement class for the tableplot DO.
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

#include <stdio.h>
#include <string.h>
#include <casa/iostream.h>
#include <casa/fstream.h>
#include <casa/math.h>

#include <casa/Exceptions.h>
#include <casa/OS/Timer.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/MatrixMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Slicer.h>

#include <tables/Tables/TableParse.h>
#include <tables/Tables/TableGram.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableLock.h>

#include <tables/TablePlot/TablePlot.h>

namespace casa { //# NAMESPACE CASA - BEGIN

/* Default Constructor */
template<class T> TablePlot<T>::TablePlot()
{
	adbg=0;
	if(adbg)cout << "TablePlot constructor" << endl;
	TABS_p.resize(0);
	nTabs_p=0; iTabs_p=0; 
	NPanels_p=1;
}

/*********************************************************************************/

/* Destructor */
template<class T> TablePlot<T>::~TablePlot()
{
	if(adbg)cout << "TablePlot destructor" << endl;
}

/*********************************************************************************/

/* Create a Table obj from the string inTabName - Add onto list of Table objects */
template<class T> Int TablePlot<T>::setTableS(Int ntabs, String &inTabName)
{
	if(adbg)cout << "TablePlot :: SelectData - Table string : " << iTabs_p << endl;
	
	if(nTabs_p==0) nTabs_p = ntabs;
	if((Int)TABS_p.nelements() != nTabs_p) TABS_p.resize(nTabs_p);
	try
	{
	TABS_p[iTabs_p] = Table(inTabName,Table::Update);
	}
	catch(TableError &x)
	{
		cout << "Table Error : " << x.getMesg() << endl;
		return -1;
	}

	iTabs_p++;
		
	return 0;
}

/*********************************************************************************/

/* Add inTabObj directly onto the list of Table objects : TABS_p */
template<class T> Int TablePlot<T>::setTableT(Int ntabs, Table &inTabObj)
{
	if(adbg)cout << "TablePlot :: SelectData - Table object : " << iTabs_p << endl;
	
	if(nTabs_p==0) nTabs_p = ntabs;
	if((Int)TABS_p.nelements() != nTabs_p) TABS_p.resize(nTabs_p);
	TABS_p[iTabs_p] = inTabObj;
	iTabs_p++;

	return 0;
}

/*********************************************************************************/

// send in a choice for type of BasePlot obj to use.

/* Create the BasePlot objects */
template<class T> Int TablePlot<T>::createBP(PtrBlock<BasePlot<T>* > &BPS, String pType)
{
	if(adbg)cout << "TablePlot :: Create BP" << endl;
	Int key = XYPLOT;
	
	if(pType.contains("cross") || pType.contains("CROSS") ) key = CROSSPLOT;
	if(pType.contains("hist") || pType.contains("HIST") ) key = HISTPLOT;

	if(adbg)cout << "string : " << pType << " and correct key : " << key << endl;

	// if switching from normal to cross plot, cleanup BPS, and remake.
	Int change = 0;
	if((Int)BPS.nelements() > 0)
	{
		if(adbg) cout << "Type : " << BPS[0]->getPlotType() << "  key : " << key << endl;
		if(BPS[0]->getPlotType() != key) 
		{
			for(Int i=0;i<(Int)BPS.nelements();i++) delete BPS[i];
			BPS.resize(0,Bool(1));
			if(adbg)cout << "Number of BPSs - should be zero !! : " << BPS.nelements() << endl;
		}
		
		if(adbg)cout << "Number of BPSs : " << BPS.nelements() << endl;
	}
	
	// if BPS is of zero size, then make according to chosen pType.
	if((Int)BPS.nelements()==0) 
	{
		BPS.resize(nTabs_p);
		switch (key)
		{
			case XYPLOT:
				for(Int i=0;i<nTabs_p;i++) BPS[i] = new BasePlot<T>();
			        break;
			case CROSSPLOT:
				for(Int i=0;i<nTabs_p;i++) BPS[i] = new CrossPlot<T>();
				break;
			//case HISTPLOT:
			//	for(Int i=0;i<nTabs_p;i++) BPS[i] = new HistPlot<T>();
			//	break;
			      
		}
		change = 1;
	}
	
	return change;
	// if this is 1 ==> need to call upDateBP
	// if this is 0 ==> no need to call upDateBP (but no harm if called)
}

/*********************************************************************************/

/* Attach one table to each BasePlot */
template<class T> Int TablePlot<T>::upDateBP(PtrBlock<BasePlot<T>* > &BPS)
{
	if(adbg)cout << "TablePlot :: UpDate BP" << endl;
	
	for(Int i=0;i<nTabs_p;i++) 
	{
		BPS[i]->init(TABS_p[i]); // initialise each BP - TableObj
		if(adbg)cout << "inited BP : " << i << endl;
	}
	
	if(adbg)cout << "TablePlot :: Finished updating BP " << endl;
	return 0;
}

/*********************************************************************************/
/* CleanUp BasePlots */
template<class T> Int TablePlot<T>::cleanUpBP(PtrBlock<BasePlot<T>* > &BPS)
{
	if(BPS.nelements()>0)
	{
		for(Int i=0;i<(Int)BPS.nelements();i++) delete BPS[i];
		BPS.resize(0,(Bool)1);
	}
	return 0;
}

/*********************************************************************************/

/* Set plot options */
template<class T> Int TablePlot<T>::setPlotParameters(TPPlotter<T> &TPLP,Record &plotoptions)
{
	TPLP.setPlotOptions(plotoptions);

	return 0;
}
/*********************************************************************************/

/* Set plot options */
template<class T> Int TablePlot<T>::setPlotLabels(TPPlotter<T> &TPLP,Vector<String> &labels)
{
	TPLP.setLabels(labels);

	return 0;
}
/*********************************************************************************/

/* Read data from all tables */
template<class T> Int TablePlot<T>::getData(PtrBlock<BasePlot<T>* > &BPS,Vector<String> &datastr)
{
	if(adbg)cout << "TablePlot :: Create TENS and Extract Data" << endl;

	/* create TableExprNodes for each table */
	/* (if different TaQL strings are to apply to diff tables, make the change here) */
	for(Int i=0;i<nTabs_p;i++)
	{
		if(BPS[i]->createTENS(datastr) == -1) 
			return -1;
	}
	
	for(Int i=0;i<nTabs_p;i++) 
	{
		if(BPS[i]->getData() == -1) 
			return -1;
	}
	
	return 0;
}


/*********************************************************************************/

/* Read Data - to be called after any 'getdata' and before any plotdata */
template<class T> Int TablePlot<T>::readXData(BasePlot<T>* &BP, Matrix<T> &xdat)
{
	if(adbg)cout << "TablePlot :: Read XData" << endl; 
	if(BP->readXD(xdat) == -1) return -1;
	return 0;
}

/*********************************************************************************/

/* Write Data - to be called after any 'getdata' and before any plotdata */
template<class T> Int TablePlot<T>::writeXData(BasePlot<T>* &BP, Matrix<T> &xdat)
{
	if(adbg)cout << "TablePlot :: Write XData" << endl; 
	if(BP->writeXD(xdat) == -1) return -1; // if shapes/types don't match.
	return 0;
}

/*********************************************************************************/

/* Read Data - to be called after any 'getdata' and before any plotdata */
template<class T> Int TablePlot<T>::readYData(BasePlot<T>* &BP, Matrix<T> &ydat)
{
	if(adbg)cout << "TablePlot :: Read YData" << endl; 
	if(BP->readYD(ydat) == -1) return -1;
	return 0;
}

/*********************************************************************************/

/* Write Data - to be called after any 'getdata' and before any plotdata */
template<class T> Int TablePlot<T>::writeYData(BasePlot<T>* &BP, Matrix<T> &ydat)
{
	if(adbg)cout << "TablePlot :: Write YData" << endl; 
	if(BP->writeYD(ydat) == -1) return -1; // if shapes/types don't match.
	return 0;
}

/*********************************************************************************/

/* Plot Data - to be called after any 'getdata' */
template<class T> Int TablePlot<T>::plotData(PtrBlock<BasePlot<T>* > &BPS,TPPlotter<T> &TPLP,  Int panel)
{
	if(adbg)cout << "TablePlot :: Plot Data" << endl; 

	TPLP.setPlotRange(BPS,panel); 
	TPLP.plotData(BPS,panel);
	return 0;
}

/*********************************************************************************/

/* Mark regions to flags */
template<class T> Int TablePlot<T>::markFlags(Int panel, TPPlotter<T> &TPLP)
{
	if(adbg)cout << "TablePlot :: Mark Flag Regions" << endl;
	Int ret = TPLP.markFlags(panel);
	if(ret==-1) 
	{
		cout << "Invalid Flag region - not marked " << endl;
		return -1;
	}
	else
	{
		if(adbg)cout << "Finished marking flag region " << endl;
		return 0;
	}
}

/*********************************************************************************/

/* Mark region to zoom */
template<class T> Int TablePlot<T>::markZoom(Int panel, TPPlotter<T> &TPLP, Int direction)
{
	if(adbg)cout << "TablePlot :: Mark Zoom Regions" << endl;
	Int pan=1;
	pan = TPLP.markZoom(panel,direction);
	if(pan==-1)
	{
		cout << "Invalid Zoom region - not zoomed " << endl;
		return -1;
	}
	else
	{
	if(adbg)cout << "Finished marking zoom region " << endl;
	return pan;
	}
}
/*********************************************************************************/

/* Flag data for each table in the list of BasePlots */
template<class T> Int TablePlot<T>::flagData(PtrBlock<BasePlot<T>* > &BPS,
			TPPlotter<T> &TPLP, Int panel, Int diskwrite, Int rowflag)
{
	if(adbg)cout << "TablePlot :: Flag Data" << endl;

	TPLP.setFlagRegions(BPS,panel); 
	if(adbg)cout << "nTabs_p : " << nTabs_p << endl;
	for(Int i=0;i<nTabs_p;i++) BPS[i]->flagData(diskwrite,rowflag);
		
	TPLP.setPlotRange(BPS,panel);
	TPLP.plotData(BPS,panel); 
	
	return 0;
}

/*********************************************************************************/

/* Clear All flags from all tables */
template<class T> Int TablePlot<T>::clearFlags(PtrBlock<BasePlot<T>* > &BPS)
{
	if(adbg)cout << "TablePlot :: Clear Flags" << endl;
	
	for(Int i=0;i<nTabs_p;i++) BPS[i]->clearFlags();
	
	return 0;
}

/*********************************************************************************/
/* Plot by iterating over a specified iteration axis. */
/*********************************************************************************/

/* Start the iterations */
template<class T> Int TablePlot<T>::iterMultiPlotStart(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPPlotter<T> &TPLP, Int npanels,Vector<String> &datastr, Vector<String> &iteraxes)
{
	if(adbg)cout << "TablePlot :: iterMultiPlotStart " << endl;
	Block<String> itx;
	NPanels_p = npanels;
	ATBPS.resize(NPanels_p);

	for(Int i=0;i<NPanels_p;i++) 
	{
		ATBPS[i] = new PtrBlock<BasePlot<T>* >();
		createBP(*ATBPS[i],datastr[0]);
	}
	
	
	itx.resize(iteraxes.nelements()); // remove !
	for(Int i=0;i<(Int)iteraxes.nelements();i++) 
	{
		itx[i] = iteraxes[i]; 
	}
	Iters_p.resize(nTabs_p);
	try
	{
	for(Int i=0;i<nTabs_p;i++) Iters_p[i] = TableIterator(TABS_p[i],itx);
	}
	catch(TableError &x)
	{
		cout << "Iteraxis Error : " << x.getMesg() << endl;
		return -1;
	}
	panelcounter_p=0;

	return 0;
}

/*********************************************************************************/

/* Next iteration */
template<class T> Int TablePlot<T>::iterMultiPlotNext(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPPlotter<T> &TPLP,Int &apanels)
{
	if(adbg)cout << "TablePlot :: iterMultiPlotNext " << endl;
	Table temptable;
	if(Iters_p[0].pastEnd()) //check for end of table from first table in list.
	{
		iterMultiPlotStop(ATBPS,TPLP);
		return 0;
	}
	else
	{
		panelcounter_p=0;
		while(panelcounter_p<NPanels_p && !Iters_p[0].pastEnd())
		{
			for(Int i=0;i<nTabs_p;i++) 
			{
				temptable = Iters_p[i].table();
				(*ATBPS[panelcounter_p])[i]->init(temptable); // init each BP with Tobj 
				Iters_p[i].next();
			}
		panelcounter_p++;
		}
	apanels=panelcounter_p;
	}

	return 1;
}

/*********************************************************************************/

/* Stop iterations */
template<class T> Int TablePlot<T>::iterMultiPlotStop(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPPlotter<T> &TPLP)
{
	if(adbg)cout << "TablePlot :: iterMultiPlotStop " << endl;
	
 	Iters_p.resize(0);
	
	for(Int i=0;i<(Int)ATBPS.nelements();i++) 
	{
		delete ATBPS[i];
	}
	
	ATBPS.resize(0);
	
	return 0;
}

/*********************************************************************************/
} //# NAMESPACE CASA - END 


