//# BasePlot.cc: Basic table access class for the TablePlot (tableplot) tool
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

#include <tables/TablePlot/BasePlot.h>

namespace casa { //# NAMESPACE CASA - BEGIN

#define TMR(a) "[User: " << a.user() << "] [System: " << a.system() << "] [Real: " << a.real() << "]"
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))

/* Default Constructor */
template<class T> BasePlot<T>::BasePlot()
{
	dbg=0;	ddbg=0;	adbg=0;
	if(adbg)cout << "BasePlot constructor" << endl;
	nip_p=0;
	nflagmarks_p=0;
	xtens_p.resize(0); ytens_p.resize(0);
	Pind_p.resize(0,0); Tsize_p.resize(0,0);
	colnames_p.resize(3); ipslice_p.resize(0);
	IndCnt_p.resize(0);
	locflagmarks_p.resize(0);
	xprange_p.resize(0,0); yprange_p.resize(0,0);
	FlagColName_p = "FLAG"; fcol_p = False;
	FlagRowName_p = "FLAG_ROW"; frcol_p = False;
	
}

/*********************************************************************************/

/* Destructor */
template<class T> BasePlot<T>::~BasePlot()
{
	if(adbg)cout << "BasePlot destructor" << endl;
}

/*********************************************************************************/

/* Attach a BasePlot to a table/subtable */
template<class T> Int BasePlot<T>::init(Table &tab)
{
	if(adbg)cout << "BasePlot :: initialize with a Table object" << endl;
	zflg=0;
	fflg=0;
	
	/* Attach table */
	SelTab_p = tab;
	
	/* Check for the existence of Flag column names */
	fcol_p = SelTab_p.tableDesc().isColumn(FlagColName_p);
	frcol_p = SelTab_p.tableDesc().isColumn(FlagRowName_p);
	if(adbg)
	{
		if(fcol_p) cout << "Column : " << FlagColName_p << " exists " << endl;
		else cout << "Column : " << FlagColName_p << " does not exist " << endl;
		if(frcol_p) cout << "Column : " << FlagRowName_p << " exists " << endl;
		else cout << "Column : " << FlagRowName_p << " does not exist " << endl;
	}
	
	if(ddbg)
	{
	cout << "is null 2 : " << SelTab_p.isNull() << endl;
	cout << "tablename : " << SelTab_p.tableName() << endl;
	cout << "is changed : " << SelTab_p.hasDataChanged() << endl;
	}

	/* clear bookkeeping arrays between tables */
	cleanUp();

	return 0;
}

/*********************************************************************************/

/* Create TableExprNodes from TaQL strings and obtain TaQL indices/column names */
template<class T> Int BasePlot<T>::createTENS(Vector<String> &datastr)
{
	if(adbg)cout << "BasePlot :: Create TableExprNodes from TaQL strings" << endl;
		
	/* Check the number of input strings */
	nTStr_p = datastr.nelements();
	if(ddbg)cout << "Number of strings : " << nTStr_p << endl;
	
	if(nTStr_p%2 != 0) 
	{ cout << "Error : Need even number of TaQL strings" << endl; return -1;}
	
	nTens_p = nTStr_p/2; 

	xtens_p.resize(nTens_p);
	ytens_p.resize(nTens_p);
	IndCnt_p.resize(nTens_p);
	colnames_p.resize(0);
	ipslice_p.resize(0);
	nip_p=0;
	
	/* Create TENS and traverse parse trees */
	try
	{
	for(Int i=0;i<nTens_p;i++) 
	{
		xtens_p[i] = RecordGram::parse(SelTab_p,datastr[i*2]);
		ytens_p[i] = RecordGram::parse(SelTab_p,datastr[i*2+1]);
		
		if( (xtens_p[i].dataType() != TpDouble) || (ytens_p[i].dataType() != TpDouble) ) 
		{
			cout << "DataType of TaQL expression is not plottable... "<< endl;
			return -1;
		}
		
		getIndices(xtens_p[i]);
		IndCnt_p[i] = nip_p;/* since flags pertain only to y data */
		getIndices(ytens_p[i]);
	}
	}
	catch(AipsError &x)
	{
		cout << " Error : " << x.getMesg() << endl;
		return -1;
	}

	/* Fill in shapes of accessed columns. Needed while flagging */
	Bool tst=False;
	IPosition dsh;
	colshapes_p.resize(0);
	colshapes_p.resize(colnames_p.nelements());
	for(Int i=0;i<nip_p;i++)	
	{
		tst = SelTab_p.tableDesc().isColumn(colnames_p(i));
		if(!tst) cout << "Column " << colnames_p(i) << " does not exist !" << endl;
		ROTableColumn rot(SelTab_p,colnames_p(i));
		ColumnDesc cdesc = rot.columnDesc();
		if(cdesc.isArray()) colshapes_p(i) = rot.shape(0);
		else colshapes_p(i) = IPosition(1,0);
		if(ddbg)
		{
			cout << "COLUMN : " << colnames_p(i) << " has shape : " << colshapes_p(i) << endl;
			cout << "Is arraycol : " << cdesc.isArray() << endl;
			if((ipslice_p[i].length())[0]==0) cout << "  Scalar Column !! " << endl;
			else cout << "   START : " << ipslice_p[i].start() << "  END : " << ipslice_p[i].end() << "  INC : " << ipslice_p[i].stride() << endl;
		}
	}

	
	return 0;
}

/*********************************************************************************/

/* Extract data from the table */
template<class T> Int BasePlot<T>::getData()
{
	if(adbg)
	cout << "BasePlot :: Get Data into storage arrays" << endl;

	NRows_p = (SelTab_p).nrow();

	/************************ Extract Data from Table ***************/
	if(ddbg) cout << "Extracting data using TEN ... " << endl;
	if(ddbg)tmr.mark();

	TableExprId tid(0);
	Double xytemp;
	Array<Double> xtemp;
	Array<Double> ytemp;
	Array<Bool> arrflag;

	NPlots_p=0; 

	/* Get data from first row, to figure out the total number of plots
	   Resize xplotdata_p, yplotdata_p and Pind_p accordingly
	   Fill in the numbers for Pind_p */

	Int xptr=0,yptr=0;
	tid.setRownr(0);
	TableExprNode tten;
	Tsize_p.resize(nTens_p,2);

	xshp_p.resize(0);
	yshp_p.resize(0);
	flagitshp_p.resize(0);
	
	try{
	for(int z=0;z<nTens_p;z++)
	{
		if(nTStr_p>0)
		{
		tten = xtens_p[z];
		
		if(tten.isScalar())
		{
			tten.get(0,xytemp);
			Tsize_p(z,0) = 1;
		}
		else
		{
			tten.get(0,xtemp);
			xshp_p = xtemp.shape();
			if(ddbg)cout << "Shape of Xaxis data : " << xshp_p << endl;
			Tsize_p(z,0) = xshp_p.product(); 
		}
		xptr+= Tsize_p(z,0); 
		NPlots_p = xptr; 
		}
		tten = ytens_p[z];
		
		if(tten.isScalar())
		{
			if(ddbg) cout << "before scalar get" << endl;
			tten.get(0,xytemp);
			Tsize_p(z,1) = 1;
			yshp_p = IPosition(2,1,1);
		}
		else
		{
			if(ddbg) cout << "before vector get" << endl;
			tten.get(0,ytemp);
			yshp_p = ytemp.shape();
			if(ddbg)cout << "Shape of Yaxis data : " << yshp_p << endl;
			if(ddbg) cout << "ytemp : " << ytemp << endl;
			Tsize_p(z,1) = yshp_p.product(); 
		}
		yptr+=Tsize_p(z,1); 
		NPlots_p = yptr; 
		
		flagitshp_p = yshp_p;
	}
	}
	catch(ArraySlicerError &x){
		cout << "Error in TaQL indices... : " << x.getMesg()<< endl;
		return -1;
	}
	catch(AipsError &x){
		cout << "Error : " << x.getMesg()<< endl;
		return -1;
	}

	xplotdata_p.resize(xptr,NRows_p);	xplotdata_p.set((T)0);
	yplotdata_p.resize(yptr,NRows_p);	yplotdata_p.set((T)0);
	theflags_p.resize(yptr,NRows_p);	theflags_p.set(False);

	Pind_p.resize(NPlots_p,2);
	
	/* Fill up Pind_p */
	xptr=0; yptr=0;
	for(Int m=0;m<nTStr_p/2;m++)
	{
		if(Tsize_p(m,0)==1 && Tsize_p(m,1) > 1) /* one to many */
		{
			for(Int ii=0;ii<Tsize_p(m,1);ii++)
			{ Pind_p(xptr+ii,0)= xptr;  Pind_p(xptr+ii,1) = yptr + ii;}
		}
		else 
		{
			/* one to one  (check  that xsize == ysize) */
			if (Tsize_p(m,0) == Tsize_p(m,1))
			{
				for(Int ii=0;ii<Tsize_p(m,1);ii++)
				{ Pind_p(xptr+ii,0)= xptr+ii; Pind_p(xptr+ii,1) = yptr + ii;}
			}
			else 
			{
				cout << " Only first set of X values is used" << endl;
				for(Int ii=0;ii<Tsize_p(m,1);ii++)
				{ Pind_p(xptr+ii,0)= xptr;  Pind_p(xptr+ii,1) = yptr + ii;}
				
			}
		}
			
		xptr += Tsize_p(m,0);
		yptr += Tsize_p(m,1);
	}

	if(ddbg) cout << "Num x rows : " << xptr << "   Num y rows : " << yptr << endl;


	/* read the rest of the data */
	
	Int xp=0, yp=0, fyp=0;
	
#if 1
	try
	{
	for(int rc=0;rc<NRows_p;rc++)
	{
		tid.setRownr(rc);
		
		xp=0; yp=0; fyp=0;
		for(int z=0;z<nTens_p;z++) // nTens_p : number of TEN pairs
		{
			tten = xtens_p[z];
			if(tten.isScalar())
			{
				tten.get(tid,xytemp);
				xplotdata_p(xp++,rc) = (T)xytemp;
			}
			else
			{
				tten.get(tid,xtemp);
				xshp_p = xtemp.shape();
				for (Array<Double>::iterator iter=xtemp.begin(); iter!=xtemp.end(); iter++)
					xplotdata_p(xp++,rc) = (T)(*iter);
			}
			
			tten = ytens_p[z];
			if(tten.isScalar())
			{
				tten.get(tid,xytemp);
				yplotdata_p(yp++,rc) = (T)xytemp;

			}
			else
			{
				tten.get(tid,ytemp);
				yshp_p = ytemp.shape();
				for (Array<Double>::iterator iter=ytemp.begin(); iter!=ytemp.end(); iter++)
					yplotdata_p(yp++,rc) = (T)(*iter);
						

				
			}
			
			getFlags(rc);
			
		}// end of for z
	}//end of for rc
	}
	catch(AipsError &x)
	{
		cout << "Error in getData : " << x.getMesg() << endl;
		return -1;
	}
	
#endif	
	if(ddbg)cout << "************** Time to extract data using TEN : " << TMR(tmr) << endl;
	
	if(ddbg)cout << "Shape of xplotdata_p : " << xplotdata_p.shape() << endl;
	if(ddbg)cout << "Shape of yplotdata_p : " << yplotdata_p.shape() << endl;

	
	return 0;
}

/*********************************************************************************/

/* Compute the combined plot range */
template<class T> Int BasePlot<T>::setPlotRange(T &xmin, T &xmax, T &ymin, T &ymax)
{
	if(adbg)cout << "BasePlot :: Set Plot Range for this table " << endl;
	xprange_p.resize(NPlots_p,2);
	yprange_p.resize(NPlots_p,2);
	
	/* compute min and max for each Plot */
	for(int i=0;i<NPlots_p;i++)
	{
		xprange_p(i,0) = 1e+30;
		xprange_p(i,1) = -1e+30;
		yprange_p(i,0) = 1e+30;
		yprange_p(i,1) = -1e+30;
		
		for(int rc=0;rc<NRows_p;rc++)
		{
			if(!theflags_p(Pind_p(i,1),rc)) 
			{
				if(xplotdata_p(Pind_p(i,0),rc) < xprange_p(i,0)) xprange_p(i,0) = xplotdata_p(Pind_p(i,0),rc);
				if(xplotdata_p(Pind_p(i,0),rc) >= xprange_p(i,1)) xprange_p(i,1) = xplotdata_p(Pind_p(i,0),rc);
				
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
	
	if(ddbg) 
	cout << " initial Ranges : [" << xmin << "," << xmax << "] [" << ymin << "," << ymax << "]" << endl;

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

	return 0;
}

/*********************************************************************************/

/* Read in marked flag regions */
template<class T> Int BasePlot<T>::convertCoords(Vector<Vector<T> > &flagmarks)
{
	if(adbg)
	cout << "BasePlot :: MarkFlag co-ordinates to data ranges + labelling " << endl;
	
	nflagmarks_p = flagmarks.nelements();
	if(adbg)cout << "nflagmarks_p : " << nflagmarks_p << endl;
	
	// record to a history list somewhere here before destroying...
	locflagmarks_p.resize(flagmarks.nelements());
	
	locflagmarks_p = flagmarks;
	if(adbg) for(Int i=0;i<nflagmarks_p;i++) cout << locflagmarks_p[i] << endl;
	// change units of locflagmarks_p, if any units changes were used while plotting..

	return 0;
}

/*********************************************************************************/

/* Fill up 'theflags_p' array and optionally write flags to disk */
template<class T> Int BasePlot<T>::flagData(Int diskwrite, Int rowflag)
{
	if(adbg)cout << "BasePlot :: Flag Data and optionally write to disk" << endl;
	
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
				if(xplotdata_p(Pind_p(np,0),nr)>(locflagmarks_p[nf])[0] &&
				   xplotdata_p(Pind_p(np,0),nr)<=(locflagmarks_p[nf])[1] && 
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

/*********************************************************************************/

template<class T> Int BasePlot<T>::clearFlags()
{
	if(adbg)cout << "BasePlot :: clearFlags" << endl;
	
	Array<Bool> flagcol;
	Vector<Bool> rflagcol;
	
	if(zflg==0) 
	{
		zflg=1;
		if(fcol_p)
		{
			Flags_p.attach(SelTab_p,FlagColName_p);
			FlagColShape_p = Flags_p.shape(0);
		}
		if(frcol_p)
		{
			RowFlags_p.attach(SelTab_p,FlagRowName_p);
		}
	}
		
	if(fcol_p)
	{
		flagcol = (Flags_p.getColumn());  
		if(ddbg) cout << "flagcol shape : " << flagcol.shape() << endl;
		flagcol = False;
		Flags_p.putColumn(flagcol);
	}
	
	if(frcol_p)
	{
		rflagcol = RowFlags_p.getColumn();
		if(ddbg) cout << "rflagcol length : " << rflagcol.nelements() << endl;
		rflagcol = False;
		RowFlags_p.putColumn(rflagcol);
	}
		
	theflags_p.set(False);

	if(adbg)cout << "All Flags Cleared !!" << endl;

	return 0;
}

/*********************************************************************************/

template<class T> Matrix<Int> BasePlot<T>::getPlotMap()
{
	if(adbg)cout << "BasePlot :: Get Plot Map" << endl;
	return Pind_p;
}


/*********************************************************************************/
/********************** Private Functions *****************************/
/*********************************************************************************/

/* Clean Up bookkeeping arrays */
template<class T> Int BasePlot<T>::cleanUp()
{
	if(adbg)cout << "BasePlot :: cleanUp" << endl;
	
	nip_p=0;
	ipslice_p.resize(nip_p);
	colnames_p.resize(nip_p);
	nflagmarks_p=0;
	locflagmarks_p.resize(nflagmarks_p);
	
	return 0;
}

/*********************************************************************************/

/* Given a TEN, get an index value - required by mspflagdata */
template<class T> Int BasePlot<T>::getIndices(TableExprNode &ten)
{
	const TableExprNodeRep* tenroot = (ten.getNodeRep());

	if(dbg)cout << "Main Root --> " ; 
	ptTraverse(tenroot); 

	return 0;
}

/*********************************************************************************/

/* Recursive Parse Tree traversal */
template<class T> void BasePlot<T>::ptTraverse(const TableExprNodeRep *tenr)
{
	if(tenr == (TableExprNodeRep*)NULL) {if(dbg) cout << "NULL" << endl ; return;}
	
	if(dbg) cout << "dat : " << tenr->dataType() << " , val : " << tenr->valueType() << " , op : " << tenr->operType() ;
		
	/* Check to see what type this node is */
	Int etype = -1;
	
if(dynamic_cast<const TableExprNodeBinary*>(tenr)!=NULL) {etype=3; if(dbg) cout << " ##########***********It's Binary" << endl;}
if(dynamic_cast<const TableExprNodeMulti*>(tenr)!=NULL) {etype=2; if(dbg) cout << " ##########********It's Multi" << endl;}
if(dynamic_cast<const TableExprNodeSet*>(tenr)!=NULL) {etype=1; if(dbg) cout << " ##########***********It's Set" << endl;}
if(dynamic_cast<const TableExprNodeSetElem*>(tenr)!=NULL) {etype=0; if(dbg) cout << " ##########***********It's SetElem" << endl;}
if(dynamic_cast<const TableExprFuncNodeArray*>(tenr)!=NULL) {etype=4; if(dbg) cout << " ##########***********It's Func node" << endl;}

	switch(etype)
	{
		case 0: /*  SetElem */
			{
			const TableExprNodeSetElem *tense = dynamic_cast<const TableExprNodeSetElem*>(tenr);
			/* Act on this node */
			if(ddbg) cout << "SetElem Node : " << endl; 
		  
			/* get children */
			if(dbg) cout << "Start ---> "; ptTraverse(tense->start());
			if(dbg) cout << "Increment ---> "; ptTraverse(tense->increment());
			if(dbg) cout << "End ---> "; ptTraverse(tense->end());
			break;
			}
		case 1: /* Set */
			{
			const TableExprNodeSet *tens=dynamic_cast<const TableExprNodeSet*>(tenr);
			/* Act on this node */
			if(ddbg) cout << "Set Node : " << endl; 
		  
			/* get children */
			for(Int i=0;i<(Int)tens->nelements();i++)
			{
				const TableExprNodeSetElem tenser = (*tens)[i];
				if(dbg) cout << " Set[" << i << "] start ---> " ; ptTraverse((*tens)[i].start());
				if(dbg) cout << " Set[" << i << "] increment ---> " ; ptTraverse((*tens)[i].increment());
				if(dbg) cout << " Set[" << i << "] end ---> " ; ptTraverse((*tens)[i].end());
			}
			break;
			}
		case 2: /* Multi */
			{
			const TableExprNodeMulti *tenm=dynamic_cast<const TableExprNodeMulti*>(tenr);
			/* Act on this node */
			if(ddbg) cout << "Multi Node : " << endl; 
			
			const TableExprNodeIndex* nodePtr = dynamic_cast<const TableExprNodeIndex*>(tenr);
			if(nodePtr!=0)
			{
				if (nodePtr->isConstant())//  &&  nodePtr->isSingle()) 
				{
					const Slicer& indices = nodePtr->getConstantSlicer();
					// Extract the index from it.
					if(ddbg) cout << "M Index start: " << indices.start() << endl;
					if(ddbg) cout << "M Index end: " << indices.end() << endl;
					if(ddbg) cout << "M Index stride: " << indices.stride() << endl;
					
					ipslice_p.resize(nip_p+1,True);
					ipslice_p[nip_p] = indices;
					nip_p++;
					
				}

			}
		  	
			/* get children */
			PtrBlock<TableExprNodeRep*> tenrarr = tenm->getChildren();
			if(ddbg) cout << "Num elements : " << tenrarr.nelements() << endl ;
			for(Int i=0;i<(Int)tenrarr.nelements();i++)
			{if(dbg) cout << " Child " << i << " ---> " ; ptTraverse(tenrarr[i]);}
			break;
			}
		case 3: /* Binary */
			{
			String cname;
			const TableExprNodeBinary *tenb=dynamic_cast<const TableExprNodeBinary*>(tenr);
			/* Act on this node */
		  	if(ddbg) cout << "Binary Node : " << endl; 
		   
			const TableExprNodeArrayColumn *tenac = dynamic_cast<const TableExprNodeArrayColumn*>(tenb);
			if(tenac != 0){
				cname = tenac->getColumn().columnDesc().name();
				if(ddbg) cout << " Array Column Name : " << cname << endl;
				if(ddbg) cout << "Num elems : " << colnames_p.nelements() << endl;
				colnames_p.resize(nip_p+1,True); // small array of strings
				colnames_p[nip_p] = cname;
			}

			const TableExprNodeColumn *tenc = dynamic_cast<const TableExprNodeColumn*>(tenr);
			if(tenc != 0) {
				cname =  tenc->getColumn().columnDesc().name() ;
				if(ddbg) cout << " Column Name : " << cname << endl;
				colnames_p.resize(nip_p+1,True); // small array of strings
				colnames_p[nip_p] = cname;
				ipslice_p.resize(nip_p+1,True);
				ipslice_p[nip_p] = Slicer(IPosition(1,0),IPosition(1,0));
				nip_p++;
			}
			
			
			/*	
			const TableExprNodeArrayPart* nodePtr = dynamic_cast<const TableExprNodeArrayPart*>(tenr);
			if (nodePtr != 0) {
				// The node represents a part of an array; get its index node.
				const TableExprNodeIndex* inxNode = nodePtr->getIndexNode();
				// If a constant index accessing a single element,
				// get the Slicer defining the index.
				if (inxNode->isConstant() )// &&  inxNode->isSingle()) 
				{
					const Slicer& indices = inxNode->getConstantSlicer();
					// Extract the index from it.
					cout << "B Index start: " << indices.start() << endl;
					cout << "B Index end: " << indices.end() << endl;
					cout << "B Index stride: " << indices.stride() << endl;
				}
			}
			*/

		  	/* get left and right children */
		  	if(dbg) cout << "  Left  ---> "; ptTraverse(tenb->getLeftChild());
		  	if(dbg) cout << "  Right ---> "; ptTraverse(tenb->getRightChild());
			break;
			}
		case 4: /* FuncNodeArray */
			{
			const TableExprFuncNodeArray *tefna=dynamic_cast<const TableExprFuncNodeArray*>(tenr);
			const TableExprFuncNode *tefn = tefna->getChild();
			
			//const TableExprNodeMulti *tenm=dynamic_cast<const TableExprNodeMulti*>(tenr);
			/* Act on this node */
			if(ddbg) cout << "Func Node Array : " << endl; 
			//tenm->show(cout,1);
			
			const TableExprNodeIndex* nodePtr = dynamic_cast<const TableExprNodeIndex*>(tefn);
			if(nodePtr!=0)
			{
				if (nodePtr->isConstant())//  &&  nodePtr->isSingle()) 
				{
					const Slicer& indices = nodePtr->getConstantSlicer();
					// Extract the index from it.
					if(ddbg) cout << "F Index start: " << indices.start() << endl;
					if(ddbg) cout << "F Index end: " << indices.end() << endl;
					if(ddbg) cout << "F Index stride: " << indices.stride() << endl;
				}

			}
		  	
			/* get children */
			PtrBlock<TableExprNodeRep*> tenrarr = tefn->getChildren();
			if(ddbg) cout << "Num elements : " << tenrarr.nelements() << endl ;
			for(Int i=0;i<(Int)tenrarr.nelements();i++)
			{if(dbg) cout << " Child " << i << " ---> " ; ptTraverse(tenrarr[i]);}
			break;
			}
  	}// end of switch
}

/*********************************************************************************/

/* Get Flags */
template<class T> Int BasePlot<T>::getFlags(Int rownum)
{
	Int xp,yp,fyp;
	Slicer fslice;
	Array<Bool> flagit, flagit2; 
	Bool sflag;
	Bool row;
	Int dindex;
	
	if(zflg==0) 
	{
		zflg=1;
		if(fcol_p) 
		{
			Flags_p.attach(SelTab_p,FlagColName_p);
			FlagColShape_p = Flags_p.shape(0);
		}
		if(frcol_p)
		{
			RowFlags_p.attach(SelTab_p,FlagRowName_p);
		}
		if(adbg)
		{
		cout << "FlagColName : " << FlagColName_p<< endl;
		cout << "FlagRowName : " << FlagRowName_p<< endl;
		}
	}
	   if(!fcol_p)
	   {
		if(frcol_p) for(int z=0;z<nTens_p;z++) RowFlags_p.get(rownum,theflags_p(z,rownum));
		else for(int z=0;z<nTens_p;z++) theflags_p(z,rownum)=False;
		return 0;
	   }
	
	flagit.resize(flagitshp_p); 
	
	Int rc = rownum;
	xp=0; yp=0; fyp=0; 
	row = False;
	
	for(int z=0;z<nTens_p;z++) 
	{
		dindex = IndCnt_p[z];
		
		if(!(colshapes_p(dindex).isEqual(FlagColShape_p)))
		{
			if(frcol_p)RowFlags_p.get(rc,theflags_p(fyp++,rc));
		}
		else
		{
			if(frcol_p)RowFlags_p.get(rc,row);
			
			fslice = ipslice_p[dindex];
		
			if( (Flags_p.getSlice(rc,fslice)).shape() == yshp_p )
			{
				flagit = Flags_p.getSlice(rc,fslice);
				for (Array<Bool>::iterator iter=flagit.begin(); iter!=flagit.end(); iter++)
					theflags_p(fyp++,rc) = (*iter)|row;
			}
			else
			{
				if(yshp_p.product()==1)
				{
					sflag = False;
					flagit2.resize( (Flags_p.getSlice(rc,fslice)).shape());
					flagit2 = Flags_p.getSlice(rc,fslice);
					for (Array<Bool>::iterator iter=flagit2.begin(); iter!=flagit2.end(); iter++)
						sflag |= *iter;
					theflags_p(fyp++,rc) = sflag|row;
				}
				else
				{
					cout << "Support for reduction functions not provided yet" << endl;
					cout << " Reading row flags..." << endl;
					if(frcol_p)RowFlags_p.get(rc,theflags_p(fyp++,rc));
					// throw an exception here...
				}
			}
		}
		
		
	}// end of for z
	
	return 0;
}

/*********************************************************************************/

/* Get Flags */
template<class T> Int BasePlot<T>::setFlags(Int rowflag)
{
	if(adbg)cout << "BasePlot :: setFlags - write flags to disk " << endl;

	Int xp,yp,fyp,ryp;
	Slicer fslice;
	Array<Bool> flagit, flagit2; 
	Bool sflag;
	Int dindex;

	if(!fcol_p)
	{
		if(frcol_p) for(int rc=0;rc<NRows_p;rc++) 
				for(int z=0;z<nTens_p;z++) 
					RowFlags_p.put(rc,theflags_p(z,rc));
		return 0;
	}
	flagit.resize(flagitshp_p); 
	for(int rc=0;rc<NRows_p;rc++)
	{
		xp=0; yp=0; fyp=0;ryp=0;
		for(int z=0;z<nTens_p;z++) 
		{
			dindex = IndCnt_p[z];
			if(!(colshapes_p(dindex).isEqual(FlagColShape_p)))
			{
				if(frcol_p)RowFlags_p.put(rc,theflags_p(fyp++,rc));
			}
			else
			{
				fslice = ipslice_p[dindex];
				
				if( (Flags_p.getSlice(rc,fslice)).shape() == yshp_p )
				{
					for (Array<Bool>::iterator iter=flagit.begin(); iter!=flagit.end(); iter++)
						*iter = theflags_p(fyp++,rc) ;
					Flags_p.putSlice(rc,fslice,flagit);
				}
				else
				{
					if(yshp_p.product()==1)
					{
						sflag = theflags_p(fyp++,rc);
						flagit2.resize( (Flags_p.getSlice(rc,fslice)).shape());
						for (Array<Bool>::iterator iter=flagit2.begin(); iter!=flagit2.end(); iter++)
							*iter = sflag;
						Flags_p.putSlice(rc,fslice,flagit2);
					}
					else
					{
						cout << "Support for reduction functions not provided yet" << endl;
						cout << "No flags written " << endl;
						// throw an exception here...
					}
				}
				
				if(rowflag==1) if(frcol_p)RowFlags_p.put(rc,theflags_p(ryp++,rc));
				// set an 'fslice' to fill the whole range
				// use Flags_p.putSlice.....
				if(rowflag==2) 
				{
					cout << "Flag all Chans for current Stokes range - not implemented" << endl;
					// not implemented
					return -1;
				}
				if(rowflag==3) 
				{
					cout << "Flag all Stokes for current Channel range - not implemented" << endl;
					// not implemented
					return -1;
				}
			}
		}// end of for z
	}//end of for rc
	if(adbg)cout << "Flags written to disk..." << endl;

	return 0;
}

	
/*********************************************************************************/
/*********************************************************************************/
} //# NAMESPACE CASA - END 


