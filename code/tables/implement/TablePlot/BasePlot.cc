
#include "BasePlot.h"

namespace casa {

#define TMR(a) "[User: " << a.user() << "] [System: " << a.system() << "] [Real: " << a.real() << "]"
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))

/* Default Constructor */
template<class T> BasePlot<T>::BasePlot()
{
	dbg=0;	ddbg=0;	adbg=0;
	if(adbg)cout << "BasePlot constructor" << endl;
	nip=0;
	nflagmarks=0;
	xtens.resize(0); ytens.resize(0);
	Pind.resize(0,0); Tsize.resize(0,0);
	colnames.resize(3); ipslice.resize(0);
	IndCnt.resize(0);
	locflagmarks.resize(0);
	xprange.resize(0,0); yprange.resize(0,0);
	FlagColName = "FLAG"; fcol = (Bool)0;
	FlagRowName = "FLAG_ROW"; frcol = (Bool)0;
	
}

/*********************************************************************************/
#if 0
/* Copy Constructor */
template<class T> BasePlot<T>::BasePlot(const BasePlot<T>& other)
{
	cout << "BasePlot copy constructor" << endl;
}
#endif
/*********************************************************************************/

/* Destructor */
template<class T> BasePlot<T>::~BasePlot()
{
	if(adbg)cout << "BasePlot destructor" << endl;
	//delete SelTab;
	CleanUp();
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::Init(Table &tab)
{
	if(adbg)cout << "BasePlot :: Initialize with a Table object" << endl;
	zflg=0;
	fflg=0;
	
	SelTab = tab;
	fcol = SelTab.tableDesc().isColumn(FlagColName);
	frcol = SelTab.tableDesc().isColumn(FlagRowName);
	if(adbg)
	{
		if(fcol) cout << "Column : " << FlagColName << " exists " << endl;
		else cout << "Column : " << FlagColName << " does not exist " << endl;
		if(frcol) cout << "Column : " << FlagRowName << " exists " << endl;
		else cout << "Column : " << FlagRowName << " does not exist " << endl;
	}
	// FLAG is a row_flag, and should be accessed when row_flag is accessed.
	//if(!frcol) FlagRowName = FlagColName; 

	
	if(ddbg)
	{
	cout << "is null 2 : " << SelTab.isNull() << endl;
	cout << "tablename : " << SelTab.tableName() << endl;
	cout << "is changed : " << SelTab.hasDataChanged() << endl;
	}

	return 0;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::CreateTENS(Vector<String> &datastr)
{
	if(adbg)cout << "BasePlot :: Create TableExprNodes from TaQL strings" << endl;
		
	nTStr = datastr.nelements(); // exception if not.
	if(nTStr%2 != 0) 
	{ cout << "Error : Need even number of TaQL strings" << endl; return -1;}
	nTens = nTStr/2; // make sure nTStr is even....
	if(ddbg)cout << "Number of strings : " << nTStr << endl;

	xtens.resize(nTens);
	ytens.resize(nTens);

	IndCnt.resize(nTens);
	
	colnames.resize(0);
	ipslice.resize(0);
	nip=0;
	
	try
	{
	for(Int i=0;i<nTens;i++) // nTens
	{
		xtens[i] = RecordGram::parse(SelTab,datastr[i*2]);
		ytens[i] = RecordGram::parse(SelTab,datastr[i*2+1]);
		
		if( (xtens[i].dataType() != TpDouble) || (ytens[i].dataType() != TpDouble) ) 
		{
			cout << "DataType of TaQL expression is not plottable... "<< endl;
			return -1;
		}
		
		getIndices(xtens[i]);
		IndCnt[i] = nip;// since flags pertain only to y data
		getIndices(ytens[i]);
	}
	}
	catch(AipsError &x)
	{
		cout << " Error : " << x.getMesg() << endl;
		return -1;
	}
	
	Bool tst=(Bool)0;
	IPosition dsh;
	colshapes.resize(0);
	colshapes.resize(colnames.nelements());
	for(Int i=0;i<nip;i++)	
	{
		tst = SelTab.tableDesc().isColumn(colnames(i));
		if(!tst) cout << "Column " << colnames(i) << " does not exist !" << endl;
		ROTableColumn rot(SelTab,colnames(i));
		ColumnDesc cdesc = rot.columnDesc();
		if(cdesc.isArray()) colshapes(i) = rot.shape(0);
		else colshapes(i) = IPosition(1,0);
		if(ddbg)
		{
			cout << "COLUMN : " << colnames(i) << " has shape : " << colshapes(i) << endl;
			cout << "Is arraycol : " << cdesc.isArray() << endl;
			if((ipslice[i].length())[0]==0) cout << "  Scalar Column !! " << endl;
			else cout << "   START : " << ipslice[i].start() << "  END : " << ipslice[i].end() << "  INC : " << ipslice[i].stride() << endl;
		}
	}

	
	return 0;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::GetData()
{
	if(adbg)
	cout << "BasePlot :: Get Data into storage arrays" << endl;

	NRows = (SelTab).nrow();

	/************************ Extract Data from Table ***************/
	if(ddbg) cout << "Extracting data using TEN ... " << endl;
	if(ddbg)tmr.mark();

	TableExprId tid(0);
	Double xytemp;
	Array<Double> xtemp;
	Array<Double> ytemp;
	Array<Bool> arrflag;

	NPlots=0; 

	/* Get data from first row, to figure out the total number of plots
	   Resize xplotdata, yplotdata and Pind accordingly
	   Fill in the numbers for Pind */

	Int xptr=0,yptr=0;
	tid.setRownr(0);
	TableExprNode tten;
	Tsize.resize(nTens,2);

	xshp.resize(0);
	yshp.resize(0);
	flagitshp.resize(0);
	
	try{
	for(int z=0;z<nTens;z++)
	{
		if(nTStr>0)
		{
		tten = xtens[z];
		
		if(tten.isScalar())
		{
			tten.get(0,xytemp);
			Tsize(z,0) = 1;
		}
		else
		{
			tten.get(0,xtemp);
			xshp = xtemp.shape();
			if(ddbg)cout << "Shape of Xaxis data : " << xshp << endl;
			Tsize(z,0) = xshp.product(); 
		}
		xptr+= Tsize(z,0); 
		NPlots = xptr; 
		}
		tten = ytens[z];
		
		if(tten.isScalar())
		{
			if(ddbg) cout << "before scalar get" << endl;
			tten.get(0,xytemp);
			Tsize(z,1) = 1;
			yshp = IPosition(2,1,1);
		}
		else
		{
			if(ddbg) cout << "before vector get" << endl;
			tten.get(0,ytemp);
			yshp = ytemp.shape();
			if(ddbg)cout << "Shape of Yaxis data : " << yshp << endl;
			if(ddbg) cout << "ytemp : " << ytemp << endl;
			Tsize(z,1) = yshp.product(); 
		}
		yptr+=Tsize(z,1); 
		NPlots = yptr; 
		
		flagitshp = yshp;
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

	xplotdata.resize(xptr,NRows);	xplotdata.set((T)0);
	yplotdata.resize(yptr,NRows);	yplotdata.set((T)0);
	theflags.resize(yptr,NRows);	theflags.set((Bool)0);

	Pind.resize(NPlots,2);
	
	/* Fill up Pind */
	xptr=0; yptr=0;
	for(Int m=0;m<nTStr/2;m++)
	{
		if(Tsize(m,0)==1 && Tsize(m,1) > 1) // one to many
		{
			for(Int ii=0;ii<Tsize(m,1);ii++)
			{ Pind(xptr+ii,0)= xptr;  Pind(xptr+ii,1) = yptr + ii;}
		}
		else 
		{
			// one to one  (check  that xsize == ysize)
			if (Tsize(m,0) == Tsize(m,1))
			{
				for(Int ii=0;ii<Tsize(m,1);ii++)
				{ Pind(xptr+ii,0)= xptr+ii; Pind(xptr+ii,1) = yptr + ii;}
			}
			else 
			{
				cout << " Only first set of X values is used" << endl;
				for(Int ii=0;ii<Tsize(m,1);ii++)
				{ Pind(xptr+ii,0)= xptr;  Pind(xptr+ii,1) = yptr + ii;}
				
			}
		}
			
		xptr += Tsize(m,0);
		yptr += Tsize(m,1);
	}

	NxRows = xptr;
	NyRows = yptr;

	if(ddbg) cout << "Num x rows : " << NxRows << "   Num y rows : " << NyRows << endl;

	Int xp=0, yp=0, fyp=0;
	
#if 1
	try
	{
	for(int rc=0;rc<NRows;rc++)
	{
		tid.setRownr(rc);
		
		xp=0; yp=0; fyp=0;
		for(int z=0;z<nTens;z++) // nTens : number of TEN pairs
		{
			tten = xtens[z];
			if(tten.isScalar())
			{
				tten.get(tid,xytemp);
				xplotdata(xp++,rc) = (T)xytemp;
			}
			else
			{
				tten.get(tid,xtemp);
				xshp = xtemp.shape();
				for (Array<Double>::iterator iter=xtemp.begin(); iter!=xtemp.end(); iter++)
					xplotdata(xp++,rc) = (T)(*iter);
			}
			
			tten = ytens[z];
			if(tten.isScalar())
			{
				tten.get(tid,xytemp);
				yplotdata(yp++,rc) = (T)xytemp;

			}
			else
			{
				tten.get(tid,ytemp);
				yshp = ytemp.shape();
				for (Array<Double>::iterator iter=ytemp.begin(); iter!=ytemp.end(); iter++)
					yplotdata(yp++,rc) = (T)(*iter);
						

				
			}
			
			getFlags(rc);
			
		}// end of for z
	}//end of for rc
	}
	catch(AipsError &x)
	{
		cout << "Error in GetData : " << x.getMesg() << endl;
		return -1;
	}
	
#endif	
	if(ddbg)cout << "************** Time to extract data using TEN : " << TMR(tmr) << endl;
	
	if(ddbg)cout << "Shape of xplotdata : " << xplotdata.shape() << endl;
	if(ddbg)cout << "Shape of yplotdata : " << yplotdata.shape() << endl;

	
	return 0;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::SetPlotRange(T &xmin, T &xmax, T &ymin, T &ymax)
{
	if(adbg)cout << "BasePlot :: Set Plot Range for this table " << endl;
	xprange.resize(NPlots,2);
	yprange.resize(NPlots,2);
	
	/* compute min and max for each Plot */
	for(int i=0;i<NPlots;i++)
	{
		xprange(i,0) = 1e+30;
		xprange(i,1) = -1e+30;
		yprange(i,0) = 1e+30;
		yprange(i,1) = -1e+30;
		
		for(int rc=0;rc<NRows;rc++)
		{
			if(!theflags(Pind(i,1),rc)) 
			{
				if(xplotdata(Pind(i,0),rc) < xprange(i,0)) xprange(i,0) = xplotdata(Pind(i,0),rc);
				if(xplotdata(Pind(i,0),rc) >= xprange(i,1)) xprange(i,1) = xplotdata(Pind(i,0),rc);
				
				if(yplotdata(Pind(i,1),rc) < yprange(i,0)) yprange(i,0) = yplotdata(Pind(i,1),rc);
				if(yplotdata(Pind(i,1),rc) >= yprange(i,1)) yprange(i,1) = yplotdata(Pind(i,1),rc);
			}
		}
	}
	
	xmin=0;xmax=0;ymin=0;ymax=0;
	xmin = xprange(0,0);
	xmax = xprange(0,1);
	ymin = yprange(0,0);
	ymax = yprange(0,1);
	
	if(ddbg) 
	cout << " Initial Ranges : [" << xmin << "," << xmax << "] [" << ymin << "," << ymax << "]" << endl;

	/* get a total min,max */

	for(int qq=1;qq<NPlots;qq++)
	{
		xmin = MIN(xmin,xprange(qq,0));
		xmax = MAX(xmax,xprange(qq,1));
	}
	for(int qq=1;qq<NPlots;qq++)
	{
		ymin = MIN(ymin,yprange(qq,0));
		ymax = MAX(ymax,yprange(qq,1));
	}

	return 0;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::ConvertCoords(Vector<Vector<T> > &flagmarks)
{
	if(adbg)
	cout << "BasePlot :: MarkFlag co-ordinates to data ranges + labelling " << endl;
	
	nflagmarks = flagmarks.nelements();
	if(adbg)cout << "nflagmarks : " << nflagmarks << endl;
	
	// record to a history list somewhere here before destroying...
	locflagmarks.resize(flagmarks.nelements());
	
	locflagmarks = flagmarks;
	if(adbg) for(Int i=0;i<nflagmarks;i++) cout << locflagmarks[i] << endl;
	// change units of locflagmarks, if any units changes were used while plotting..

	return 0;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::FlagData(Int diskwrite, Int rowflag)
{
	if(adbg)cout << "BasePlot :: Flag Data and optionally write to disk" << endl;
	
	if(nflagmarks>0)
	{
	for(Int nf=0;nf<nflagmarks;nf++)
	{
		if(ddbg)cout << "*******" << endl;
		if(ddbg)cout << (locflagmarks[nf])[0] << "," << (locflagmarks[nf])[1] << "," << (locflagmarks[nf])[2] << "," << (locflagmarks[nf])[3] << endl;
	}
	
	for(int nr=0;nr<NRows;nr++)
	{
		for(int np=0;np<NPlots;np++)
		{
			for(int nf=0;nf<nflagmarks;nf++)
				if(xplotdata(Pind(np,0),nr)>(locflagmarks[nf])[0] &&
				   xplotdata(Pind(np,0),nr)<=(locflagmarks[nf])[1] && 
				   yplotdata(Pind(np,1),nr)>(locflagmarks[nf])[2] && 
				   yplotdata(Pind(np,1),nr)<=(locflagmarks[nf])[3]) 
					{
						theflags(Pind(np,1),nr) = (Bool)1;
					}
		}
	}
	
	setFlags(diskwrite,rowflag);
	}

	return 0;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::ClearFlags()
{
	if(adbg)cout << "BasePlot :: ClearFlags" << endl;
	
	Array<Bool> flagcol;
	Vector<Bool> rflagcol;
	
	if(zflg==0) 
	{
		zflg=1;
		if(fcol)
		{
			Flags.attach(SelTab,FlagColName);
			FlagColShape = Flags.shape(0);
		}
		if(frcol)
		{
			RowFlags.attach(SelTab,FlagRowName);
			//rfshp = IPosition(1,0);
			//FlagColShape = rfshp;
		}
	}
		
	if(fcol)
	{
		flagcol = (Flags.getColumn());  
		if(ddbg) cout << "flagcol shape : " << flagcol.shape() << endl;
		flagcol = (Bool)0;
		Flags.putColumn(flagcol);
	}
	
	if(frcol)
	{
		rflagcol = RowFlags.getColumn();
		if(ddbg) cout << "rflagcol length : " << rflagcol.nelements() << endl;
		rflagcol = (Bool)0;
		RowFlags.putColumn(rflagcol);
	}
		
	theflags.set((Bool)0);

	if(adbg)cout << "All Flags Cleared !!" << endl;

	return 0;
}

/*********************************************************************************/

template<class T> Matrix<Int> BasePlot<T>::GetPlotMap()
{
	if(adbg)cout << "BasePlot :: Get Plot Map" << endl;
	return Pind;
}

/*********************************************************************************/

template<class T> Int BasePlot<T>::CleanUp()
{
	if(adbg)cout << "BasePlot :: CleanUp" << endl;
	
	nip=0;
	ipslice.resize(nip);
	if(ddbg)cout << "Colnames: " << colnames.nelements() << endl;

	colnames.resize(nip);
	
	nflagmarks=0;
	locflagmarks.resize(nflagmarks);
	
	return 0;
}

/*********************************************************************************/
/********************** Private Functions *****************************/
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
			//tense->show(cout,1);
		  
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
			//tens->show(cout,1);
		  
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
			//const TableExprNodeMulti *tenm=(const TableExprNodeMulti*)(tenr);
			/* Act on this node */
			if(ddbg) cout << "Multi Node : " << endl; 
			//tenm->show(cout,1);
			
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
					
					//ipslice.resize(nip+1,(Bool)0,(Bool)1);
					//ipslice[nip] = new Slicer(indices);
					ipslice.resize(nip+1,(Bool)1);
					ipslice[nip] = indices;
					nip++;
					
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
		  	//tenb->show(cout,1);
		   
			const TableExprNodeArrayColumn *tenac = dynamic_cast<const TableExprNodeArrayColumn*>(tenb);
			if(tenac != 0){
				cname = tenac->getColumn().columnDesc().name();
				if(ddbg) cout << " Array Column Name : " << cname << endl;
				if(ddbg) cout << "Num elems : " << colnames.nelements() << endl;
				colnames.resize(nip+1,(Bool)1); // small array of strings
				colnames[nip] = cname;
			}

			const TableExprNodeColumn *tenc = dynamic_cast<const TableExprNodeColumn*>(tenr);
			if(tenc != 0) {
				cname =  tenc->getColumn().columnDesc().name() ;
				if(ddbg) cout << " Column Name : " << cname << endl;
				colnames.resize(nip+1,(Bool)1); // small array of strings
				colnames[nip] = cname;
				/* also fill in full flag indices */
				//ipslice.resize(nip+1,(Bool)0,(Bool)1);
				//ipslice[nip] = NULL;
				ipslice.resize(nip+1,(Bool)1);
				ipslice[nip] = Slicer(IPosition(1,0),IPosition(1,0));
				nip++;
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

	
	if(zflg==0) 
	{
		zflg=1;
		if(fcol) 
		{
			Flags.attach(SelTab,FlagColName);
			FlagColShape = Flags.shape(0);
		}
		if(frcol)
		{
			RowFlags.attach(SelTab,FlagRowName);
			//rfshp = IPosition(1,0);
			//FlagColShape = rfshp;
		}
		if(adbg)
		{
		//cout << "Shape of Flags : " << fshp << endl;
		cout << "FlagColName : " << FlagColName<< endl;
		cout << "FlagRowName : " << FlagRowName<< endl;
		}
	}
	   if(!fcol)
	   {
	   	//cout << "FLAG column doesn't exist - not considering any flags" << endl;
		if(frcol) for(int z=0;z<nTens;z++) RowFlags.get(rownum,theflags(z,rownum));
		else for(int z=0;z<nTens;z++) theflags(z,rownum)=(Bool)0;
		return 0;
	   }
	
	flagit.resize(flagitshp); //same as yshp
	
	Int rc = rownum;
	xp=0; yp=0; fyp=0; 
	row = (Bool)0;
	
	for(int z=0;z<nTens;z++) // nTens : number of TEN pairs
	{
		dindex = IndCnt[z];
		
		if(!(colshapes(dindex).isEqual(FlagColShape)))
		{
			if(frcol)RowFlags.get(rc,theflags(fyp++,rc));
		}
		else
		{
			if(frcol)RowFlags.get(rc,row);
			
			fslice = ipslice[dindex];
		
			if( (Flags.getSlice(rc,fslice)).shape() == yshp )
			{
				flagit = Flags.getSlice(rc,fslice);
				for (Array<Bool>::iterator iter=flagit.begin(); iter!=flagit.end(); iter++)
					theflags(fyp++,rc) = (*iter)|row;
			}
			else
			{
				if(yshp.product()==1)
				{
					sflag = (Bool)0;
					/*** this RESIZE must be eventually taken out of here !! ***/
					flagit2.resize( (Flags.getSlice(rc,fslice)).shape());
					flagit2 = Flags.getSlice(rc,fslice);
					for (Array<Bool>::iterator iter=flagit2.begin(); iter!=flagit2.end(); iter++)
						sflag |= *iter;
					theflags(fyp++,rc) = sflag|row;
				}
				else
				{
					cout << "Support for reduction functions not provided yet" << endl;
					cout << " Reading row flags..." << endl;
					if(frcol)RowFlags.get(rc,theflags(fyp++,rc));
					// throw an exception here...
				}
			}
		}
		
		
	}// end of for z
	
	return 0;
}

/*********************************************************************************/

/* Get Flags */
template<class T> Int BasePlot<T>::setFlags(Int write,Int rowflag)
{
	if(adbg)cout << "BasePlot :: setFlags - write flags to disk " << endl;

	Int xp,yp,fyp,ryp;
	Slicer fslice;
	Array<Bool> flagit, flagit2; 
	Bool sflag;

	if(!fcol)
	{
		//cout << "FLAG column doesn't exist - not writing any flags to disk" << endl;
		if(frcol) for(int rc=0;rc<NRows;rc++) 
				for(int z=0;z<nTens;z++) 
					RowFlags.put(rc,theflags(z,rc));
		return 0;
	}
	flagit.resize(flagitshp); 
	for(int rc=0;rc<NRows;rc++)
	{
		xp=0; yp=0; fyp=0;ryp=0;
		for(int z=0;z<nTens;z++) 
		{
			dindex = IndCnt[z];
			if(!(colshapes(dindex).isEqual(FlagColShape)))
			{
				if(frcol)RowFlags.put(rc,theflags(fyp++,rc));
			}
			else
			{
				fslice = ipslice[dindex];
				
				if( (Flags.getSlice(rc,fslice)).shape() == yshp )
				{
					for (Array<Bool>::iterator iter=flagit.begin(); iter!=flagit.end(); iter++)
						*iter = theflags(fyp++,rc) ;
					Flags.putSlice(rc,fslice,flagit);
				}
				else
				{
					if(yshp.product()==1)
					{
						sflag = theflags(fyp++,rc);
						/*** this RESIZE must be eventually taken out of here !! ***/
						flagit2.resize( (Flags.getSlice(rc,fslice)).shape());
						for (Array<Bool>::iterator iter=flagit2.begin(); iter!=flagit2.end(); iter++)
							*iter = sflag;
						Flags.putSlice(rc,fslice,flagit2);
					}
					else
					{
						cout << "Support for reduction functions not provided yet" << endl;
						cout << "No flags written " << endl;
						// throw an exception here...
					}
				}
				
				if(rowflag==1) if(frcol)RowFlags.put(rc,theflags(ryp++,rc));
				// set an 'fslice' to fill the whole range
				// use Flags.putSlice.....
				if(rowflag==2) cout << "Flag all Chans for current Stokes range " << endl;
				if(rowflag==3) cout << "Flag all Stokes for current Channel range " << endl;
			}
		}// end of for z
	}//end of for rc
	if(adbg)cout << "Flags written to disk..." << endl;

	return 0;
}

	
/*********************************************************************************/
/*********************************************************************************/
} //#End casa namespace

