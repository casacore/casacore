
#include "TablePlot.h"
#include "BasePlot.h"

namespace casa {

/* Default Constructor */
template<class T> TablePlot<T>::TablePlot()
{
	adbg=0;
	if(adbg)cout << "TablePlot constructor" << endl;
	TABS.resize(0);
	TQL.resize(0);
	nTabs=0; iTabs=0; ToFlag=0;
	NPanels=1;
}

/*********************************************************************************/

/* Destructor */
template<class T> TablePlot<T>::~TablePlot()
{
	if(adbg)cout << "TablePlot destructor" << endl;
}

/*********************************************************************************/

/* Create a Table obj from the string inTabName - Add onto list of Table objects */
template<class T> Int TablePlot<T>::SetTableS(String &inTabName)
{
	if(adbg)cout << "TablePlot :: SelectData - Table string : " << iTabs << endl;
	
	if(nTabs<=0) cout << "Throw Exception here" << endl;
	if((Int)TABS.nelements() != nTabs) TABS.resize(nTabs);
	try
	{
	TABS[iTabs] = Table(inTabName,Table::Update);
	}
	catch(TableError &x)
	{
		cout << "Table Error : " << x.getMesg() << endl;
		return -1;
	}

	iTabs++;
		
	return 0;
}

/*********************************************************************************/

/* Add inTabObj directly onto the list of Table objects : TABS */
template<class T> Int TablePlot<T>::SetTableT(Table &inTabObj)
{
	if(adbg)cout << "TablePlot :: SelectData - Table object : " << iTabs << endl;
	
	if(nTabs<=0) cout << "Throw Exception here" << endl;
	if((Int)TABS.nelements() != nTabs) TABS.resize(nTabs);
	TABS[iTabs] = inTabObj;
	iTabs++;

	return 0;
}

/*********************************************************************************/
/*
template<class T> Int TablePlot<T>::SortTaQL(BasePlot<T> &BP)
{
	cout << "TablePlot :: SortTaQL - get and match TaQL strings to Tables" << endl;
	return 0;
}
*/

/*********************************************************************************/

template<class T> Int TablePlot<T>::CreateBP(PtrBlock<BasePlot<T>* > &BPS)
{
	if(adbg)cout << "TablePlot :: Create BP" << endl;
	
	if((Int)BPS.nelements()==0) 
	{
		BPS.resize(nTabs);
		for(Int i=0;i<nTabs;i++) BPS[i] = new BasePlot<T>();
	}
	
	return 0;
}

/*********************************************************************************/

template<class T> Int TablePlot<T>::UpDateBP(PtrBlock<BasePlot<T>* > &BPS)
{
	if(adbg)cout << "TablePlot :: UpDate BP" << endl;
	
	// for all Tables
	for(Int i=0;i<nTabs;i++) 
	{
		BPS[i]->Init(TABS[i]); // initialise each BP - TableObj
		if(adbg)cout << "Inited BP : " << i << endl;
	}
	
	if(adbg)cout << "TablePlot :: Finished updating BP " << endl;
	return 0;
}

/*********************************************************************************/

template<class T> Int TablePlot<T>::CleanBP(PtrBlock<BasePlot<T>* > &BPS)
{
	if(adbg)cout << "TablePlot :: Cleanup BP" << endl;
	// for all Tables
	for(Int i=0;i<nTabs;i++) BPS[i]->CleanUp();
	return 0;
}

/*********************************************************************************/

template<class T> Int TablePlot<T>::GetData(PtrBlock<BasePlot<T>* > &BPS,Vector<String> &datastr)
{
	if(adbg)cout << "TablePlot :: Create TENS and Extract Data" << endl;
	
	TQL.resize(datastr.shape());
	TQL = datastr;
	
	nTql = TQL.nelements();

	for(Int i=0;i<nTabs;i++)
	{
		if(BPS[i]->CreateTENS(TQL) == -1) 
			return -1;
	}
	
	// later figure out some mapping, and correct TaQL string vectors to each BP.
	
	//for all Tables
	for(Int i=0;i<nTabs;i++) 
	{
		if(BPS[i]->GetData() == -1) 
			return -1;
	}
	
	return 0;
}

/*********************************************************************************/

template<class T> Int TablePlot<T>::PlotData(PtrBlock<BasePlot<T>* > &BPS,TPLPlot<T> &TPLP,  Int panel)
{
	if(adbg)cout << "TablePlot :: Plot Data" << endl; 

	TPLP.SetPlotRange(BPS,panel); 
	TPLP.PlotData(BPS,panel);
	return 0;
}

/*********************************************************************************/

template<class T> Int TablePlot<T>::MarkFlags(Int panel, TPLPlot<T> &TPLP)
{
	if(adbg)cout << "TablePlot :: Mark Flag Regions" << endl;
	Int ret = TPLP.MarkFlags(panel);
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

template<class T> Int TablePlot<T>::MarkZoom(Int panel, TPLPlot<T> &TPLP, Int direction)
{
	if(adbg)cout << "TablePlot :: Mark Zoom Regions" << endl;
	Int pan=1;
	pan = TPLP.MarkZoom(panel,direction);
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

template<class T> Int TablePlot<T>::FlagData(PtrBlock<BasePlot<T>* > &BPS,
			TPLPlot<T> &TPLP, Int panel, Int diskwrite, Int rowflag)
{
	if(adbg)cout << "TablePlot :: Flag Data" << endl;

	TPLP.SetFlagRegions(BPS,panel); 
	if(adbg)cout << "nTabs : " << nTabs << endl;
	for(Int i=0;i<nTabs;i++) BPS[i]->FlagData(diskwrite,rowflag);
		
	TPLP.SetPlotRange(BPS,panel);
	TPLP.PlotData(BPS,panel); 
	
	//TPLP.ClearPlot();
	
	return 0;
}

/*********************************************************************************/

template<class T> Int TablePlot<T>::ClearFlags(PtrBlock<BasePlot<T>* > &BPS)
{
	if(adbg)cout << "TablePlot :: Clear Flags" << endl;
	
	for(Int i=0;i<nTabs;i++) BPS[i]->ClearFlags();
	
	return 0;
}

/*********************************************************************************/

/*********************************************************************************/
template<class T> Int TablePlot<T>::IterMultiPlotStart(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPLPlot<T> &TPLP, Int npanels,Vector<String> &datastr, Vector<String> &iteraxes)
{
	if(adbg)cout << "TablePlot :: IterMultiPlotStart " << endl;
#if 1
	NPanels = npanels;
	
	ATBPS.resize(NPanels);

	for(Int i=0;i<NPanels;i++) 
	{
		ATBPS[i] = new PtrBlock<BasePlot<T>* >();
		CreateBP(*ATBPS[i]);
	}
	
	
	itx.resize(iteraxes.nelements()); // remove !
	for(Int i=0;i<(Int)iteraxes.nelements();i++) 
	{
		itx[i] = iteraxes[i]; 
	}
	Iters.resize(nTabs);
	try
	{
	for(Int i=0;i<nTabs;i++) Iters[i] = TableIterator(TABS[i],itx);
	}
	catch(TableError &x)
	{
		cout << "Iteraxis Error : " << x.getMesg() << endl;
		return -1;
	}
	panelcounter=0;

#endif
	
	return 0;
}

/*********************************************************************************/
template<class T> Int TablePlot<T>::IterMultiPlotNext(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPLPlot<T> &TPLP,Int &apanels)
{
	if(adbg)cout << "TablePlot :: IterMultiPlotNext " << endl;
#if 1
	if(Iters[0].pastEnd()) //check for end of table from first table in list.
	{
		IterMultiPlotStop(ATBPS,TPLP);
		return 0;
	}
	else
	{
		for(Int i=0;i<panelcounter;i++) CleanBP(*ATBPS[i]);
		panelcounter=0;

		while(panelcounter<NPanels && !Iters[0].pastEnd())
		{
			for(Int i=0;i<nTabs;i++) 
			{
				temptable = Iters[i].table();
				(*ATBPS[panelcounter])[i]->Init(temptable); // init each BP with Tobj 
				Iters[i].next();
			}
		panelcounter++;
		}
	apanels=panelcounter;
	}

#endif
	
	return 1;
}
/*********************************************************************************/
template<class T> Int TablePlot<T>::IterMultiPlotStop(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPLPlot<T> &TPLP)
{
	if(adbg)cout << "TablePlot :: IterMultiPlotStop " << endl;
	
	for(Int i=0;i<panelcounter;i++) CleanBP(*ATBPS[i]);
	
 	Iters.resize(0);
	
	for(Int i=0;i<(Int)ATBPS.nelements();i++) 
	{
		delete ATBPS[i];
	}
	
	ATBPS.resize(0);
	
	return 0;
}

/*********************************************************************************/
} //#End casa namespace

