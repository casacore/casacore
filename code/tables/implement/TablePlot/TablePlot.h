#ifndef TABLEPLOT_H
#define TABLEPLOT_H

#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include <fstream.h>
#include <math.h>

#include <casa/aips.h>
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
#include <tables/Tables/TableIter.h>
#include <tables/Tables/TableError.h>

#include <casa/namespace.h>

#include "BasePlot.h"
#include "TPLPlot.h"

using namespace casa;

template<class T> class TablePlot 
{
	public:
		TablePlot();
		~TablePlot();
		
		Int SetTableS(String &inTabName);  // Create a table object and add to list.
		Int SetTableT(Table &inTabObj);    // Add this table obj to the list.
		//Int SortTaQL(BasePlot<T> &BP);     // Take in TaQL strings and match with table name.
		Int CreateBP(PtrBlock<BasePlot<T>* > &BPS);     // Instantiate BasePlot for each table and create TENS.
		Int UpDateBP(PtrBlock<BasePlot<T>* > &BPS); // reset links to table objects
		Int CleanBP(PtrBlock<BasePlot<T>* > &BPS);      // Clean up ptrblock leftovers from each BP.
		
		Int GetData(PtrBlock<BasePlot<T>* > &BPS,Vector<String> &datastr);      // Call BasePlot::GetData
		Int PlotData(PtrBlock<BasePlot<T>* > &BPS,TPLPlot<T> &TPLP, Int panel);     // Display the data.
		
		Int MarkFlags(Int panel,TPLPlot<T> &TPLP);    // mark regions to flag.
		Int MarkZoom(Int panel,TPLPlot<T> &TPLP,Int direction);    // mark region to zoom.
		
		Int FlagData(PtrBlock<BasePlot<T>* > &BPS,TPLPlot<T> &TPLP,Int panel, Int diskwrite, Int rowflag);     // flag regions from the plot.
		Int ClearFlags(PtrBlock<BasePlot<T>* > &BPS);   // clear flags. 

		
		Int IterMultiPlotStart(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPLPlot<T> &TPLP, Int npanels,Vector<String> &datastr,Vector<String> &iteraxes);
		Int IterMultiPlotNext(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPLPlot<T> &TPLP,Int &apanels);
		Int IterMultiPlotStop(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPLPlot<T> &TPLP);

		Int nTabs; // Number of simultaneously accessed Tables - sizeof(TABS)
		           // Make this a private variable later.
		

	private:

		Vector<Table> TABS;
		
		Int iTabs;
		Vector<String> TQL;
		Int nTql;

		Int ToFlag; // Set this via a glish event from the DO...
		            // After plotdata, always check the value of this
		            // variable, before calling MarkFlag and FlagData.
		            // This is only for iterplot.
		Block<String> itx;
		Vector<TableIterator> Iters;
		Table temptable;
		Int panelcounter;
		Int NPanels;
		
		Int adbg;
		
};

template class TablePlot<Float>;

#endif

