#ifndef BASEPLOT_H
#define BASEPLOT_H

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
#include <casa/Arrays/ArrayError.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Slicer.h>

#include <tables/Tables/TableParse.h>
#include <tables/Tables/TableGram.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableLock.h>
#include <tables/Tables/TableIter.h>

#include <tables/Tables/ExprNode.h>
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


#include <casa/namespace.h>

//using namespace casa;

template<class T> class BasePlot 
{
	public:
		BasePlot();  // allow table obj.
		//BasePlot(const BasePlot<T>& other);
		~BasePlot();
		BasePlot<Float>& operator=(const BasePlot<T>&)//{return *this;}
		{cout << "BasePlot operator=" << endl; return *this;}

		Int Init(Table &tab);   // Take in table obj here.
		                    // If table obj already exists, then just shut and 
		                    // reassign.
		Int CreateTENS(Vector<String> &datastr);   // Create TableExprNodes for all TaQL strings.
		Int GetData();      // Read data from the table and fill up storage arrays.
		Int SetPlotRange(T &xmin, T &xmax, T &ymin, T &ymax); // Set plot range (all plots for this table).
		Int ConvertCoords(Vector<Vector<T> > &flagmarks);// MarkFlags co-ordinates to data ranges + labelling.
		Int FlagData(Int diskwrite, Int rowflag);     // Fill in flags in the storage arrays.
		Int ClearFlags();   // Clear all flags (history stuff comes in here)
		Matrix<Int> GetPlotMap();   // To be called by the plotter to identify plotdata
		                    // arrays and corresponding sizes.
		Int CleanUp();      // Reset lists and pointers...
		
		Int NRows,NPlots; // make private later.
		Matrix<T> xplotdata,yplotdata;
		Matrix<Bool> theflags;
		
		Table SelTab;

	private:/* Everything pertaining to a single table */
		Int getIndices(TableExprNode &ten);
		void ptTraverse(const TableExprNodeRep *tenr);
		Int getFlags(Int rownum);
		Int setFlags(Int diskwrite,Int rowflag);

		Int NxRows,NyRows;
		
		Int nTStr,nTens;
		Vector<TableExprNode> xtens;
		Vector<TableExprNode> ytens;
		
		Matrix<Int> Pind,Tsize;
		IPosition xshp,yshp,flagitshp;

		Vector<String> colnames;
		Vector<Slicer> ipslice;
		Vector<IPosition> colshapes;
		Int nip; // number of pairs of colnames and indices.
		Int dindex;
		Vector<Int> IndCnt;
		
		ArrayColumn<Bool> Flags;
		ScalarColumn<Bool> RowFlags;
		String FlagColName,FlagRowName;
		IPosition FlagColShape;
		Bool fcol,frcol;
		
		Vector<Vector<T> > locflagmarks;
		Int nflagmarks;

		Int dbg,ddbg,adbg;
		Timer tmr;
		Int zflg,fflg;

		Matrix<T> xprange, yprange;

		Int junk;
};

//template class BasePlot<Float>;

#endif

