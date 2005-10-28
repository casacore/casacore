//# CrossPlot.h: Basic table access class for the TablePlot (tableplot) tool
//#              to plot across rows for an arrya-column.
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
//# You should have receied a copy of the GNU Library General Public License
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


#ifndef CROSSPLOT_H
#define CROSSPLOT_H

//# Includes

#include <casa/aips.h>
#include <casa/iostream.h>
#include <casa/OS/Timer.h>

#include <tables/TablePlot/BasePlot.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Basic table access class for the TablePlot (tableplot) tool
// - derived from BasePlot. Used for plotting across rows when 
// a table column is an arraycolumn.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//#! Classes or concepts you should understand before using this class.
// BasePlot
// </prerequisite>

// <etymology>
// CrossPlot is a class derived from BasePlot, and allows plots in a direction
// transposed to that allowed by BasePlot. 
// </etymology>

// <synopsis>
// Class CrossPlot holds the same data structures as does BasePlot, but
// provides a transposed view of arraycolumn data to the TPPlotter class
// which queries it to accumulate data to plot.
// 
// </synopsis>

// <example>
// <srcblock>
// // Instantiate BasePlot 
// CrossPlot<T> CP();
// CP.Init(Table&);
// CP.CreateTENS(Vector<String> &TaQL);
// CP.GetData();
// ... followed by TPPlotter 'setPlotRange' and 'plotData' calls
// </srcblock>
// </example>

// <motivation>
// This class was written to allow transposed plots for array-column data,
// where the 'x-axis' of the plot is by default the column index of the
// arraycolumn.
// For a measurement set type table, this corresponds to plotting data as
// a function of spectral channel. It is derived from BasePlot because the
// data read/write/storage mechanisms and data structures are identical to that
// in BasePlot. The only difference is the view presented to the TPPlotter
// class. The TPPlotter class does not distinguish between BasePlot and
// CrossPlot. In a future version, Histogram plots will also be implemented
// in this manner.
// </motivation>

// <templating arg=T>
//    <li>
// </templating>

// <thrown>
//    <li>
//    <li>
// </thrown>


// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


template<class T> class CrossPlot : public BasePlot<T> {
	public:
		// Constructor
		CrossPlot();  

		// Destructor
		~CrossPlot();

		// This function is called from TPPlotter::setPlotRange().
		// Set plot range (all plots for this table).
		// Scan the data storage arrays to compute data ranges. In the case 
		// of overlay plots (due to Array TpDouble TaQL results), combined
		// data ranges for this tables data are computed.
		// This function requires that all stored data arrays be traversed.
		// This can get expensive for large number of data points. It is
		// assumed that for such a large number of data points, plotting
		// could broken down into chunks using an iteration axis.
		Int setPlotRange(T &xmin, T &xmax, T &ymin, T &ymax, Int useflags); 
		
		// Fill in flags in the storage arrays.
		// The data storage arrays are traversed and flags for all
		// data values falling within the chosen flag regions are
		// set to true. If diskwrite=1, updated flags are written to disk.
		// If diskwrite=0, the flags are not written to disk, but will
		// be applied to plots that use the current instance of BasePlot.
		// If rowflag=1, a the FLAG_ROW column is set (if it exists) in
		// addition to the individual flags in the FLAG column (if it exists).
		Int flagData(Int diskwrite, Int rowflag);     
		
		// Create TableExprNodes from input TAQL strings.
		Int createXTENS(Vector<String> &datastr);   
		
		// Query the internal structures for X data values
		T getXVal(Int pnum, Int col);
		
		// Query the internal structures for Y data values
		T getYVal(Int pnum, Int col);
		
		// Query the internal structures for flag values
		Bool getYFlags(Int pnum, Int col);
		
		// Query for the number of points per plot
		Int getNumRows();

		// Query for the number of plots
		Int getNumPlots();

	private:
		
		// Read X data from the table. For CrossPlots this corresponds
		// to filling the X data arrays with channel indices.
		// For 'tid' set to row 0, the x storage arrays are created 
		// and filled. Subsequent calls to getXData are ineffectual,
		// since the accessed channel indices can be filled in after
		// reading only the first row of the table, 
		// but are required to preserve the format used in BasePlot
		// which reads a value from each row in the table.
		Int getXData(TableExprId &tid);

		using BasePlot<T>::dbg;
		using BasePlot<T>::ddbg;
		using BasePlot<T>::adbg;
		using BasePlot<T>::nflagmarks_p;
		using BasePlot<T>::ytens_p;
		using BasePlot<T>::Pind_p;
		using BasePlot<T>::Tsize_p;
		using BasePlot<T>::ipslice_p;
		using BasePlot<T>::locflagmarks_p;
		using BasePlot<T>::xprange_p;
		using BasePlot<T>::yprange_p;
		using BasePlot<T>::nTens_p;
		using BasePlot<T>::NRows_p;
		using BasePlot<T>::tmr;
		using BasePlot<T>::NPlots_p;
		using BasePlot<T>::nTens_p;
		using BasePlot<T>::xplotdata_p;
		using BasePlot<T>::yplotdata_p;
		using BasePlot<T>::theflags_p;
		using BasePlot<T>::xpd_p;
		using BasePlot<T>::ypd_p;
		using BasePlot<T>::xptr_p;
		using BasePlot<T>::yptr_p;
		using BasePlot<T>::pType_p;
		
		using BasePlot<T>::setFlags;

		//using BasePlot<T>::nip_p;
		//using BasePlot<T>::xtens_p;
		//using BasePlot<T>::IndCnt_p;
		//using BasePlot<T>::SelTab_p;
		//using BasePlot<T>::colnames_p;
		//using BasePlot<T>::colshapes_p;
		//using BasePlot<T>::xshp_p;
		//using BasePlot<T>::yshp_p;
		//using BasePlot<T>::flagitshp_p;
		//using BasePlot<T>::FlagColName_p;
		//using BasePlot<T>::fcol_p;
		//using BasePlot<T>::FlagRowName_p;
		//using BasePlot<T>::frcol_p;
		//using BasePlot<T>::nTStr_p;
		
};

} //# NAMESPACE CASA - END 

#ifndef AIPS_NO_TEMPLATE_SRC
#include <tables/TablePlot/CrossPlot.cc>
#endif //# AIPS_NO_TEMPLATE_SRC
#endif
