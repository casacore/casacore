//# TableMeasures.h: Create Measure and Quantum columns Tables.
//# Copyright (C) 1996,1997,1998
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

#if !defined (AIPS_TABLEMEASURES_H)
#define AIPS_TABLEMEASURES_H

#include <trial/TableMeasures/ArrayMeasColumn.h>
#include <trial/TableMeasures/ArrayQuantColumn.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/ScalarQuantColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <trial/TableMeasures/TableMeasValueDesc.h>
#include <trial/TableMeasures/TableQuantumDesc.h>

// <module> 
//
// <summary> 
// TableMeasures.h: Create and use columns of Quantums and Measures in Tables.
// </summary>

// <prerequisite>
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
// </prerequisite>

// <reviewed reviewer="" date="" demos="" tests="">

// <etymology>
// Table columns containing Measures.
// </etymology>
//
// <synopsis>
// Using the classes in the TableMeasures module it is possible to store 
// Measures and Quantums directly in Table columns.  The following discussion
// focuses primarily on Measure columns.  For the specific differences between 
// Measure and Quantum columns one should refer to the descriptions in the
// <linkto class="TableQuantumDesc">TableQuantumDesc</linkto>,
// <linkto class="ScalarQuantColumn">ScalarQuantColumn</linkto> and 
// <linkto class="ArrayQuantColumn">ArrayQuantColumn</linkto> classes.
//
// <h3>Table measure column descriptors</h3>
// Creating Measure columns is a two step process.  Firstly a
// <linkto class="TableMeasDesc">TableMeasDesc</linkto> is used define
// an exiting Table column as a Measure column.  After this the column can be 
// accessed as a Measure column through which Measures can be added to or read 
// from.
//
// The <linkto class="TableMeasDesc">TableMeasDesc</linkto> class and its
// helper classes are used to describe a measure column's charateristics, i.e., 
// the characteristics of the measures that will be stored in the column. These
// characteristics include the type of the measures that are to be stored in the
// column (<src>MEpoch</src>, <src>MDirection</src>, etc.) and their reference 
// and offset. The TableMeasDesc describes these charateristics and associates 
// an existing table column with this description. This association is then 
// made permanent.
//
// The "existing table column" is the name of a column which has already been
// added to the table's table descriptor.  Apart from knowing the name of this
// column the column must be an <src>ArrayColumn</src> of type 
// <src>Double</src>.
//
// The two <src>TableMeasDesc</src> helper classes, 
// <linkto class="TableMeasRefDesc">TableMeasRefDesc</linkto> and
// <linkto class="TableMeasOffsetDesc">TableMeasRefDesc</linkto>, are used to
// define the reference and offset component of the measure column. 
// These components can be defined to be static (i.e, all measures in the 
// column have the same reference and/or offset) or they can be variable 
// (for each measure stored in the column its reference and/or offset is 
// also stored).  To create a Measure column with a variable reference a
// <src>TableMeasRefDesc</src> constructor exists which 
// accepts the name of an existing <src>ScalarColumn<Int></src> as a
// parameter, which is used to store the reference of each measure
// put into the column.  
//
// Setting up a variable offset is slightly more complicated.  As a measure's 
// offset is itself a measure a variable offset must itself be a measure
// column.  The constructor to provide a variable offset accepts a previously
// declared <src>TableMeasDesc</src> as a parameter.
//
// <srcBlock>
// #include <aips/Measures.h>
// #include <aips/Tables.h>
// #include <trial/TableMeasures/TableMeasDesc.h>
// #include <trial/TableMeasures/TableMeasValueDesc.h>
// #include <trial/TableMeasures/TableMeasOffsetDesc.h>
// #include <trial/TableMeasures/TableMeasRefDesc.h>
//
// int main()
// {
//    // Need a table to work with.
//    TableDesc td("Meas table decriptor", "1", TableDesc::New);
//    td.comment() = "A table for demonstrating measure columns.";
//    
//    // The underlying column for storing the Measures must be of type
//    // Array<Double>
//    // Create an underlying column for each Measure column.
//    ArrayColumnDesc<Double> cdTime("Time1", "An MEpoch column");
//    ArrayColumnDesc<Double> cdTimeArr("Time1Arr", "An MEpoch array column");
//
//    td.addColumn(cdTime);
//    td.addColumn(cdTimeArr);
//    
//    // A TableMeasDesc for a simple MEpoch column "Time1" with reference
//    // MEpoch::TAI.  The intension here is that all measures will have this 
//    // same reference.
//    TableMeasRefDesc tmrd(MEpoch::TAI);
//    // The TableMeasValueDesc actually contains the reference to the measure
//    // column.
//    TableMeasValueDesc tmvd(td, "Time1");    
//    // The TableMeasDesc defines the measures type.
//    TableMeasDesc<MEpoch> tmdMEpoch(tmvd, tmrd);
//    // write() makes it permanent.
//    tmdMEpoch.write(td); 
//
//    // Now a TableMeasDesc for an array measure column.  TableMeasDesc
//    // for array and scalar columns are actually the same.
//    // This MEpoch column descriptor specifies a fixed offset.
//    // The offset is just a measure.
//    MEpoch obsTime(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)), 
//        MEpoch::UTC);
//    TableMeasOffsetDesc tmodObsTime(obsTime);
//    // Reference MEpoch::LAST.
//    TableMeasRefDesc tmrdObs(MEpoch::LAST, tmodObsTime);
//    TableMeasValueDesc tmvdObs(td, "Time1Arr");    
//    TableMeasDesc<MEpoch> tmdObs(tmvdObs, tmrdObs);
//    tmdObs.write(td);
// 
//    ...
// </srcBlock>
// For variable references a ScalarColumn of type <src>Int</src> is 
// needed.  For example:
// <srcBlock>
//    ...
//    ScalarColumnDesc<Int> cdVarRef("RefCol", "Variable MeasRef column");
//    td.addColumn(cdVarRef);
//    ...
//    TableMeasRefDesc tmrdObs("RefCol", tmodObsTime);
//    ...
// </srcBlock>
// As stated above a variable offset column needs its own 
// <src>TableMeasDesc</src>.  From the above example the TableMeasDesc 
// tmdMEpoch could be used as a variable offset column for tmdObs by declaring 
// the <src>TableMeasOffsetDesc</src> as follows:
// <srcBlock>
//    ...
//    TableMeasOffsetDesc tmodObsTime(tmdMEpoch);
//    // etc
// </srcBlock>
//
// <h3>Scalar and Array measure columns</h3>
// Accessing a measure column for the purposes of adding and retrieving 
// measures is done via 
// <linkto class="ScalarMeasColumn">ScalarMeasColumn</linkto> and 
// <linkto class="ArrayMeasColumn">ArrayMeasColumn</linkto> objects. (As with
// other column objects read only versions of these classes exist.  These are
// <src>ROScalarMeasColumn</src> and <src>ROArrayMeasColumn</src>.)  These
// object are templated on measure type so declaring a scalar measure
// column would normally look as follows:
// <srcBlock>
//    ScalarMeasColumn<MEpoch> meCol(table, "columnName");
//    // or
//    ROArrayMeasColumn<MDirection> mdCol(table, "MDcolumnName");
// </srcBlock>
//
// Typedefs exist for these constructors making declarations a little
// simpler:
// <srcBlock>
//    MEpochScaCol meCol(table, "columnName");
//    // and
//    ROMDirectionArrCol mdCol(table, "MDcolumnName");
// </srcBlock>
//
// Measures can be added to a column using the put() member and
// retrieved with get():
// <srcBlock>
//    // add an MEpoch to row 3
//    // Need an MEpoch to add
//    MEpoch me(MVEpoch(Quantity(13.45, "h"), MEpoch::Ref(MEpoch::TAI)));
//    meCol.put(3, me);
//
//    // retrieve first 10 measures from the columns and send them to
//    // cout.
//    for (uInt i=0; i<10; i++) {
//        cout << meCol(i) << endl;
//    }
//
//    // get the MDirection array stored in row 1. 
//    Vector<MDirection> mdArr;
//    mdCol(mdArr, 1);
// </srcBlock>
//
// <h3>Non-variable references and offsets</h3>
// Users should be aware that measure columns with non-variable reference
// and/or offset components make no attempt to prevent measures with the
// incorrect reference or offset being added to the column.  Nor are
// these measures converted in any way in an attempt to conform to the
// local reference before they are stored.  All measures added to such
// columns are simply assumed to have the correct reference and offset and
// these components are simply ignored.
//
// <h3>Variable references and offsets with Array Measure Columns</h3>
// The variable components of Array Measure columns can be set up to be either
// variable per row or variable per element of array per row.  This distinction 
// is determined by the type of the underlying column specified for these 
// variable components when the <src>TableMeasDesc</src> for the column was
// first declared.  Specifying a reference with a ScalarColumn will produce an
// and Array Measure Column with references which vary per row.  All measures
// in an array of measures retrieved from such a column will have the same
// reference.  Alternatively, specifying an ArrayColumn when setting up the
// reference component of the TableMeasDesc will allow each measure in
// in array to have its own reference component.
//
// <motivation>
// Useful to be able to store Quantums and Measures in tables.
// </motivation>

// <todo asof="1997/12/24">
//   <li> It would be useful to be able to store measure frames in tables too.
// </todo>

// </module>

#endif
