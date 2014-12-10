//# TiledDataStMan.h: Tiled Data Storage Manager
//# Copyright (C) 1995,1996,1997,1999,2001
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

#ifndef TABLES_TILEDDATASTMAN_H
#define TABLES_TILEDDATASTMAN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Tiled Data Storage Manager.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TiledStMan>TiledStMan</linkto>
//   <li> <linkto class=TSMCube>TSMCube</linkto>
//   <li> <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
//        for a discussion of the maximum cache size
//   <li> <linkto class=Record>Record</linkto>
// </prerequisite>

// <etymology>
// TiledDataStMan is the Tiled Storage Manager for general
// data arrays.
// </etymology>

// <synopsis>
// TiledDataStMan is a derivation from TiledStMan, the abstract
// tiled storage manager class. A description of the basics
// of tiled storage managers is given in the
// <linkto module=Tables:TiledStMan>Tables module</linkto> description.
// <p>
// TiledDataStMan allows the user explicit control over the
// definition and extension of hypercubes by means of the accessor
// class <linkto class=TiledDataStManAccessor>TiledDataStManAccessor</linkto>.
// The user can determine which row should be put in which hypercube,
// so it is possible to put row 0-9 in hypercube A, row 10-29 in B,
// row 30-39 in A again, etc.. This makes it possible to use a tiled
// storage manager for a data column containing data with
// different shapes (e.g. line and continuum data). Actually,
// this storage manager is developed for irregularly shaped
// UV-data, but can be used for any purpose.
// <br>
// Each extensible hypercube uses a file of its own. This means that there
// shouldn't be too many of them, otherwise the number of files may
// get too high.
// <p>
// The TiledDataStMan has the following (extra) properties:
// <ul>
//  <li> When multiple hypercubes are used, one or more id columns have
//       to be used to differentiate between them. The id values must
//       be defined when the hypercube gets added; they cannot be put
//       explicitly.
//  <li> A hypercube can be extensible in its last dimension by setting
//       its last dimension to zero. In that case extendHypercube can
//       be used to extend the hypercube when needed.
//       All fixed sized hypercubes are stored in one file, while there
//       is one file per extensible hypercube.
//  <li> The table must be large enough to accommodate the addition
//       or extension of a hypercube. This means that a sufficient
//       number of rows must be added to the table before a hypercube
//       can be added or extended. It is the responsibility of the user
//       to "synchronize" addition of rows and hypercubes.
//  <li> It is possible to define coordinates for the hypercube axes
//       in several ways:
//       <ul>
//        <li> Use the TiledDataStMan storage manager to hold their values
//             and define the coordinates when adding or extending the
//             hypercube. This is the preferred way.
//        <li> As above, but use explicit puts to write their values.
//             This has to be used when coordinates are defined after
//             the hypercube has been added or extended.
//             Note that several rows may share the same value, so
//             overwriting a value may affect multiple rows.
//        <li> Use another storage manager to hold their values.
//             This is useful when their values depend on other axes,
//             because that cannot be handled by TiledDataStMan.
//       </ul>
//       Note that it is possible to store one coordinate column with
//       TiledDataStMan and another with another storage manager.
// </ul>
// </synopsis> 

// <motivation>
// This tiled storage manager allows one to create and extend hypercubes
// as needed. One has complete control over which row is stored in which
// hypercube.
// </motivation>

// <example>
// The following example shows how to create a TiledDataStMan tiled
// storage manager using the hypercolumn as defined in the table description.
// Furthermore it shows how to use TiledDataStManAccessor
// to add a hypercube, while defining its tile shape, coordinates,
// and id-value.
// The example shows that reading the data back does not require any knowledge
// of the data manager. It's exactly the same if another data manager was used.
// <br>
// The table created contains the equally shaped data columns "Data" and
// "Weight".
// Each cell in those columns contains a 2D array with shape [12,20]. The
// coordinates of those arrays are "Pol" and "Freq".
// The tiled storage manager superimposes two more axes ("Baseline"and "Time")
// on the data resulting in a 4D hypercube with shape [12,20,30,42].
// The table contains 42*30 rows (which has to be equal to the number of
// elements in the superimposed axes).
// <br>
// The tile shape of the hypercube is (arbitrarily) set to [4,5,6,7].
// Of course, any tile shape could be chosen. This tile shape results
// in a tile size of 6720 bytes (4*5*6*7 *(4+4) bytes), which is not
// that large (32768 as tile size is very reasonable). The number of tiles
// is integral in each dimension, so no space is wasted.
// Finally it makes access along the various axes about equally efficient.
// <br>
// Although in this example only one hypercube is added, multiple hypercubes
// are possible, because an id column has been defined. 
// <note role=caution>
// The example uses the global Array function indgen to fill the data
// and coordinate arrays with arbitrary values.
// </note>
// Note that the description of class
// <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
// contains a discussion about the effect of setting the maximum cache size.
//
// <srcblock>
//  // Define the table description and the columns in it.
//  TableDesc td ("", "1", TableDesc::Scratch);
//  td.addColumn (ScalarColumnDesc<float>  ("Time"));
//  td.addColumn (ScalarColumnDesc<float>  ("Baseline"));
//  td.addColumn (ArrayColumnDesc<float>   ("Pol", 1));
//  td.addColumn (ArrayColumnDesc<float>   ("Freq", 1));
//  td.addColumn (ScalarColumnDesc<String> ("Id"));
//  td.addColumn (ArrayColumnDesc<float>   ("Data", 2));
//  td.addColumn (ArrayColumnDesc<float>   ("Weight", 2));
//  // Define the 4-dim hypercolumn with its data, coordinate and id columns.
//  td.defineHypercolumn ("TSMExample",
//			  4,
//			  stringToVector ("Data,Weight"),
//			  stringToVector ("Pol,Freq,Baseline,Time"),
//			  stringToVector ("Id"));
//  
//  // Now create a new table from the description.
//  SetupNewTable newtab("tTiledDataStMan_tmp.data", td, Table::New);
//  // Create a TiledDataStMan storage manager for the hypercolumn
//  // and bind the columns to it.
//  TiledDataStMan sm1 ("TSMExample");
//  newtab.bindAll (sm1);
//  // Create the table with 42*30 rows.
//  Table table(newtab, 42*30);
//  // Create the accessor to be able to add a hypercube to this
//  // storage manager.
//  TiledDataStManAccessor accessor(table, "TSMExample");
//  // Define the values for the coordinates of the hypercube
//  // and put them into the record.
//  Vector<float> timeValues(42);
//  Vector<float> baselineValues(30);
//  Vector<float> freqValues(20);
//  Vector<float> polValues(12);
//  indgen (timeValues);
//  indgen (baselineValues, float(100));
//  indgen (freqValues, float(200));
//  indgen (polValues, float(300));
//  Record hyperDef;
//  hyperDef.define ("Time", timeValues);
//  hyperDef.define ("Baseline", baselineValues);
//  hyperDef.define ("Freq", freqValues);
//  hyperDef.define ("Pol", polValues);
//  // Define the id value as well.
//  hyperDef.define ("Id", "");
//  // Now add the hypercube with the given shape, tile shape,
//  // and coordinate and id values.
//  accessor.addHypercube (IPosition(4,12,20,30,42),
//			   IPosition(4,4,5,6,7), hyperDef);
//  ArrayColumn<float> data (table, "Data");
//  ArrayColumn<float> weight (table, "Weight");
//  Matrix<float> array(IPosition(2,12,20));
//  uInt i;
//  indgen (array);
//  // Write some data into the data columns.
//  for (i=0; i<30*42; i++) {
//	data.put (i, array);
//	weight.put (i, array+float(100));
//	array += float(200);
//  }
//  // Prepare for reading the data back.
//  // Note that time and baseline are in fact scalar columns. They are
//  // superimposed dimensions on the hypercube.
//  ScalarColumn<float> time (table, "Time");
//  ScalarColumn<float> baseline (table, "Baseline");
//  ArrayColumn<float> freq (table, "Freq");
//  ArrayColumn<float> pol (table, "Pol");
//  ScalarColumn<String> id (table, "Id");
//  float fValue;
//  String sValue;
//  for (i=0; i<table.nrow(); i++) {
//      data.get (i, array);
//      weight.get (i, array);
//      pol.get (i, polValues);
//      freq.get (i, freqValues);
//      baseline.get (i, fValue);
//      time.get (i, fValue);
//      id.get (i, sValue);
//  }
// </srcblock>
// Note that in this example an id column was not necessary, because
// there is only one hypercube.
// <p>
// The following example is more advanced. Two (extensible) hypercubes
// are used for line and continuum data. Writing such a data set
// could be done as shown. Reading it back is the same as above.
// <br>
// In this example the data columns contain line and continuum data.
// So there are two types of data, each with their own shape and
// stored in their own (extensible) hypercube. Note that the last
// dimension of the hypercube shape is set to zero (to make extensible),
// but the last tile shape dimension has been filled in,
// because the exact tile shape must be known.
// <br>
// Before each put of the data the appropriate hypercube is extended.
// Also the time has to be put, which is done (as an example) in
// two different ways (using an explicit put and using the extendHypercube).
//
// <srcblock>
//  // Defining TableDesc and storage manager is same as in first example.
//  // Create the table.
//  Table table(newtab);
//  // Create the accessor to be able to add the hypercubes to this
//  // storage manager.
//  TiledDataStManAccessor accessor(table, "TSMExample");
//  // Fill the coordinate values.
//  // Note that the time axis of the hypercube will have length 0 to
//  // make it extensible. Therefore the time coordinate can only be
//  // filled in when the hypercube is extended.
//  Vector<float> baselineValues(30);
//  Vector<float> freqValuesCont(1);
//  Vector<float> freqValuesLine(20);
//  Vector<float> polValues(4);
//  indgen (baselineValues, float(100));
//  indgen (freqValuesLine, float(200));
//  indgen (freqValuesCont, float(200));
//  indgen (polValues, float(300));
//  Record hyperDefLine;
//  hyperDefLine.define ("Baseline", baselineValues);
//  hyperDefLine.define ("Pol", polValues);
//  // Make similar record for line data.
//  // Fill the correct id and frequency values for each type.
//  // Add the 2 hypercubes.
//  Record hyperDefCont (hyperDefLine);
//  hyperDefLine.define ("Id", "L");
//  hyperDefLine.define ("Freq", freqValuesLine);
//  hyperDefCont.define ("Id", "C");
//  hyperDefCont.define ("Freq", freqValuesCont);
//  // Add the hypercubes.
//  // Define their last dimension as zero to make them extensible.
//  accessor.addHypercube (IPosition(4,4,20,30,0),
//			   IPosition(4,4,5,6,7), hyperDefLine);
//  accessor.addHypercube (IPosition(4,4,1,30,0),
//			   IPosition(4,4,1,6,7), hyperDefCont);
//  ScalarColumn<float> time (table, "Time");
//  ScalarColumn<float> baseline (table, "Baseline");
//  ArrayColumn<float> freq (table, "Freq");
//  ArrayColumn<float> pol (table, "Pol");
//  ArrayColumn<float> data (table, "Data");
//  ArrayColumn<float> weight (table, "Weight");
//  Matrix<float> arrayLine(IPosition(2,4,20));
//  Matrix<float> arrayCont(IPosition(2,4,1));
//  indgen (arrayLine);
//  indgen (arrayCont);
//  // Write some data into the data columns.
//  // Alternately line and continuum is written.
//  // Each hypercube requires 30 rows to be added (i.e. nr of baselines).
//  // The last dimension of each hypercube is extended with 1.
//  uInt i, j;
//  uInt rownr = 0;
//  for (i=0; i<42; i++) {
//      if (i%2 == 0) {
//          table.addRow (30);
//          accessor.extendHypercube (1, hyperDefLine);
//          time.put (rownr, float(i));
//          for (j=0; j<30; j++) {
//              data.put (rownr, arrayLine);
//              weight.put (rownr, arrayLine);
//              rownr++;
//          }
//      }else{
//          table.addRow (30);
//          Vector<float> timeValue(1);
//          timeValue(0) = float(i);
//          hyperDefCont.define ("Time", timeValue);
//          accessor.extendHypercube (1, hyperDefCont);
//          time.put (rownr, float(i));
//          for (j=0; j<30; j++) {
//              data.put (rownr, arrayCont);
//              weight.put (rownr, arrayCont);
//              rownr++;
//          }
//      }
//  }
// </srcblock>
// Note that in this example the time is defined in 2 different ways.
// The first one by an explicit put, the second one as a record in
// the extendHypercube call. The second way if the preferred one,
// although it requires a bit more coding.
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TiledDataStMan : public TiledStMan
{
friend class TiledDataStManAccessor;

public:
    // Create a TiledDataStMan storage manager for the hypercolumn
    // with the given name.
    // The hypercolumn name is also the name of the storage manager.
    // The given maximum cache size (default is unlimited) is persistent,
    // thus will be reused when the table is read back. Note that the class
    // <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
    // allows one to overwrite the maximum cache size temporarily.
    // <br>The constructor taking a Record expects fields in the record with
    // the name of the arguments in uppercase. If not defined, their
    // default value is used.
    // <group>
    TiledDataStMan (const String& hypercolumnName,
		    uInt maximumCacheSize = 0);
    TiledDataStMan (const String& hypercolumnName,
		    const Record& spec);
    // </group>

    ~TiledDataStMan();

    // Clone this object.
    // It does not clone TSMColumn objects possibly used.
    DataManager* clone() const;

    // Get the type name of the data manager (i.e. TiledDataStMan).
    String dataManagerType() const;

    // Make the object from the type name string.
    // This function gets registered in the DataManager "constructor" map.
    static DataManager* makeObject (const String& dataManagerType,
				    const Record& spec);

private:
    // Create a TiledDataStMan.
    // This constructor is private, because it should only be used
    // by makeObject.
    TiledDataStMan();

    // Forbid copy constructor.
    TiledDataStMan (const TiledDataStMan&);

    // Forbid assignment.
    TiledDataStMan& operator= (const TiledDataStMan&);

    // Add rows to the storage manager.
    // This will only increase the number of rows. When a hypercube is
    // added or extended, it will be checked whether the number of rows
    // is sufficient.
    void addRow (uInt nrrow);

    // Add a hypercube.
    // The number of rows in the table must be large enough to
    // accommodate this hypercube.
    // The possible id values must be given in the record, while
    // coordinate values are optional. The field names in the record
    // should match the coordinate and id column names.
    // The last dimension in the cube shape can be zero, indicating that
    // the hypercube is extensible.
    void addHypercube (const IPosition& cubeShape,
		       const IPosition& tileShape,
		       const Record& values);

    // Extend the hypercube with the given number of elements in
    // the last dimension.
    // The record should contain the id values (to get the correct
    // hypercube) and optionally coordinate values for the elements added.
    void extendHypercube (uInt incrInLastDim, const Record& values);

    // Get the hypercube in which the given row is stored.
    virtual TSMCube* getHypercube (uInt rownr);

    // Get the hypercube in which the given row is stored.
    // It also returns the position of the row in that hypercube.
    virtual TSMCube* getHypercube (uInt rownr, IPosition& position);

    // Flush and optionally fsync the data.
    // It returns a True status if it had to flush (i.e. if data have changed).
    virtual Bool flush (AipsIO&, Bool fsync);

    // Let the storage manager create files as needed for a new table.
    // This allows a column with an indirect array to create its file.
    virtual void create (uInt nrrow);

    // Read the header info.
    virtual void readHeader (uInt nrrow, Bool firstTime);

    // Update the map of row numbers to cube number plus offset.
    void updateRowMap (uInt cubeNr, uInt incrInLastDim);

    // Check if the table is large enough to hold this
    // hypercube extension.
    void checkNrrow (const IPosition& cubeShape,
		     uInt incrInLastDim) const;


    //# Declare the data members.
    // The map of row number to cube and position in cube.
    Block<uInt> rowMap_p;
    Block<uInt> cubeMap_p;
    Block<uInt> posMap_p;
    // The nr of elements used in the map blocks.
    uInt nrUsedRowMap_p;
    // The row number since the last hypercube extension.
    uInt nrrowLast_p;
};




} //# NAMESPACE CASACORE - END

#endif
