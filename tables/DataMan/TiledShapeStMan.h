//# TiledShapeStMan.h: Tiled Data Storage Manager using the shape as id
//# Copyright (C) 1998,2000,2001,2002
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

#ifndef TABLES_TILEDSHAPESTMAN_H
#define TABLES_TILEDSHAPESTMAN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Tiled Data Storage Manager using the shape as id.
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
// </prerequisite>

// <etymology>
// TiledShapeStMan is the Tiled Storage Manager where the shape is used as id
// to support variable shaped arrays.
// </etymology>

// <synopsis>
// TiledShapeStMan is a derivation from TiledStMan, the abstract
// tiled storage manager class. A description of the basics
// of tiled storage managers is given in the
// <linkto module=Tables:TiledStMan>Tables module</linkto> description.
// <p>
// TiledShapeStMan creates a hypercube for each different shape of
// the data arrays. For example, if a table contains line and continuum
// data of an observation, it results in 2 hypercubes.
// TiledShapeStMan does it all automatically, so it is much easier to use
// than class <linkto class=TiledDataStMan>TiledDataStMan</linkto>.
// <br>TiledShapeStMan is meant for columns with not too many different
// shapes, otherwise looking for a matching hypercube may take too long.
// When many different shapes are used, class
// <linkto class=TiledCellStMan>TiledCellStMan</linkto>
// should be used instead.
//
// TiledShapeStMan has the following (extra) properties:
// <ul>
//  <li> It can only handle columns containing arrays, thus not scalars.
//  <li> Addition of a row sets the appropriate data arrays
//       in that row temporarily to an empty hypercube.
//       However, if the data arrays have a fixed shape, the
//       shape is known and the hypercube can be generated immediately.
//       Note that for a fixed shape column, one can as well use class
//       <linkto class=TiledColumnStMan>TiledColumnStMan</linkto>.
//  <li> When the shape of the data array in a row is set for the
//       first time, it is known which hypercube should be used or
//       if a new hypercube has to be created.
//       <br>Note that is is not possible to change the shape of an array.
//       If that is needed, TiledCellStMan should be used instead.
//       <br>Note that a hypercolumn has a given dimensionality, so each
//       data cell in the hypercolumn has to match that dimensionality.
//  <li> Although there are multiple hypercubes, an id value is not needed.
//       The shape serves as the id value.
//  <li> Coordinates for the hypercubes can be defined and (of course)
//       their shapes have to match the hypercube shape.
//       Their values have to be put explicitly (so it is not possible
//       to define them via an addHypercube call like in
//       <linkto class=TiledDataStMan>TiledDataStMan</linkto>).
//       It is possible to put the coordinate values before or after
//       the shape of the data array in that row is defined.
//  <li> It is possible to define a (default) tile shape in the
//       TiledShapeStMan constructor. When setting the shape of the
//       array in a row (using <linkto class=ArrayColumn>
//       ArrayColumn::setShape</linkto>), it is possible to override
//       that default for the hypercube in this particular row.
//       However, since the tile shape is only used when creating
//       a hypercube, using an overriding tile shape makes only
//       sense when a given array shape is used for the first time.
//       Note that the dimensionality of the hypercube is one higher
//       than the dimensionality of the data arrays (since the hypercube
//       contains multiple rows). It means that the number of values in
//       tile shape can be one more than the number of axes in the data
//       array. The last tile shape value defaults to 1; the other
//       tile shape values have to be defined.
// </ul>
// </synopsis> 

// <motivation>
// TiledDataStMan proved to be very powerful, but also a bit cumbersome
// to use because a few special functions need to be called.
// TiledShapeStMan alleviates that problem.
// </motivation>

// <example>
// <srcblock>
//  // Define the table description and the columns in it.
//  TableDesc td ("", "1", TableDesc::Scratch);
//  td.addColumn (ArrayColumnDesc<float>  ("RA", 1));
//  td.addColumn (ArrayColumnDesc<float>  ("Dec", 1));
//  td.addColumn (ScalarColumnDesc<float> ("Velocity"));
//  td.addColumn (ArrayColumnDesc<float>  ("Image", 2));
//  // Define the 3-dim hypercolumn with its data and coordinate columns.
//  // Note that its dimensionality must be one higher than the dimensionality
//  // of the data cells.
//  td.defineHypercolumn ("TSMExample",
//			  3,
//			  stringToVector ("Image"),
//			  stringToVector ("RA,Dec,Velocity"));
//  // Now create a new table from the description.
//  SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
//  // Create a TiledShapeStMan storage manager for the hypercolumn
//  // and bind the columns to it.
//  // The (default) tile shape has to be specified for the storage manager.
//  TiledShapeStMan sm1 ("TSMExample", IPosition(3,16,32,32));
//  newtab.bindAll (sm1);
//  // Create the table.
//  Table table(newtab);
//  // Define the values for the coordinates of the hypercube.
//  Vector<float> raValues(512);
//  Vector<float> DecValues(512);
//  indgen (raValues);
//  indgen (decValues, float(100));
//  ArrayColumn<float> ra (table, "RA");
//  ArrayColumn<float> dec (table, "Dec");
//  ScalarColumn<float> velocity (table, "Velocity");
//  ArrayColumn<float> image (table, "Image");
//  Cube<float> imageValues(IPosition(2,512,512));
//  indgen (imageValues);
//  // Write some data into the data columns.
//  uInt i;
//  for (i=0; i<64; i++) {
//      table.addRow();
//      image.put (i, imageValues);
//      ra.put (i, raValues);
//      dec.put (i, decValues);
//      velocity.put (i, float(i));
//  }
// </srcblock>
// Note that in this example the same shape is used for each row,
// but it could have been different.
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TiledShapeStMan : public TiledStMan
{
public:
    // Create a TiledShapeStMan storage manager for the hypercolumn
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
    TiledShapeStMan (const String& hypercolumnName,
		     const IPosition& defaultTileShape,
		     uInt maximumCacheSize = 0);
    TiledShapeStMan (const String& hypercolumnName,
		     const Record& spec);
    // </group>

    ~TiledShapeStMan();

    // Clone this object.
    // It does not clone TSMColumn objects possibly used.
    virtual DataManager* clone() const;

    // Get the type name of the data manager (i.e. TiledShapeStMan).
    virtual String dataManagerType() const;

    // Return a record containing data manager specifications and info.
    virtual Record dataManagerSpec() const;

    // TiledShapeStMan can access a column if there are 2 hypercubes
    // and the first one is empty.
    // reask is set to True (because next time things might be different).
    virtual Bool canAccessColumn (Bool& reask) const;

    // Test if only one hypercube is used by this storage manager.
    // If not, throw an exception. Otherwise return the hypercube.
    virtual TSMCube* singleHypercube();

    // Set the shape and tile shape of the given hypercube.
    // It is used when the first row in a new hypercube is written.
    // If needed it adds a dimension to the shape, which reflects the
    // row dimension. The tile shape in that dimension is by default 1.
    virtual void setShape (uInt rownr, TSMCube* hypercube,
			   const IPosition& shape,
			   const IPosition& tileShape);

    // Make the object from the type name string.
    // This function gets registered in the DataManager "constructor" map.
    static DataManager* makeObject (const String& dataManagerType,
				    const Record& spec);

private:
    // Create a TiledShapeStMan.
    // This constructor is private, because it should only be used
    // by makeObject.
    TiledShapeStMan();

    // Forbid copy constructor.
    TiledShapeStMan (const TiledShapeStMan&);

    // Forbid assignment.
    TiledShapeStMan& operator= (const TiledShapeStMan&);

    // Get the default tile shape.
    virtual IPosition defaultTileShape() const;

    // Add rows to the storage manager.
    void addRow (uInt nrrow);

    // Find the hypercube for the given shape.
    // It returns -1 when not found.
    Int findHypercube (const IPosition& shape);

    // Add a hypercube.
    // The number of rows in the table must be large enough to
    // accommodate this hypercube.
    // The possible id values must be given in the record, while
    // coordinate values are optional. The field names in the record
    // should match the coordinate and id column names.
    // The last dimension in the cube shape can be zero, indicating that
    // the hypercube is extensible.
    void addHypercube (uInt rownr,
		       const IPosition& cubeShape,
		       const IPosition& tileShape);

    // Extend the hypercube with the given number of elements in
    // the last dimension.
    // The record should contain the id values (to get the correct
    // hypercube) and optionally coordinate values for the elements added.
    void extendHypercube (uInt rownr, uInt cubeNr);

    // Get the hypercube in which the given row is stored.
    virtual TSMCube* getHypercube (uInt rownr);

    // Get the hypercube in which the given row is stored.
    // It also returns the position of the row in that hypercube.
    virtual TSMCube* getHypercube (uInt rownr, IPosition& position);

    // Check if the hypercolumn definition fits this storage manager.
    virtual void setupCheck (const TableDesc& tableDesc,
			     const Vector<String>& dataNames) const;

    // Flush and optionally fsync the data.
    // It returns a True status if it had to flush (i.e. if data have changed).
    virtual Bool flush (AipsIO&, Bool fsync);

    // Let the storage manager create files as needed for a new table.
    // This allows a column with an indirect array to create its file.
    virtual void create (uInt nrrow);

    // Read the header info.
    virtual void readHeader (uInt nrrow, Bool firstTime);

    // Update the map of row numbers to cube number plus offset.
    void updateRowMap (uInt cubeNr, uInt pos, uInt rownr);

    // Extend the map of row numbers to cube number plus offset
    // will new empty entries.
    void extendRowMap (uInt nrow);


    //# Declare the data members.
    // The default tile shape.
    IPosition defaultTileShape_p;
    // The map of row number to cube and position in cube.
    Block<uInt> rowMap_p;
    Block<uInt> cubeMap_p;
    Block<uInt> posMap_p;
    // The nr of elements used in the map blocks.
    uInt nrUsedRowMap_p;
    // The last hypercube found.
    Int lastHC_p;
};




} //# NAMESPACE CASACORE - END

#endif
