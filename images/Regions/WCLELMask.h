//# WCLELMask.h: Class to define a mask as a LEL expression
//# Copyright (C) 2000,2003
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
//# $Id$



#ifndef IMAGES_WCLELMASK_H
#define IMAGES_WCLELMASK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/WCRegion.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LCRegion;
class TableRecord;
class IPosition;
template<class T> class ImageExpr;
template<class T> class LatticeExpr;
class LatticeExprNode;


// <summary>
// Class to define a mask as a LEL expression
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=WCRegion>WCRegion</linkto>
//   <li> <linkto class=ImageExpr>ImageExpr</linkto>
// </prerequisite>

// <synopsis> 
// The WCLELMask class is a specialization of class
// <linkto class=WCRegion>WCRegion</linkto>.
// <br>
// It can be used to define an on-the-fly mask for an image
// using a boolean <linkto class=LatticeExpr>LatticeExpr</linkto>.
// The contents of the mask are calculated on the fly from the expression.
// Thus the mask may change if the data in the image(s) used in the
// expression change.
// <note role=caution>
// This mask is only persistent if constructed from an expression string.
// When constructed from an <linkto class=ImageExpr>ImageExpr</linkto>
// the mask is not persistent.
// </note>
// </synopsis> 

// <example>
// </example>

// <motivation>
// Users must be able to specify a mask based on an expression.
// </motivation>

//# <todo asof="1998/05/20">
//#   <li> 
//# </todo>

class WCLELMask : public WCRegion
{
public:
  WCLELMask();

  // Construct from the given expression command.
  // The command will be parsed and converted to an ImageExpr.
  // <group>
  explicit WCLELMask (const String& command);
  explicit WCLELMask (const char* command);
  // </group>

  // Construct from the given image expression.
  explicit WCLELMask (const ImageExpr<Bool>& expr);

  // Construct from the given lattice expression.
  explicit WCLELMask (const LatticeExpr<Bool>& expr);

  // Construct from the given lattice expression.
  // This constructor makes it possible to have an expression with an
  // unknown shape (e.g. using LEL function INDEXIN).
  // If the shape is known, the LatticeExprNode will be converted to
  // a LatticeExpr<Bool>.
  explicit WCLELMask (const LatticeExprNode& expr);

  // Copy constructor (copy semantics).
  WCLELMask (const WCLELMask& other);

  // Destructor
  virtual ~WCLELMask();

  // Assignment (copy semantics) 
  WCLELMask& operator= (const WCLELMask& other);

  // Comparison
  virtual Bool operator== (const WCRegion& other) const;

  // Clone a WCLELMask object.
  virtual WCRegion* cloneRegion() const;

  // Get the dimensionality (i.e. the number of axes).
  virtual uInt ndim() const;

  // WCLELMask cannot extend a region.
  virtual Bool canExtend() const;

  // Convert to an LCRegion using the given new coordinate system and shape.
  // If the region has coordinates, the WCRegion implementation will
  // be called. Otherwise the LatticeExpr is returned after checking
  // that the shape matches.
  virtual LCRegion* toLCRegion (const CoordinateSystem& cSys,
				const IPosition& latticeShape) const;

  // Convert to an LCRegion using the supplied <src>CoordinateSystem</src> 
  // and shape.  
  // It checks that coordinates match and that axes are not swapped.
  virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
				  const IPosition& latticeShape,
				  const IPosition& pixelAxesMap,
				  const IPosition& outOrder) const;

  // Convert the WCLELMask object to a record.
  // The record can be used to make the object persistent.
  // The <src>tableName</src> argument can be used by derived
  // classes (e.g. LCPagedMask) to put very large objects.
  virtual TableRecord toRecord (const String& tableName) const;

  // Convert to a WCLELMask from a record.
  static WCLELMask* fromRecord (const TableRecord& rec,
				const String& tableName);

  // Returns WCLELMask
  static String className();

  // Return region type.  Returns the class name 
  virtual String type() const;

  const ImageExpr<Bool>* getImageExpr() const {return itsImageExpr;}

private:
  // Process the command.
  void processCommand();

  // Initialize as a LatticeExprNode if expression's shape is unknown.
  // Otherwise as a LatticeExpr<Bool> if coordinates are unknown.
  // Otherwise as an ImageExpr<Bool>.
  void init (const LatticeExprNode& expr);


  String             itsCommand;
  ImageExpr<Bool>*   itsImageExpr;
  LatticeExpr<Bool>* itsLattExpr;
  LatticeExprNode*   itsLattNode;
};



} //# NAMESPACE CASACORE - END

#endif
