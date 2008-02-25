//# RegionManager.cc: framework independent class that provides 
//# functionality to tool of same name
//# Copyright (C) 2007
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
#include <casa/BasicSL/String.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/Block.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogFilter.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/Quanta/Quantum.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <lattices/Lattices/LCBox.h>
#include <lattices/Lattices/LCSlicer.h>
#include <lattices/Lattices/RegionType.h>
#include <images/Images/ImageRegion.h>
#include <images/Images/RegionManager.h>
#include <images/Images/WCBox.h>
#include <images/Images/WCPolygon.h>
#include <images/Images/WCRegion.h>
#include <images/Images/WCUnion.h>
#include <tables/Tables/TableRecord.h>
#include <casa/namespace.h>


namespace casa { //# name space casa begins

  RegionManager::RegionManager()
  {
    itsLog= new LogIO();
    itsCSys=0;

  }

  RegionManager::RegionManager(CoordinateSystem& csys):itsCSys(0)
  {
    itsLog= new LogIO();
    setcoordsys(csys);

  }
  RegionManager::~RegionManager()
  {
    if(itsLog !=0)
      delete itsLog;

  }
  String RegionManager::absreltype(const Int absrelval){
    *itsLog << LogOrigin("RegionManager", "absreltype");
    if(absrelval == RegionType::Abs)
      return String("abs");
    else if(absrelval == RegionType::RelRef)
      return String("relref");
    else if(absrelval == RegionType::RelCen)
      return String("relcen");

    *itsLog << LogIO::WARN << "absrelvalue " << absrelval << " is not valid" 
	    << LogIO::POST;
    return String("Unknown");


  }
  Record* RegionManager::box(const Vector<Double>& blc, 
			     const Vector<Double>& trc, 
			     const Vector<Double>& inc, const String& absrel, 
			     Bool frac, const String& comment){
    *itsLog << LogOrigin("RegionManager", "box");
    /*   if(blc.nelements() != trc.nelements())
      throw(AipsError("blc and trc do not have the shape"));
    if(inc.nelements() != trc.nelements())
      throw(AipsError("inc and trc do not have the shape"));
    */
    RegionType::AbsRelType leType=regionTypeFromString(absrel);
    LCSlicer muiSlicer(blc, trc, inc, frac, leType);
    Record *leRecord= new Record();
    leRecord->assign(muiSlicer.toRecord(String("")));
    leRecord->define("comment", comment);

    return leRecord;

    
  }
  

  Record* RegionManager::box(const Vector<Double>& blc, 
			     const Vector<Double>& trc, 
			     const Vector<Int>& shape, const String& comment){
  

    *itsLog << LogOrigin("RegionManager", "box");
    if(blc.nelements() != trc.nelements())
      throw(AipsError("blc and trc do not have the shape"));
    IPosition latShape(shape);
    LCBox lcbox(blc, trc, latShape);
    Record *leRecord= new Record();
    leRecord->assign(lcbox.toRecord(String("")));
    leRecord->define("comment", comment);

    return leRecord;

  }

  ImageRegion* RegionManager::wbox(const Vector<Quantity>& blc, 
				   const Vector<Quantity>& trc, 
				   const Vector<Int>& pixelaxes, 
				   const CoordinateSystem& csys, 
				   const String& absrel){

    *itsLog << LogOrigin("RegionManager", "wbox");
    RegionType::AbsRelType leType=regionTypeFromString(absrel);
    Vector<Int> absRel(blc.nelements(), leType);
    WCBox worldbox;

 
    if(pixelaxes.nelements() > 0 && pixelaxes[0] <0){
      worldbox=WCBox(blc, trc, csys, absRel);
    }
    else{
      worldbox=WCBox(blc,trc,IPosition(pixelaxes),csys,absRel);
    }
    ImageRegion *leRegion = new ImageRegion(worldbox);
    return leRegion;

  } 

  Record* RegionManager::wbox(const Vector<Quantity>& blc, 
			      const Vector<Quantity>& trc, 
			      const Vector<Int>& pixelaxes, 
			      const CoordinateSystem& csys, 
			      const String& absrel, const String& comment){
    setcoordsys(csys);    
    return wbox(blc, trc, pixelaxes, absrel, comment);

  }

  Record* RegionManager::wbox(const Vector<Quantity>& blc, 
			      const Vector<Quantity>& trc, 
			      const Vector<Int>& pixelaxes,  
			      const String& absrel, const String& comment){
 

    if(itsCSys==0)
      throw(AipsError(String("CoordinateSystem has not been set"))); 
    ImageRegion * leImReg=wbox(blc, trc, pixelaxes, *itsCSys, absrel);
    Record *leRecord= new Record();
    leRecord->assign(leImReg->toRecord(String("")));
    delete leImReg;
    leRecord->define("comment", comment);
    return leRecord;



  }


  ImageRegion* RegionManager::wpolygon(const Vector<Quantity>& x, 
				       const Vector<Quantity>& y, 
				       const Vector<Int>& pixelaxes, 
				       const CoordinateSystem& csys, 
				       const String& absrel){
    
    *itsLog << LogOrigin("RegionManager", "wpolygon");
    Vector<Int> pixax=pixelaxes;
    if(pixax.nelements() > 0 && pixax[0] <0){ 

      pixax.resize(2);
      pixax(0)=0; pixax(1)=1; 
      
    }

    if(y.nelements() != x.nelements())
      throw(AipsError("Y values of vertices not same length as the X values"));

    //Now lets convert everything to one unit in this instance the the pix unit
    uInt nvertices=y.nelements();
    Vector<Double> leX(nvertices);
    Vector<Double> leY(nvertices);
    String xUnit=csys.worldAxisUnits()[pixax[0]];
    String yUnit=csys.worldAxisUnits()[pixax[1]];
    //  Vector<Int> worldaxes(2);
    // worldaxes(0)=csys.pixelAxisToWorldAxis(pixax[0]);
    // worldaxes(1)=csys.pixelAxisToWorldAxis(pixax[1]);
    const DirectionCoordinate& dirCoor=csys.directionCoordinate(csys.findCoordinate(Coordinate::DIRECTION));
    Vector<Double> world = csys.referenceValue();
    Vector<Double> pixel(world.nelements());
    for (uInt k=0; k < nvertices; ++k){
      if(x[k].getUnit().contains("pix") && y[k].getUnit().contains("pix") ){
	
	Vector<Double> lepix(2);
	lepix[0]=x[k].getValue();
	lepix[1]=y[k].getValue();
	Vector<Double> lemonde(2);
	dirCoor.toWorld(lemonde, lepix);
	leX[k]=lemonde[0]; leY[k]=lemonde[1];
	
      }
      else if((x[k].getUnit().contains("pix") && 
	       !y[k].getUnit().contains("pix")) || 
	      (!x[k].getUnit().contains("pix") && 
	       y[k].getUnit().contains("pix"))){
	throw(AipsError("Cannot  handle cross units pix and non-pix together"));
      }
      else{
        leX[k]=x[k].getValue(xUnit);
	leY[k]=y[k].getValue(yUnit);
	/*	csys.toPixel(pixel, world);
	leX[k]=pixel[pixax[0]];
	leY[k]=pixel[pixax[1]];
	*/
      }
      
    }
    Quantum<Vector<Double> > elX(leX, xUnit);
    Quantum<Vector<Double> > elY(leY, yUnit);


    RegionType::AbsRelType leType=regionTypeFromString(absrel);
    WCPolygon worldpoly(elX,elY,IPosition(pixax),csys, leType);
    ImageRegion *leRegion = new ImageRegion(worldpoly);
    return leRegion;

  } 
  ImageRegion* RegionManager::wpolygon(const Vector<Quantity>& x, 
				       const Vector<Quantity>& y, 
				       const Vector<Int>& pixelaxes,  
				       const String& absrel){
    *itsLog << LogOrigin("RegionManager", "wpolygon");
    if(itsCSys !=0){
      return wpolygon(x, y, pixelaxes, *itsCSys, absrel);
    }
    else{
      throw(AipsError("CoordinateSystem not set in RegionManager tool")); 
    
    }
    return 0;
  }
  ImageRegion*  RegionManager::doUnion(const WCRegion& reg1, 
				       const WCRegion& reg2){


    ImageRegion imageReg1(reg1);
    ImageRegion imageReg2(reg2);
    return doUnion(imageReg1, imageReg2);


  }

  ImageRegion*  RegionManager::doUnion(const PtrBlock<const WCRegion*>& regions){


    WCUnion leUnion(False, regions);
    ImageRegion* leReturn= new ImageRegion(leUnion);
    return leReturn;

  }

  ImageRegion*  RegionManager::doUnion(const ImageRegion& reg1, 
				       const ImageRegion& reg2){

      *itsLog << LogOrigin("RegionManager", "doUnion");
      *itsLog << LogIO::DEBUGGING
	      << "reg1 type " << reg1.isWCRegion() << " " << reg1.isLCRegion() 
	      << " "<< reg1.isLCSlicer() 
	      << "\nreg2 type " << reg2.isWCRegion() << " " 
	      << reg2.isLCRegion() << " "<< reg2.isLCSlicer()
	      << LogIO::POST;

    WCUnion leUnion(reg1, reg2);
    ImageRegion* leReturn= new ImageRegion(leUnion);
    return leReturn;


  }

  


  void RegionManager::setcoordsys(const CoordinateSystem& csys){

    itsCSys= new CoordinateSystem(csys);

  }
  RegionType::AbsRelType RegionManager::regionTypeFromString(const String& absrel){

    String str(absrel);
    str.upcase();
    if(str.contains("ABS"))
      return RegionType::Abs;
    else if(str.contains("RELREF"))
      return RegionType::RelRef;
    else if(str.contains("RELCEN"))
      return RegionType::RelCen;
    else
      throw(AipsError(String("Undefined region type")+absrel));
    


  }

  

 


} // end of  casa namespace
