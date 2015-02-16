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
//#
//# $Id$

#include <casacore/images/Regions/RegionManager.h>

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/HashMap.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogFilter.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/File.h>

#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Regions/RegionHandlerTable.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/images/Regions/WCComplement.h>
#include <casacore/images/Regions/WCConcatenation.h>
#include <casacore/images/Regions/WCDifference.h>
#include <casacore/images/Regions/WCEllipsoid.h>
#include <casacore/images/Regions/WCExtension.h>
#include <casacore/images/Regions/WCIntersection.h>
#include <casacore/images/Regions/WCLELMask.h>
#include <casacore/images/Regions/WCPolygon.h>
#include <casacore/images/Regions/WCUnion.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/namespace.h>


namespace casacore { //# name space casa begins


  RegionManager::RegionManager()
  {
      itsLog= new LogIO();
      itsCSys=0;
  }

  RegionManager::RegionManager(const CoordinateSystem& csys) : itsCSys(new CoordinateSystem(csys))
  {
    itsLog= new LogIO();
    //setcoordsys(csys);

  }
  RegionManager::~RegionManager()
  {
    if(itsLog !=0)
      delete itsLog;
    if(itsCSys != 0)
      delete itsCSys;

  }

  /*************************************************************
   **  An assortment of little helper and set/get methods     **
   *************************************************************/

  // Private method
  String RegionManager::absreltype(const Int absrelval){
    *itsLog << LogOrigin("RegionManager", "absreltype");

    if(absrelval == RegionType::Abs)
      return String("abs");
    else if(absrelval == RegionType::RelRef)
      return String("relref");
    else if(absrelval == RegionType::RelCen)
      return String("relcen");
    //    else if(absrelval == RegionType::RelDir)
    //return String("reldir");

    *itsLog << LogIO::WARN << "absrelvalue " << absrelval 
	    << " is not valid" << LogIO::POST;
    return String("Unknown");
  }

  void RegionManager::setcoordsys(const CoordinateSystem& csys){
    itsCSys= new CoordinateSystem(csys);
  }

  const CoordinateSystem& RegionManager::getcoordsys() const{
	  if (itsCSys == 0) {
	      throw(AipsError("CoordinateSystem not set in RegionManager tool"));
	  }
	  return *itsCSys;
  }

  Bool RegionManager::isPixelRegion(const ImageRegion& reg ){
      return  reg.isLCRegion();
  }
	
  Bool RegionManager::isWorldRegion(const ImageRegion& reg ){
      return  reg.isWCRegion();
  }
	


  /*************************************************************
   **  Make BOX region routines                               **
   *************************************************************/

  Record* RegionManager::box(const Vector<Double>& blc, 
			     const Vector<Double>& trc, 
			     const Vector<Double>& inc, 
                             const String& absrel, 
			     const Bool frac, const String& comment){
    *itsLog << LogOrigin("RegionManager", "box");
    /*   if(blc.nelements() != trc.nelements())
      throw(AipsError("blc and trc do not have the shape"));
    if(inc.nelements() != trc.nelements())
      throw(AipsError("inc and trc do not have the shape"));
    */
    RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
    LCSlicer muiSlicer(blc, trc, inc, frac, leType);
    muiSlicer.setComment( comment );

    Record *leRecord= new Record();    
    leRecord->assign(muiSlicer.toRecord(String("")));

    return leRecord;
  }
	

  Record* RegionManager::box(const Vector<Double>& blc, 
			     const Vector<Double>& trc, 
			     const Vector<Int>& shape, 
			     const String& comment){
  
    ThrowIf(blc.nelements() != trc.nelements(),
            "blc and trc do not have the same shape");
    IPosition latShape(shape);
    LCBox lcbox(blc, trc, latShape);

    // Note: LCBox is a LCRegionfixed -> LCRegionSingle -> LCRegion
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
    RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
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
 
    if(itsCSys==0) {
      ThrowCc("CoordinateSystem has not been set");
    }
    ImageRegion * leImReg=wbox(blc, trc, pixelaxes, *itsCSys, absrel);
    Record *leRecord= new Record();
    leRecord->assign(leImReg->toRecord(String("")));
    delete leImReg;
    leRecord->define("comment", comment);
    return leRecord;
  }

  void RegionManager::toQuantity(Quantity& out, const String& in){
    String leString=in;
    QuantumHolder qh;
    if(leString.contains("pix")){
      leString=leString.before("pix");
      Double value=atof(leString.chars());
      out=Quantity(value, "pix");
    }
    else{
      String error; 
      if(!qh.fromString(error, leString)){
	ostringstream oss;
	String err="Error " + error + " In converting quantity " + leString;
	throw( AipsError(err));
      }
      out=qh.asQuantity();
    }

  }
  Record* RegionManager::wbox(const Vector<String>& blc, 
			      const Vector<String>& trc, 
			      const Vector<Int>& pixelaxes,  
			      const String& absrel, const String& comment){
    ThrowIf(! itsCSys, "Coordinate system has not been set");
    Vector<Quantity> losBlc(blc.nelements());
    Vector<Quantity> losTrc(trc.nelements());
    QuantumHolder qh;
    //Stokes is not known in Quantity
    Int stInd = itsCSys->findCoordinate(Coordinate::STOKES);
    StokesCoordinate  stCoord(Vector<Int>(1, Stokes::I));
    Int wSt=-1;
    if(stInd>=0){
     wSt= (itsCSys->worldAxes(stInd))[0];
     stCoord=itsCSys->stokesCoordinate(stInd);
    }
    for (Int k=0; k < blc.shape()(0); ++k){
      if(k != wSt){
	toQuantity(losBlc[k], blc[k]);
	toQuantity(losTrc[k], trc[k]);
      }
      else{
	//Stokes is not known in Quantity...have to convert them to pix
	Int stpix=-1;
	if(blc[k].contains("pix"))
	  toQuantity(losBlc[k], blc[k]);
	else if(stCoord.toPixel(stpix, Stokes::type(blc[k])))
	  losBlc[k]=Quantity(stpix, "pix");
	stpix=-1;
	if(trc[k].contains("pix"))
	  toQuantity(losTrc[k], trc[k]);
	else if(stCoord.toPixel(stpix, Stokes::type(trc[k])))
	  losTrc[k]=Quantity(stpix, "pix");
      }
    }

    return wbox(losBlc, losTrc, pixelaxes, absrel, comment);

  }

  Record* RegionManager::wbox(const Vector<String>& blc, 
			      const Vector<String>& trc, 
			      const Vector<Int>& pixelaxes, const CoordinateSystem& csys,  
			      const String& absrel, const String& comment){
    setcoordsys(csys);    
    return wbox(blc, trc, pixelaxes, absrel, comment);

  }

  /*************************************************************
   **  Make POLYGON routines                                  **
   *************************************************************/

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


    RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
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


  ImageRegion* RegionManager::wellipse(
		  const Quantity& xc,
		  const Quantity& yc,
		  const Quantity& a,
		  const Quantity& b,
		  const Quantity& pa,
		  const uInt pixelAxis0,
		  const uInt pixelAxis1,
		  const CoordinateSystem& csys,
		  const String& absrel
  ) {
	  RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
	  WCEllipsoid wellipse(xc, yc, a, b, pa, pixelAxis0, pixelAxis1, csys, leType);
	  return new ImageRegion(wellipse);
  }

  ImageRegion* RegionManager::wellipse(
		  const Quantity& xc,
		  const Quantity& yc,
		  const Quantity& a,
		  const Quantity& b,
		  const Quantity& pa,
		  const uInt pixelAxis0,
		  const uInt pixelAxis1,
		  const String& absrel
  ) const {
	  *itsLog << LogOrigin("RegionManager", __FUNCTION__);
	  if (itsCSys == 0) {
		  throw(AipsError("CoordinateSystem not set in RegionManager tool"));
	  }
	  return wellipse(xc, yc, a, b, pa, pixelAxis0, pixelAxis1, *itsCSys, absrel);
  }

  ImageRegion* RegionManager::wsphere(
		  const Vector<Quantity>& center,
		  const Quantity& radius,
		  const Vector<Int>& pixelAxes,
		  const CoordinateSystem& csys,
		  const String& absrel

  ) {
	  RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
	  WCEllipsoid wsphere(center, radius, pixelAxes, csys, leType);
	  return new ImageRegion(wsphere);
  }

  ImageRegion* RegionManager::wsphere(
		  const Vector<Quantity>& center,
		  const Quantity& radius,
		  const Vector<Int>& pixelAxes,
		  const String& absrel
  ) const {
	  *itsLog << LogOrigin("RegionManager", __FUNCTION__);
	  if(itsCSys == 0){
		  throw(AipsError("CoordinateSystem not set in RegionManager tool"));
	  }
	  return wsphere(center, radius, pixelAxes, *itsCSys, absrel);
  }

  ImageRegion* RegionManager::wellipsoid(
		  const Vector<Quantity>& center,
		  const Vector<Quantity>& radii,
		  const Vector<Int>& pixelAxes,
		  const CoordinateSystem& csys,
		  const String& absrel
  ) {
	  RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
	  WCEllipsoid ellipsoid(center, radii, pixelAxes, csys, leType);
	  return new ImageRegion(ellipsoid);
  }

  ImageRegion* RegionManager::wellipsoid(
		  const Vector<Quantity>& center,
		  const Vector<Quantity>& radii,
		  const Vector<Int>& pixelAxes,
		  const String& absrel
  ) const {
	  *itsLog << LogOrigin("RegionManager", __FUNCTION__);
	  if(itsCSys == 0){
		  throw(AipsError("CoordinateSystem not set in RegionManager tool"));
	  }
	  return wellipsoid(center, radii, pixelAxes, *itsCSys, absrel);
  }


  ImageRegion* RegionManager::wshell(
		  const Vector<Quantity>& center,
		  const Vector<Quantity>& innerRadii,
		  const Vector<Quantity>& outerRadii,
		  const Vector<Int>& pixelAxes,
		  const CoordinateSystem& csys,
		  const String& absrel
  ) {
	  for (uInt i=0; i<innerRadii.size(); i++) {
		  if (
				  innerRadii[i].getValue()
				  > outerRadii[i].getValue(innerRadii[i].getUnit())
		  ) {
			  throw AipsError(
				  "RegionManager::" + String(__FUNCTION__)
				  + ": For radius " + String::toString(i)
				  + " inner radius " + String::toString(innerRadii[i])
				  + " is greater than outer radius "
				  + String::toString(outerRadii[i])
			  );
		  }
	  }
	  RegionType::AbsRelType leType=RegionType::absRelTypeFromString(absrel);
	  WCEllipsoid inner(center, innerRadii, pixelAxes, csys, leType);
	  WCEllipsoid outer(center, outerRadii, pixelAxes, csys, leType);
	  WCDifference shell(outer, inner);
	  return new ImageRegion(shell);
  }

  ImageRegion* RegionManager::wshell(
		  const Vector<Quantity>& center,
		  const Vector<Quantity>& innerRadii,
		  const Vector<Quantity>& outerRadii,
		  const Vector<Int>& pixelAxes,
		  const String& absrel
  ) const {
	  *itsLog << LogOrigin("RegionManager", __FUNCTION__);
	  if(itsCSys == 0){
		  throw(AipsError("CoordinateSystem not set in RegionManager tool"));
	  }
	  return wshell(center, innerRadii, outerRadii, pixelAxes, *itsCSys, absrel);
  }

  ImageRegion* RegionManager::wmask(const String& command) {
	  WCLELMask wmask(command);
	  return new ImageRegion(wmask);
  }


  /*************************************************************
   **  UNION routines                                  **
   *************************************************************/


  ImageRegion*  RegionManager::doUnion(const WCRegion& reg1, 
				       const WCRegion& reg2) {
      *itsLog << LogOrigin("RegionManager", String(__FUNCTION__) + "_1");
      ImageRegion imageReg1(reg1);
      ImageRegion imageReg2(reg2);
      return doUnion(imageReg1, imageReg2);
  }

  ImageRegion*  RegionManager::doUnion(const PtrBlock<const WCRegion*>& regions) {
      *itsLog << LogOrigin("RegionManager", String(__FUNCTION__) + "_2");
      WCUnion leUnion(False, regions);
      ImageRegion* leReturn= new ImageRegion(leUnion);
      return leReturn;
  }

  ImageRegion*  RegionManager::doUnion(const ImageRegion& reg1, 
				       const ImageRegion& reg2) const {

      *itsLog << LogOrigin("RegionManager", String(__FUNCTION__) + "_3");
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

  /*************************************************************
   **  INTERSECTION routines                                  **
   *************************************************************/

  ImageRegion*  RegionManager::doIntersection(const WCRegion& reg1, 
					const WCRegion& reg2){

      ImageRegion imageReg1(reg1);
      ImageRegion imageReg2(reg2);
      return doIntersection(imageReg1, imageReg2);
  }

  ImageRegion*  RegionManager::doIntersection(
      const PtrBlock<const WCRegion*>& regions)
  {
      WCIntersection leIntersect(False, regions);
      ImageRegion* leReturn= new ImageRegion(leIntersect);
      return leReturn;
  }

  ImageRegion*  RegionManager::doIntersection(const ImageRegion& reg1, 
				       const ImageRegion& reg2){
      *itsLog << LogOrigin("RegionManager", "doIntersection");
      *itsLog << LogIO::DEBUGGING
	      << "reg1 type " << reg1.isWCRegion() << " " << reg1.isLCRegion() 
	      << " "<< reg1.isLCSlicer() 
	      << "\nreg2 type " << reg2.isWCRegion() << " " 
	      << reg2.isLCRegion() << " "<< reg2.isLCSlicer()
	      << LogIO::POST;

      WCIntersection leIntersection(reg1, reg2);
      ImageRegion* leReturn= new ImageRegion(leIntersection);
      return leReturn;
  }

  /*************************************************************
   **  COMPLEMENT routines                                    **
   *************************************************************/

  ImageRegion*  RegionManager::doComplement(const WCRegion& reg){
      *itsLog << LogOrigin("RegionManager", "doComplement");
      ImageRegion imageReg1(reg);
      return doComplement(imageReg1);
  }

  ImageRegion*  RegionManager::doComplement(const PtrBlock<const WCRegion*>& regions){
      *itsLog << LogOrigin("RegionManager", "doComplement");
      WCComplement leComplement(False, regions);
      ImageRegion* leReturn= new ImageRegion(leComplement);
      return leReturn;
  }

  ImageRegion*  RegionManager::doComplement(const ImageRegion& reg1){
      *itsLog << LogOrigin("RegionManager", "doComplement");
      *itsLog << LogIO::DEBUGGING
	      << "reg1 type " << reg1.isWCRegion() << " " << reg1.isLCRegion() 
	      << " "<< reg1.isLCSlicer() 
	      << LogIO::POST;

      WCComplement leComplement(reg1);
      ImageRegion* leReturn = new ImageRegion(leComplement);

      return leReturn;
  }


  /*************************************************************
   **  DIFFERENCE routines                                    **
   **  Note, that the we could add support for doing the      **
   **  difference of multiple regions since the support       **
   **  exists underneath.                                     **
   *************************************************************/

  ImageRegion*  RegionManager::doDifference(const WCRegion& reg1, 
					const WCRegion& reg2){

      ImageRegion imageReg1(reg1);
      ImageRegion imageReg2(reg2);
      return doDifference(imageReg1, imageReg2);
  }

  ImageRegion*  RegionManager::doDifference(
      const PtrBlock<const WCRegion*>& regions)
  {
      WCDifference leDiff(False, regions);
      ImageRegion* leReturn = new ImageRegion(leDiff);
      return leReturn;
  }

  ImageRegion*  RegionManager::doDifference(const ImageRegion& reg1, 
				       const ImageRegion& reg2){
      *itsLog << LogOrigin("RegionManager", "doDifference");
      *itsLog << LogIO::DEBUGGING
	      << "reg1 type " << reg1.isWCRegion() << " " << reg1.isLCRegion() 
	      << " "<< reg1.isLCSlicer() 
	      << "\nreg2 type " << reg2.isWCRegion() << " " 
	      << reg2.isLCRegion() << " "<< reg2.isLCSlicer()
	      << LogIO::POST;

      WCDifference leDiff(reg1, reg2);
      ImageRegion* leReturn= new ImageRegion(leDiff);
      return leReturn;
  }

    
  /*************************************************************
   **  CONCAT a box to a region(s) routines                   **
   *************************************************************/
	
 ImageRegion*  RegionManager::doConcatenation(
     const WCRegion& region, 
     const WCBox& box )
 {
     PtrBlock<const ImageRegion *> imageRegions(1);
     imageRegions[0]= new ImageRegion(region);
     TableRecord recordBox = box.toRecord("");
     return doConcatenation(imageRegions, recordBox);
  }

  ImageRegion*  doConcatenation(
      const PtrBlock<const WCRegion*>& regions, 
      const WCBox& box)
  {
      WCConcatenation leConcat(False, regions, box);
      ImageRegion* leReturn= new ImageRegion(leConcat);
      return leReturn;
  }

  ImageRegion*  RegionManager::doConcatenation(
      const PtrBlock<const ImageRegion*>& regions,
      const TableRecord& box )
  {
      *itsLog << LogOrigin("RegionManager", "doConcatenation");

      for ( uInt i=0; regions.nelements(); i++ )
	  *itsLog << LogIO::DEBUGGING
		  << "\nregion " << i 
		  << "'s type (WCRegion/LCRegion/LCSLicer): " 
		  << regions[i]->isWCRegion() << "/" 
		  << regions[i]->isLCRegion() << "/"
		  << regions[i]->isLCSlicer()  << LogIO::POST;

      const WCBox *lebox = WCBox::fromRecord( box, "" );
      WCConcatenation leConcatenation(regions, *lebox );
      
      ImageRegion* leReturn= new ImageRegion(leConcatenation);
      return leReturn;
  }


  ImageRegion*  RegionManager::doConcatenation(
      const Record& regions,
      const TableRecord& box )
  {
      *itsLog << LogOrigin("RegionManager", "doConcatenation");

      // Once we convert the region Record to PtrBlock of 
      // ImageRegions then we just call one of the other
      // doConcatenation routines.

      if ( regions.nfields() < 1 )
	  throw(AipsError(String("No regions have been supplied to concatenation" ) ) );

      PtrBlock<const ImageRegion*> imageRegions(regions.nfields());
      ImageRegion* reg=0;
      TableRecord tblRec;
      for( uInt i=0; i < (regions.nfields()); i++ )
      {
	  tblRec.assign(regions.asRecord(casacore::RecordFieldId(0)));
	  reg=ImageRegion::fromRecord(tblRec, "");
	  imageRegions[i]=reg;
      }       

      // Convert the box table record to a WCBox
      const WCBox *lebox = WCBox::fromRecord( box, "" );
      WCConcatenation leConcatenation(imageRegions, *lebox );
      
      ImageRegion* leReturn= new ImageRegion(leConcatenation);
      return leReturn;
  }


  /*************************************************************
   **  EXTEND region routines                                 **
   *************************************************************/


  /*************************************************************
   ** Regions from file/table  routines                       **
   *************************************************************/
  
  Record* RegionManager::readImageFile( String filepath, String regionname )
  {
      // open the file
      AipsIO ios( filepath, ByteIO::Old );

      // Read the file contents and convert it too an ImageRegion.
      // The commented out lines really should be used, but when
      // uncommented we get exceptions thrown.  For some reason
      // AipsIO finds a type of TableRecord, then a type of RecordDesc
      // this causes Exceptions to be thrown because the type we give
      // and the type found don't match.  This could be due to the way
      // the file is saved or some other quirk in AipsIO.
      TableRecord leTblRec;
      ImageRegion *leImReg;
      //ios.getstart( "TableRecord" );
      ios >> leTblRec;
      //ios.getend();
      
      if ( regionname.length() > 0 )
	  leImReg = ImageRegion::fromRecord( leTblRec, regionname );
      else
	  // TODO strip path part off and use just the tail of
	  // the filename.
	  leImReg = ImageRegion::fromRecord( leTblRec, filepath );
      //delete leTblRec;
      

      // Convert the ImageRegion to a Record
      Record * leRecord = new Record();
      leRecord->assign( leImReg->toRecord(String("")) );
      delete leImReg;
      String comment = "Created from file: " + filepath;
      leRecord->define( "comment", comment );

      return leRecord;
  }

  Bool RegionManager::writeImageFile(const String& file, const String& regionname, const Record& regionRecord){

    TableRecord regionTblRecord(regionRecord);
    ImageRegion *imageReg=ImageRegion::fromRecord(regionTblRecord, "");
     try{
         AipsIO oos(file, ByteIO::NewNoReplace);
         oos << imageReg->toRecord(regionname);  
      }
      catch(...) { 
	throw(AipsError(String("Could not create the region file.\n Please check pathname and directory \n")+
			String(" permissions and be sure the file \n does not already exist.")));
      }

     delete imageReg;
     return True;

  }

	
  String RegionManager::imageRegionToTable(const String& tabName, const ImageRegion& imreg, const String& regName, Bool asmask){
    tab_p=Table(tabName, Table::Update);
    RegionHandlerTable regtab (getTable, this);
    String newName=regName;
    Bool retval=False;
    if(regtab.hasRegion(newName) || newName=="")
      newName=regtab.makeUniqueRegionName(regName, 0);
    if(asmask){
      try{
	PagedImage<Float> myimage(tabName);
	SubImage<Float> subim(myimage, imreg, True);
	ImageRegion outreg=myimage.makeMask(newName, False, False);
	LCRegion& mask=outreg.asMask();
	LatticeRegion latReg=imreg.toLatticeRegion(myimage.coordinates(), myimage.shape());
	SubLattice<Bool> subMask(mask, latReg, True);
	subMask.set(True);
	myimage.defineRegion (newName, mask, RegionHandler::Masks);
	retval=myimage.hasRegion(newName);
      }
      catch(AipsError x){
	throw(AipsError("Could not write mask in image "+tabName+" because "+x.getMesg()));
      }
      catch(...){
	throw(AipsError("Could not write mask in image "+tabName));
      }

    }
    else{
      retval=regtab.defineRegion (newName, imreg, RegionHandler::Regions);
    }
    tab_p.relinquishAutoLocks();
    tab_p=Table();

    if(retval)
      return newName;
    else
      return String("");

 }
  String RegionManager::recordToTable(const String& tabName, const RecordInterface& rec, const String& regName, Bool asmask){

    if(!Table::isWritable(tabName)){
    
      *itsLog << LogIO::WARN  << tabName << " is not valid or writeable table" 
	    << LogIO::POST;
      return String("");

    }
    TableRecord lerec(rec);
    ImageRegion* imreg=ImageRegion::fromRecord(lerec, "") ;
    String newName=imageRegionToTable(tabName, *imreg, regName, asmask);
    
    delete imreg;
    return newName;

  }

  Record* RegionManager::tableToRecord(const String& tabName,   
				       const String& regname){

    if(!Table::isReadable(tabName)){

      *itsLog << LogIO::WARN  << tabName << " is not a valid or readable table" 
	    << LogIO::POST;
      return 0;

    }
    tab_p=Table(tabName, Table::Old);
    RegionHandlerTable regtab (getTable, this);
    if(!regtab.hasRegion(regname)){
      *itsLog << LogIO::WARN  << tabName << " does not have region "
	      << regname << LogIO::POST;
      tab_p=Table();
      return 0;

    }
    ImageRegion* imreg=regtab.getRegion(regname, RegionHandler::Any, False);
    Record * leRecord = new Record();
    leRecord->assign( imreg->toRecord(String("")) );
    delete imreg;
    tab_p.relinquishAutoLocks();
    tab_p=Table();
    return leRecord;

  }

  Vector<String> RegionManager::namesInTable(const String& tabName){
    Vector<String> retval;
    if(!Table::isReadable(tabName)){
      *itsLog << LogIO::WARN  << tabName << " is not a valid or readable table" 
	      << LogIO::POST;
      return retval;

    }
    tab_p=Table(tabName, Table::Old);
    RegionHandlerTable regtab (getTable, this);
    retval=regtab.regionNames();
    tab_p.relinquishAutoLocks();
    tab_p=Table();
    return retval;
  }

  Bool RegionManager::removeRegionInTable(const String& tabName, const String& regName){
    Bool retval;
    if(!Table::isWritable(tabName)){
      *itsLog << LogIO::WARN  << tabName << " is not a valid or writable table" 
	      << LogIO::POST;
      return False;

    }
    if(regName==""){
      *itsLog << LogIO::WARN  << "No region name given to remove...nothing done" 
	      << LogIO::POST;
      return False;
    }
    tab_p=Table(tabName, Table::Update);
    RegionHandlerTable regtab (getTable, this);
    if(!regtab.hasRegion(regName)){
      *itsLog << LogIO::WARN  << tabName << " does not have region "
	      << regName << LogIO::POST;
      tab_p.relinquishAutoLocks(); 
      tab_p=Table();
      return False;

    }
    retval=regtab.removeRegion(regName, RegionHandler::Any, False);
    tab_p.relinquishAutoLocks();
    tab_p=Table();
    return retval;

  }


  Table& RegionManager::getTable(void* ptr, Bool)
  {
    RegionManager* rg = static_cast<RegionManager*>(ptr);
    return rg->tab_p;
  }

} // end of  casa namespace
