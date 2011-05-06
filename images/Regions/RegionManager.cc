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

#include <images/Regions/RegionManager.h>

#include <casa/BasicSL/String.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/Block.h>
#include <casa/Containers/HashMap.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogFilter.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/IO/AipsIO.h>
#include <casa/OS/File.h>

#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/StokesCoordinate.h>
#include <lattices/Lattices/LCBox.h>
#include <lattices/Lattices/LCSlicer.h>
#include <lattices/Lattices/RegionType.h>
#include <images/Regions/ImageRegion.h>
#include <images/Regions/RegionHandlerTable.h>
#include <images/Regions/WCBox.h>
#include <images/Regions/WCComplement.h>
#include <images/Regions/WCConcatenation.h>
#include <images/Regions/WCDifference.h>
#include <images/Regions/WCExtension.h>
#include <images/Regions/WCIntersection.h>
#include <images/Regions/WCPolygon.h>
#include <images/Regions/WCRegion.h>
#include <images/Regions/WCUnion.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/Table.h>
#include <casa/namespace.h>


namespace casa { //# name space casa begins

	const String RegionManager::ALL = "ALL";


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
    return *itsCSys;
  }
  Bool RegionManager::isPixelRegion(const ImageRegion& reg ){
      return  reg.isLCRegion();
  }
	
  Bool RegionManager::isWorldRegion(const ImageRegion& reg ){
      return  reg.isWCRegion();
  }
	

  // Private Method.
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
    RegionType::AbsRelType leType=regionTypeFromString(absrel);
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
  

    *itsLog << LogOrigin("RegionManager", "box");
    if(blc.nelements() != trc.nelements())
      throw(AipsError("blc and trc do not have the shape"));
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
	  tblRec.assign(regions.asRecord(casa::RecordFieldId(0)));
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

	
  String RegionManager::imageRegionToTable(const String& tabName, const ImageRegion& imreg, const String& regName){
    tab_p=Table(tabName, Table::Update);
    RegionHandlerTable regtab (getTable, this);
    String newName=regName;
    Bool retval=False;
    if(regtab.hasRegion(newName) || newName=="")
      newName=regtab.makeUniqueRegionName(regName, 0);
    retval=regtab.defineRegion (newName, imreg, RegionHandler::Regions);
    tab_p.relinquishAutoLocks();
    tab_p=Table();

    if(retval)
      return newName;
    else
      return String("");

 }
  String RegionManager::recordToTable(const String& tabName, const RecordInterface& rec, const String& regName){

    if(!Table::isWritable(tabName)){
    
      *itsLog << LogIO::WARN  << tabName << " is not valid or writeable table" 
	    << LogIO::POST;
      return String("");

    }
    TableRecord lerec(rec);
    ImageRegion* imreg=ImageRegion::fromRecord(lerec, "") ;
    String newName=imageRegionToTable(tabName, *imreg, regName);
    
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

  Vector<uInt> RegionManager::_setSpectralRanges(
	String specification, uInt& nSelectedChannels, const uInt nChannels
  ) const {
      LogOrigin origin("ImageInputProcessor", __FUNCTION__);
      *itsLog << origin;

	  Vector<uInt> ranges(0);
	  if (! itsCSys->hasSpectralAxis()) {
          nSelectedChannels = 0;
		  return ranges;
	  }

	  specification.trim();
	  specification.upcase();

	  uInt nchan = nChannels;
	  if (specification.empty() || specification == ALL) {
		  ranges.resize(2);
		  ranges[0] = 0;
		  ranges[1] = nchan - 1;
          nSelectedChannels = nchan;
		  return ranges;
	  }

	  // First split on commas
	  Vector<String> parts = stringToVector(specification, Regex("[,;]"));
	  ranges.resize(2*parts.size());
	  Regex regexuInt("^[0-9]+$");
	  Regex regexRange("^[0-9]+[ \n\t\r\v\f]*~[ \n\t\r\v\f]*[0-9]+$");
	  Regex regexLT("^<.*$");
	  Regex regexLTEq("^<=.*$");
	  Regex regexGT("^>.*$");
	  Regex regexGTEq("^>=.*$");

	  for (uInt i=0; i<parts.size(); i++) {
		  parts[i].trim();
		  uInt min = 0;
		  uInt max = 0;
		  if (parts[i].matches(regexuInt)) {
			  // just one channel
			  min = String::toInt(parts[i]);
			  max = min;
		  }
		  else if(parts[i].matches(regexRange)) {
              // a range of channels
			  Vector<String> values = stringToVector(parts[i], '~');
			  if (values.size() != 2) {
				  *itsLog << "Incorrect specification for channel range "
						  << parts[i] << LogIO::EXCEPTION;
			  }
			  values[0].trim();
			  values[1].trim();
			  for(uInt j=0; j < 2; j++) {
				  if (! values[j].matches(regexuInt)) {
					  *itsLog << "For channel specification " << values[j]
					                                                    << " is not a non-negative integer in "
					                                                    << parts[i] << LogIO::EXCEPTION;
				  }
			  }
			  min = String::toInt(values[0]);
			  max = String::toInt(values[1]);
		  }
		  else if (parts[i].matches(regexLT)) {
			  String maxs = parts[i].matches(regexLTEq) ? parts[i].substr(2) : parts[i].substr(1);
			  maxs.trim();
			  if (! maxs.matches(regexuInt)) {
				  *itsLog << "In channel specification, " << maxs
						  << " is not a non-negative integer in " << parts[i]
						                                                   << LogIO::EXCEPTION;
			  }
			  min = 0;
			  max = String::toInt(maxs);
			  if (! parts[i].matches(regexLTEq)) {
				  if (max == 0) {
					  *itsLog << "In channel specification, max channel cannot "
							  << "be less than zero in " + parts[i];
				  }
				  else {
					  max--;
				  }
			  }
		  }
		  else if (parts[i].matches(regexGT)) {
			  String mins = parts[i].matches(regexGTEq)
         					? parts[i].substr(2)
         							: parts[i].substr(1);
			  mins.trim();
			  if (! mins.matches(regexuInt)) {
				  *itsLog << " In channel specification, " << mins
						  << " is not an integer in " << parts[i]
						                                       << LogIO::EXCEPTION;
			  }
			  max = nchan - 1;
			  min = String::toInt(mins);
			  if(! parts[i].matches(regexGTEq)) {
				  min++;
			  }
			  if (min > nchan - 1) {
				  *itsLog << "Min channel cannot be greater than the (zero-based) number of channels ("
						  << nchan - 1 << ") in the image" << LogIO::EXCEPTION;
			  }
		  }
		  else {
			  *itsLog << "Invalid channel specification in " << parts[i]
			                                                          << " of spec " << specification << LogIO::EXCEPTION;
		  }
		  if (min > max) {
			  *itsLog << "Min channel " << min << " cannot be greater than max channel "
					  << max << " in " << parts[i] << LogIO::EXCEPTION;
		  }
		  else if (max >= nchan) {
			  *itsLog << "Zero-based max channel " << max
					  << " must be less than the total number of channels ("
					  << nchan << ") in the channel specification " << parts[i] << LogIO::EXCEPTION;
		  }
		  ranges[2*i] = min;
		  ranges[2*i + 1] = max;

	  }
	  Vector<uInt> consolidatedRanges = _consolidateAndOrderRanges(ranges);
	  nSelectedChannels = 0;
	  for (uInt i=0; i<consolidatedRanges.size()/2; i++) {
		  nSelectedChannels += consolidatedRanges[2*i + 1] - consolidatedRanges[2*i] + 1;
	  }
	  return consolidatedRanges;
  }

  Vector<uInt> RegionManager::_consolidateAndOrderRanges(
		  const Vector<uInt>& ranges
  ) const {
	  uInt arrSize = ranges.size()/2;
	  vector<uInt> arrMin(arrSize);
	  vector<uInt> arrMax(arrSize);
	  for (uInt i=0; i<arrSize; i++) {
		  arrMin[i] = ranges[2*i];
		  arrMax[i] = ranges[2*i + 1];
	  }
	  Sort sort;
	  sort.sortKey (&(arrMin[0]), TpUInt);
	  sort.sortKey (&(arrMax[0]), TpUInt, 0, Sort::Descending);
	  Vector<uInt> inxvec;
	  Vector<uInt> consol(0);
	  sort.sort(inxvec, arrSize);
	  for (uInt i=0; i<arrSize; i++) {
		  uInt idx = inxvec(i);
		  uInt size = consol.size();
		  uInt min = arrMin[idx];
		  uInt max = arrMax[idx];
		  uInt lastMax = (i == 0) ? 0 : consol[size-1];
		  if (i==0) {
			  consol.resize(2, True);
			  consol[0] = min;
			  consol[1] = max;
		  }
		  else if (
				  // overlaps previous range, so extend
				  (min < lastMax && max > lastMax)
				  // or contiguous with previous range, so extend
				  || min == lastMax + 1
		  ) {
			  consol[size-1] = max;
		  }

		  else if (min > lastMax + 1) {
			  // non overlap of and not contiguous with previous range,
			  // so create new end point pair
			  uInt newSize = consol.size()+2;
			  consol.resize(newSize, True);
			  consol[newSize-2] = min;
			  consol[newSize-1] = max;
		  }
	  }
	  return consol;
  }

  Vector<uInt> RegionManager::_setPolarizationRanges(
		  String& specification, const String& firstStokes, const uInt nStokes,
		  const StokesControl stokesControl
  ) const {
      LogOrigin origin("ImageInputProcessor", __FUNCTION__);
      *itsLog << origin;

	  Vector<uInt> ranges(0);
	  if (! itsCSys->hasPolarizationAxis()) {
		  return ranges;
	  }
	  specification.trim();
	  specification.upcase();
	  if (specification == ALL) {
		  ranges.resize(2);
		  ranges[0] = 0;
		  ranges[1] = nStokes - 1;
		  return ranges;
	  }
	  if (specification.empty()) {
		  ranges.resize(2);
		  ranges[0] = 0;
		  switch (stokesControl) {
		  case USE_FIRST_STOKES:
			  ranges[1] = 0;
			  specification = firstStokes;
			  break;
		  case USE_ALL_STOKES:
			  ranges[1] = nStokes - 1;
			  specification = ALL;
			  break;
		  default:
			  // bug if we get here
			  *itsLog << "Logic error, unhandled stokes control" << LogIO::EXCEPTION;
		  };
		  return ranges;
	  }
	  // First split on commas and semi-colons.
	  // in the past for polarization specification.

	  Vector<String> parts = stringToVector(specification, Regex("[,;]"));
	  Vector<String> polNames = Stokes::allNames(False);
	  uInt nNames = polNames.size();
	  Vector<uInt> nameLengths(nNames);
	  for (uInt i=0; i<nNames; i++) {
		  nameLengths[i] = polNames[i].length();
	  }
	  uInt *lengthData = nameLengths.data();

	  Vector<uInt> idx(nNames);
	  Sort sorter;
	  sorter.sortKey(lengthData, TpUInt, 0, Sort::Descending);
	  sorter.sort(idx, nNames);

	  Vector<String> sortedNames(nNames);
	  for (uInt i=0; i<nNames; i++) {
		  sortedNames[i] = polNames[idx[i]];
		  sortedNames[i].upcase();
	  }

	  for (uInt i=0; i<parts.size(); i++) {
		  String part = parts[i];

		  Vector<String>::iterator iter = sortedNames.begin();
		  while (iter != sortedNames.end() && ! part.empty()) {
			  if (part.startsWith(*iter)) {
				  Int stokesPix = itsCSys->stokesPixelNumber(*iter);
				  if (stokesPix >= int(nStokes)) {
					  stokesPix = -1;
				  }
				  if (stokesPix >= 0) {
					  uInt newSize = ranges.size() + 2;
					  ranges.resize(newSize, True);
					  ranges[newSize-2] = stokesPix;
					  ranges[newSize-1] = stokesPix;
					  // consume the string
					  part = part.substr(iter->length());
					  if (! part.empty()) {
						  // reset the iterator to start over at the beginning of the list for
						  // the next specified polarization
						  iter = sortedNames.begin();
					  }
				  }
				  else {
					  *itsLog << "Polarization " << *iter << " specified in "
							  << parts[i] << " does not exist in the specified "
							  << "coordinate system for the specified number of "
							  << "polarization parameters" << LogIO::EXCEPTION;
				  }
			  }
			  else {
				  iter++;
			  }
		  }
		  if (! part.empty()) {
			  *itsLog << "(Sub)String " << part << " in stokes specification part " << parts[i]
			                                                                                 << " does not match a known polarization." << LogIO::EXCEPTION;
		  }
	  }
	  return _consolidateAndOrderRanges(ranges);
  }

  Vector<Double> RegionManager::_setBoxCorners(const String& box) const {
	  Vector<String> boxParts = stringToVector(box);
	  AlwaysAssert(boxParts.size() % 4 == 0, AipsError);
	  Vector<Double> corners(boxParts.size());
	  for(uInt i=0; i<boxParts.size()/4; i++) {
		  uInt index = 4*i;
		  for (uInt j=0; j<4; j++) {
			  boxParts[index + j].trim();
			  if (! boxParts[index + j].matches(RXdouble)) {
				  *itsLog << "Box spec contains non numeric characters and so is invalid"
						  << LogIO::EXCEPTION;
			  }
			  corners[index + j] = String::toDouble(boxParts[index + j]);
		  }
	  }
	  return corners;
  }

  Record RegionManager::fromBCS(
		  String& diagnostics, uInt& nSelectedChannels, String& stokes,
		  const Record  * const regionPtr, const String& regionName,
		  const String& chans, const StokesControl stokesControl,
		  const String& box, const IPosition& imShape, const String& imageName
  ) {
	  LogOrigin origin("RegionManager", __FUNCTION__);
	  Record regionRecord;
	  if (! box.empty()) {
		  if (box.freq(",") % 4 != 3) {
			  *itsLog << "box not specified correctly" << LogIO::EXCEPTION;
		  }
		  regionRecord = _fromBCS(
				  diagnostics, nSelectedChannels, stokes,
				  chans, stokesControl, box, imShape
		  ).toRecord("");
		  *itsLog << origin;
		  *itsLog << LogIO::NORMAL << "Using specified box(es) " << box << LogIO::POST;
	  }
	  else if (regionPtr != 0) {
		  _setRegion(regionRecord, diagnostics, regionPtr);
		  *itsLog << origin;
		  *itsLog << LogIO::NORMAL << "Set region from supplied region record"
				  << LogIO::POST;
		  stokes = _stokesFromRecord(regionRecord, stokesControl, imShape);
	  }

	  else if (! regionName.empty()) {
		  _setRegion(regionRecord, diagnostics, regionName, imageName);
		  *itsLog << origin;
		  *itsLog << LogIO::NORMAL << diagnostics << LogIO::POST;
		  stokes = _stokesFromRecord(regionRecord, stokesControl, imShape);
	  }
	  else {
		  Vector<uInt> chanEndPts, polEndPts;
		  regionRecord = _fromBCS(
				  diagnostics, nSelectedChannels, stokes,
				  chans, stokesControl, box, imShape
		  ).toRecord("");
		  *itsLog << origin;
		  *itsLog << LogIO::NORMAL << "No region specified. Using full positional plane."
				  << LogIO::POST;
		  if (chans.empty()) {
			  *itsLog << LogIO::NORMAL << "Using all spectral channels."
					  << LogIO::POST;
		  }
		  else {
			  *itsLog << LogIO::NORMAL << "Using channel range(s) "
					  << _pairsToString(chanEndPts) << LogIO::POST;
		  }
		  if (!stokes.empty()) {
			  *itsLog << LogIO::NORMAL << "Using polarizations " << stokes << LogIO::POST;
		  }
		  else {
			  *itsLog << LogIO::NORMAL << "Using polarization range(s) "
					  << _pairsToString(polEndPts) << LogIO::POST;
		  }
	  }
	  return regionRecord;
  }

  void RegionManager::_setRegion(
  	Record& regionRecord, String& diagnostics,
  	const Record* regionPtr
  )  {
   	// region record pointer provided
   	regionRecord = *(regionPtr->clone());
   	// set stokes from the region record
   	diagnostics = "used provided region record";
  }

  void RegionManager::_setRegion(
  	Record& regionRecord, String& diagnostics,
  	const String& regionName, const String& imageName
  ) {
  	// region name provided
	Regex image("(.*)+:(.*)+");
  	File myFile(regionName);
  	if (myFile.exists()) {
  		Record *rec = readImageFile(regionName, "");
  		regionRecord = *rec;
  		delete rec;
  		diagnostics = "Region read from binary region file " + regionName;
  	}
  	else if (regionName.matches(image) || ! imageName.empty()) {
  		ImageRegion imRegion;
  		String imagename, region;
  		if (regionName.matches(image)) {
  			String res[2];
  			casa::split(regionName, res, 2, ":");
  			imagename = res[0];
  			region = res[1];
  		}
  		else {
  			// imageName is not empty if we get here
  			imagename = imageName;
  			region = regionName;
  		}
  		try {
  			Record *myRec = tableToRecord(imagename, region);
  			if (Table::isReadable(imagename)) {
  				if (myRec == 0) {
  					*itsLog << "Region " << region << " not found in image "
  							<< imagename << LogIO::EXCEPTION;
  				}
  				regionRecord = *myRec;
  				diagnostics = "Used region " + region + " from image "
  					+ imagename + " table description";
  			}
  			else {
  				*itsLog << "Cannot read image " << imagename
  					<< " to get region " << region << LogIO::EXCEPTION;
  			}
  		}
  		catch (AipsError) {
  			*itsLog << "Unable to open region file or region table description "
  					<< region << " in image " << imagename << LogIO::EXCEPTION;
  		}
  	}
  	else {
  		*itsLog << "Unable to open region file or region table description "
  			<< regionName << LogIO::EXCEPTION;
  	}
  }


  ImageRegion RegionManager::_fromBCS(
		  String& diagnostics, uInt& nSelectedChannels, String& stokes,
		  const String& chans,
		  const StokesControl stokesControl, const String& box,
		  const IPosition& imShape
  ) const {
	  Int specAxisNumber = itsCSys->spectralAxisNumber();
	  uInt nTotalChannels = specAxisNumber >= 0 ? imShape[specAxisNumber] : 0;
	  Vector<uInt> chanEndPts = _setSpectralRanges(
			  chans, nSelectedChannels, nTotalChannels
	  );
	  Int polAxisNumber = itsCSys->polarizationAxisNumber();
	  uInt nTotalPolarizations = polAxisNumber >= 0 ? imShape[polAxisNumber] : 0;
	  String firstStokes = polAxisNumber >= 0 ? itsCSys->stokesAtPixel(0) : "";
	  Vector<uInt> polEndPts = _setPolarizationRanges(
			  stokes, firstStokes,
			  nTotalPolarizations, stokesControl
	  );
	  Vector<Double> boxCorners;
	  if (box.empty()) {
	    	if (
	    		itsCSys->hasDirectionCoordinate()
	    		|| itsCSys->hasLinearCoordinate()
	    	) {
	    		Vector<Int> dirAxesNumbers;
	    		if (itsCSys->hasDirectionCoordinate()) {
	    			dirAxesNumbers = itsCSys->directionAxesNumbers();
	    		}
	    		else {
	    			dirAxesNumbers = itsCSys->linearAxesNumbers();

	    		}
	    		Vector<Int> dirShape(2);
	    		dirShape[0] = imShape[dirAxesNumbers[0]];
	    		dirShape[1] = imShape[dirAxesNumbers[1]];
	    		boxCorners.resize(4);
	    		boxCorners[0] = 0;
	    		boxCorners[1] = 0;
	    		boxCorners[2] = dirShape[0] - 1;
	    		boxCorners[3] = dirShape[1] - 1;
	    	}
	  }
	  else {
		  boxCorners = _setBoxCorners(box);
	  }
	  return _fromBCS(
			  diagnostics, boxCorners,
			  chanEndPts, polEndPts, imShape
	  );
  }

  ImageRegion RegionManager::_fromBCS(
		  String& diagnostics, const Vector<Double>& boxCorners,
		  const Vector<uInt>& chanEndPts, const Vector<uInt>& polEndPts,
		  const IPosition imShape
  ) const {
	  LogOrigin origin("ImageInputProcessor", __FUNCTION__);
	  *itsLog << origin;
	  Vector<Double> blc(imShape.nelements(), 0);
	  Vector<Double> trc(imShape.nelements(), 0);
	  Vector<Int> directionAxisNumbers = itsCSys->directionAxesNumbers();
	  Vector<Int> linearAxisNumbers = itsCSys->linearAxesNumbers();

	  Int spectralAxisNumber = itsCSys->spectralAxisNumber();
	  Int polarizationAxisNumber = itsCSys->polarizationAxisNumber();

	  Vector<Double> xCorners(boxCorners.size()/2);
	  Vector<Double> yCorners(xCorners.size());
	  for (uInt i=0; i<xCorners.size(); i++) {
		  Double x = boxCorners[2*i];
		  Double y = boxCorners[2*i + 1];

		  if (x < 0 || y < 0 ) {
			  *itsLog << "blc in box spec is less than 0" << LogIO::EXCEPTION;
		  }
		  if (
			  (
			      itsCSys->hasDirectionCoordinate()
			      && (
			          x >= imShape[directionAxisNumbers[0]]
			          || y >= imShape[directionAxisNumbers[1]]
			      )
			  )
			  || (
		          itsCSys->hasLinearCoordinate()
		          && (
		              x >= imShape[linearAxisNumbers[0]]
		              || y >= imShape[linearAxisNumbers[1]]
		          )
			  )
		  ) {
			  *itsLog << "trc in box spec is greater than or equal to number "
					  << "of direction pixels in the image" << LogIO::EXCEPTION;
		  }
		  xCorners[i] = x;
		  yCorners[i] = y;
	  }
	  Vector<Double> polEndPtsDouble(polEndPts.size());
	  for (uInt i=0; i<polEndPts.size(); i++) {
		  polEndPtsDouble[i] = (Double)polEndPts[i];
	  }

	  Vector<Double> chanEndPtsDouble(chanEndPts.size());
	  for (uInt i=0; i<chanEndPts.size(); i++) {
		  chanEndPtsDouble[i] = (Double)chanEndPts[i];
	  }
	  uInt nRegions = 1;
	  if (itsCSys->hasDirectionCoordinate())  {
		  nRegions *= boxCorners.size()/4;
	  }
	  if (itsCSys->hasLinearCoordinate())  {
		  nRegions *= boxCorners.size()/4;
	  }
	  if (itsCSys->hasPolarizationAxis()) {
		  nRegions *= polEndPts.size()/2;
	  }
	  if (itsCSys->hasSpectralAxis()) {
		  nRegions *= chanEndPts.size()/2;
	  }
	  Vector<Double> extXCorners(2*nRegions);
	  Vector<Double> extYCorners(2*nRegions);
	  Vector<Double> extPolEndPts(2*nRegions);
	  Vector<Double> extChanEndPts(2*nRegions);

	  uInt count = 0;
	  for (uInt i=0; i<max(uInt(1), xCorners.size()/2); i++) {
		  for (uInt j=0; j<max((uInt)1, polEndPts.size()/2); j++) {
			  for (uInt k=0; k<max(uInt(1), chanEndPts.size()/2); k++) {
				  if (
					  itsCSys->hasDirectionCoordinate()
					  || itsCSys->hasLinearCoordinate()
			      ) {
					  extXCorners[2*count] = xCorners[2*i];
					  extXCorners[2*count + 1] = xCorners[2*i + 1];
					  extYCorners[2*count] = yCorners[2*i];
					  extYCorners[2*count + 1] = yCorners[2*i + 1];
				  }
				  if (itsCSys->hasPolarizationAxis()) {
					  extPolEndPts[2*count] = polEndPtsDouble[2*j];
					  extPolEndPts[2*count + 1] = polEndPtsDouble[2*j + 1];
				  }
				  if (itsCSys->hasSpectralAxis()) {
					  extChanEndPts[2*count] = chanEndPtsDouble[2*k];
					  extChanEndPts[2*count + 1] = chanEndPtsDouble[2*k + 1];
				  }
				  count++;
			  }
		  }
	  }
	  HashMap<uInt, Vector<Double> > axisCornerMap;
	  for (uInt i=0; i<nRegions; i++) {
		  for (uInt axisNumber=0; axisNumber<itsCSys->nPixelAxes(); axisNumber++) {
			  if (
			      (
			          directionAxisNumbers.size() > 1
			          && (Int)axisNumber == directionAxisNumbers[0]
			      )
			      || (
				      linearAxisNumbers.size() > 1
				      && (Int)axisNumber == linearAxisNumbers[0]
				  )
			  ) {
				  axisCornerMap(axisNumber) = extXCorners;
			  }
			  else if (
				      (
				          directionAxisNumbers.size() > 1
				          && (Int)axisNumber == directionAxisNumbers[1]
				      )
				      || (
					      linearAxisNumbers.size() > 1
					      && (Int)axisNumber == linearAxisNumbers[1]
					  )
			  ) {
				  axisCornerMap(axisNumber) = extYCorners;
			  }
			  else if ((Int)axisNumber == spectralAxisNumber) {
				  axisCornerMap(axisNumber) = extChanEndPts;
			  }
			  else if ((Int)axisNumber == polarizationAxisNumber) {
				  axisCornerMap(axisNumber) = extPolEndPts;
			  }
			  else {
				  *itsLog << "Unhandled image axis number " << axisNumber
						  << LogIO::EXCEPTION;
			  }
		  }
	  }
	  ImageRegion imRegion;
	  for (uInt i=0; i<nRegions; i++) {
		  for (uInt axisNumber=0; axisNumber<itsCSys->nPixelAxes(); axisNumber++) {
			  blc(axisNumber) = axisCornerMap(axisNumber)[2*i];
			  trc(axisNumber) = axisCornerMap(axisNumber)[2*i + 1];
		  }
		  LCBox lcBox(blc, trc, imShape);
		  WCBox wcBox(lcBox, *itsCSys);
		  ImageRegion thisRegion(wcBox);
		  imRegion = (i == 0)
  				? thisRegion
  				: imRegion = *(doUnion(imRegion, thisRegion));
	  }
	  ostringstream os;
	  os << "Used image region from " << endl;
	  if (itsCSys->hasDirectionCoordinate()) {
		  os << "    position box corners: ";
		  for (uInt i=0; i<boxCorners.size()/4; i++) {
			  os << boxCorners[4*i] << ", " << boxCorners[4*i + 1]
			     << ", " << boxCorners[4*i + 2] << ", " << boxCorners[4*i + 3];
			  if (i < boxCorners.size()/4 - 1) {
				  os << "; ";
			  }
		  }
	  }
	  if (itsCSys->hasSpectralAxis()) {
		  os << "    spectral channel ranges: " << _pairsToString(chanEndPts);
	  }
	  if (itsCSys->hasPolarizationAxis()) {
		  os << "    polarization pixel ranges: " << _pairsToString(polEndPts);
	  }
	  diagnostics = os.str();
	  return imRegion;
  }

  String RegionManager::_pairsToString(const Vector<uInt>& pairs) const {
	  ostringstream os;
	  uInt nPairs = pairs.size()/2;
	  for (uInt i=0; i<nPairs; i++) {
		  os << pairs[2*i] << " - " << pairs[2*i + 1];
		  if (i < nPairs - 1) {
			  os << "; ";
		  }
	  }
	  return os.str();
  }

  String RegionManager::_stokesFromRecord(
  	const Record& region, const StokesControl stokesControl, const IPosition& shape
  ) const {
  	// FIXME This implementation is incorrect for complex, recursive records
      String stokes = "";

      if(! itsCSys->hasPolarizationAxis()) {
    	  return stokes;
      }
      Int polAxis = itsCSys->polarizationAxisNumber();
      uInt stokesBegin;
      uInt stokesEnd=0;
      ImageRegion *imreg = ImageRegion::fromRecord(region, "");
      Array<Float> blc, trc;
      Bool oneRelAccountedFor = False;
      if (imreg->isLCSlicer()) {
    	  blc = imreg->asLCSlicer().blc();
    	  trc = imreg->asLCSlicer().trc();
    	  stokesBegin = (uInt)((Vector<Float>)blc)[polAxis];
    	  stokesEnd = (uInt)((Vector<Float>)trc)[polAxis];
    	  oneRelAccountedFor = True;
      }
      else if (RegionManager::isPixelRegion(*(ImageRegion::fromRecord(region, "")))) {
    	  region.toArray("blc", blc);
    	  region.toArray("trc", trc);
    	  stokesBegin = (uInt)((Vector<Float>)blc)[polAxis];
    	  stokesEnd = (uInt)((Vector<Float>)trc)[polAxis];
      }
      else if (region.fieldNumber("x") >= 0 && region.fieldNumber("y") >= 0) {
    	  // world polygon
    	  oneRelAccountedFor = True;
		  stokesBegin = 0;

    	  if (stokesControl == USE_FIRST_STOKES) {
    		  stokesEnd = 0;
    	  }
    	  else if (stokesControl == USE_ALL_STOKES) {
    		  stokesEnd = shape[polAxis];
    	  }
      }
      else if (region.fieldNumber("blc") >= 0 && region.fieldNumber("blc") >= 0) {
    	  // world box
    	  Record blcRec = region.asRecord("blc");
    	  Record trcRec = region.asRecord("trc");
    	  stokesBegin = (Int)blcRec.asRecord(
    			  String("*" + String::toString(polAxis - 1))
    	  ).asDouble("value");
    	  stokesEnd = (Int)trcRec.asRecord(
    			  String("*" + String::toString(polAxis - 1))
    	  ).asDouble("value");
      }
      else {
    	  // FIXME not very nice, but until all can be implemented this will have to do
    	  *itsLog << LogIO::WARN << "Stokes cannot be determined because this region type is not handled yet"
    			 << LogIO::POST;
    	  return stokes;
      }

      if (! oneRelAccountedFor && region.isDefined("oneRel") && region.asBool("oneRel")) {
    	  stokesBegin--;
    	  stokesEnd--;
      }
      for (uInt i=stokesBegin; i<=stokesEnd; i++) {
    	  stokes += itsCSys->stokesAtPixel(i);
      }
      return stokes;
  }



} // end of  casa namespace
