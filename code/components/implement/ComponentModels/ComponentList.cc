//# ClassFileName.cc:  this defines ClassName, which ...
//# Copyright (C) 1996,1997
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

#include <trial/ComponentModels/ComponentList.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Mathematics/Constants.h>
#include <trial/ComponentModels/GaussianComponent.h>
#include <trial/ComponentModels/PointComponent.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>

ComponentList::
ComponentList()
  :theFileName(), 
   theList(new List<CountedPtr<SkyComponent> >(), True){}

ComponentList::
ComponentList(SkyComponent & component)
  :theFileName(), 
   theList(new List<CountedPtr<SkyComponent> >(), True){
     insertComp(component);
}

ComponentList::
ComponentList(CountedPtr<SkyComponent> ptrComponent)
  :theFileName(), 
   theList(new List<CountedPtr<SkyComponent> >(), True){
     insertComp(ptrComponent);
}

ComponentList::
ComponentList(SkyComponent * ptrComponent)
  :theFileName(), 
   theList(new List<CountedPtr<SkyComponent> >(), True){
     insertComp(ptrComponent);
}

ComponentList::
ComponentList(const String & filename, Bool readOnly)  
  :theFileName(filename), 
   theList(new List<CountedPtr<SkyComponent> >(), True){
     if (readOnly)
       theFileName = "";
     // The table is always opened readonly
     Table table(filename);
     ROScalarColumn<String> typeCol(table, "Type");
     ROArrayColumn<Float> fluxCol(table, "Flux");
     ROArrayColumn<Double> dirCol(table, "Direction");
     ROArrayColumn<Double> parmCol(table, "Parameters");
     TableRecord dirKeywords(dirCol.keywordSet());
     String angleUnit, frame;
     dirKeywords.get("Unit", angleUnit);
     dirKeywords.get("Frame", frame);

     String componentType;
     //
     Vector<Float> flux(4);
     StokesVector sflux;
     //
     Vector<Double> dir(2);
     Quantum<Vector<Double> > qdir(dir, angleUnit);
     MDirection compDir;
     {
       MDirection::Ref refType;
       if (compDir.giveMe(frame, refType) == False)
	 throw(AipsError("ComponentList::ComponentList(const String &, Bool)"
			 " - Could not translate the reference frame"));
       compDir.set(refType);
     }
     //
     uInt nComponents = typeCol.nrow();
     for (uInt i = 0; i < nComponents; i++){
       typeCol.get(i, componentType);
       if (componentType.matches("Point"))
	 addComp(new PointComponent());
       else if (componentType.matches("Gaussian"))
	 addComp(new GaussianComponent());
       else
	 throw(AipsError("ComponentList::ComponentList(const String &, Bool)"
			 " - Unknown component type"));

       fluxCol.get(i, flux);
       sflux = flux;
       getComp()->setFlux(sflux);
       //       cout << " Flux = " << flux.ac();
       dirCol.get(i, dir);
       qdir.setValue(dir);
       compDir.set(qdir);
       //       cout << compDir << endl;
       getComp()->setPosition(compDir);
     }
}

ComponentList::
~ComponentList(){
  if (theFileName.empty() == 0){
    // These two constants define the units and frame of the output list
    const String angleUnits("deg");
    const String refFrame("J2000");
    // Build the table description
    TableDesc td("ComponentListDescription", "1", TableDesc::Scratch);  
    {
      td.comment() = "A description of a component list ";
      ScalarColumnDesc<String> typeCol("Type" ,"Type of the Component");
      td.addColumn (typeCol);

      ArrayColumnDesc<Float> fluxCol("Flux" ,"Stokes I,Q,U,V flux in Jy",
				     IPosition(1,4),  ColumnDesc::Direct);
      fluxCol.rwKeywordSet().define ("Unit", "Jy");
      td.addColumn(fluxCol);

      ArrayColumnDesc<Double> dirCol("Direction" ,"RA/Dec in "
				     + angleUnits + " ("+refFrame+")",
				     IPosition(1,2),  ColumnDesc::Direct);
      dirCol.rwKeywordSet().define ("Unit", angleUnits);
      dirCol.rwKeywordSet().define ("Frame", refFrame);
      td.addColumn(dirCol);
      
      ArrayColumnDesc<Double> 
	parmCol("Parameters", "Parameters specific to this component type", 1);
      td.addColumn(parmCol);
    }
    SetupNewTable newTable(theFileName, td, Table::New);
    Table table(newTable, nComponents(), True);
    ScalarColumn<String> typeCol(table, "Type");
    ArrayColumn<Float> fluxCol(table, "Flux");
    ArrayColumn<Double> dirCol(table, "Direction");
    ArrayColumn<Double> parmCol(table, "Parameters");

    StokesVector flux;
    MDirection compDir;
    Vector<Double> dirn;
    uInt refNum;
    Vector<Double> parameters(3); parameters = 1;
    {
      MDirection::Ref refType;
      if (compDir.giveMe(refFrame, refType) == False)
	throw(AipsError("ComponentList::~ComponentList()"
			" - Could not translate the default reference frame"));
      refNum = refType.getType();
    }
    
    gotoStart();
    for (uInt i = 0; i < nComponents(); i++){
      typeCol.put(i, getComp()->type());
      //
      flux = getComp()->flux();
      fluxCol.put(i, flux.vector());
      //
      compDir = getComp()->position();
      if (compDir.getRef().getType() != refNum)
	compDir = MDirection::Convert(compDir, refNum)();
      dirn = compDir.getAngle().getValue(angleUnits);
      dirCol.put(i, dirn);
      //
      // parameters.resize(0);
      // parameters = getComp()->parameters()
      parmCol.put(i, parameters);
      //
      nextComp();
    }
  }
}

StokesVector ComponentList::
operator()(const MDirection & samplePos) {
  StokesVector result(0.0, 0.0, 0.0, 0.0);
  for (uInt i = 0; i < nComponents(); i++) {
    if (curPosition() == nComponents()) 
      gotoStart();
    else
      nextComp();
    result += getComp()->operator()(samplePos);
  }
  return result;
}

Vector<StokesVector> ComponentList::
operator()(const Vector<MDirection> & samplePos) {
  Vector<StokesVector> result(samplePos.nelements());
  result = StokesVector(0.0, 0.0, 0.0, 0.0);
  for (uInt i = 0; i < samplePos.nelements(); i++) {
    result(i) = operator()(samplePos(i));
  }
  return result;
}

void ComponentList::
operator()(ImageInterface<Float>& plane){
  for (uInt i = 0; i < nComponents(); i++) {
    if (curPosition() == nComponents()) 
      gotoStart();
    else
      nextComp();
    getComp()->operator()(plane);
  }
}

void ComponentList::
operator()(ImageInterface<Float>& plane, const ImageInterface<Float>& psf) {
  for (uInt i = 0; i < nComponents(); i++) {
    if (curPosition() == nComponents()) 
      gotoStart();
    else
      nextComp();
    getComp()->operator()(plane, psf);
  }
}

void ComponentList::
insertComp(SkyComponent & component){
  CountedPtr<SkyComponent> countedPtr(&component, False);
  insertComp(countedPtr);
}

void ComponentList::
insertComp(CountedPtr<SkyComponent> & countedPtr){
  theList.addRight(countedPtr);
}

void ComponentList::
insertComp(SkyComponent * ptrComponent){
  CountedPtr<SkyComponent> countedPtr(ptrComponent);
  insertComp(countedPtr);
}

void ComponentList::
addComp(SkyComponent & component){
  CountedPtr<SkyComponent> countedPtr(&component, False);
  addComp(countedPtr);
}

void ComponentList::
addComp(CountedPtr<SkyComponent> & countedPtr){
  if (nComponents() != 0) theList++;
  insertComp(countedPtr);
}

void ComponentList::
addComp(SkyComponent * ptrComponent){
  CountedPtr<SkyComponent> countedPtr(ptrComponent);
  addComp(countedPtr);
}

void ComponentList::
removeComp(){
  theList.removeRight();
  if ((curPosition() > nComponents()) && (nComponents() != 0))
    theList--;
}

void ComponentList::
gotoStart() {
  theList.toStart();
}

void ComponentList::
gotoEnd() {
  theList.toEnd();
  theList--;
}

void ComponentList::
gotoPosition(uInt index) {
  theList.pos(index-1);
}

void ComponentList::
nextComp() {
  if (curPosition() < nComponents())
    theList++;
}

void ComponentList::
prevComp() {
  if (curPosition() > 1)
    theList--;
}

uInt ComponentList::
nComponents() const {
  return theList.len();
}

uInt ComponentList::
curPosition() const {
  return theList.pos() + 1;
}

CountedPtr<SkyComponent> ComponentList::
getComp(){
  return theList.getRight();
}

void ComponentList::
setListName(const String& filename){
  theFileName = filename;
}

