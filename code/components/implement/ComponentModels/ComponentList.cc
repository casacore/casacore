//# ComponentList.cc:  this defines the ComponentList implementation
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
#include <trial/ComponentModels/ComponentType.h>
#include <trial/Images/ImageInterface.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/Quantum.h>
#include <aips/OS/Directory.h>
#include <aips/OS/File.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
// #include <iostream.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> gpp_complist_bug1;
#endif

ComponentList::ComponentList()
  :theList(),
   theNelements(0),
   theFileName("defaultComponentList")
{
}

ComponentList::ComponentList(const File & fileName, const Bool readOnly)
  :theList(),
   theNelements(0),
   theFileName("defaultComponentList") 
{
  {
    // The table is always opened readonly
    const Table table(fileName.path().expandedName(), Table::Old);
    const ROScalarColumn<String> typeCol(table, "Type");
    const ROArrayColumn<Double> fluxCol(table, "Flux");
    const ROArrayColumn<Double> dirCol(table, "Direction");
    const TableRecord dirKeywords(dirCol.keywordSet());
    const ROArrayColumn<Double> parmCol(table, "Parameters");
    MDirection compDir;
    {
      MDirection::Ref refType;
      String frame;
      dirKeywords.get("Frame", frame);
      AlwaysAssert(compDir.giveMe(frame, refType), AipsError);
      compDir.set(refType);
    }
    Vector<Double> dir(2);
    Quantum<Vector<Double> > qdir(dir);
    {
      String angleUnit;
      dirKeywords.get("Unit", angleUnit);
      qdir.setUnit(angleUnit);
    }
    const uInt nComp = typeCol.nrow();
    Vector<Double> flux, parameters;
    String componentName;
    SkyComponent currentComp;
    for (uInt i = 0; i < nComp; i++) {
      typeCol.get(i, componentName);
      currentComp = SkyComponent(ComponentType::type(componentName));
      fluxCol.get(i, flux); currentComp.setFlux(flux);
      dirCol.get(i, dir); qdir.setValue(dir); compDir.set(qdir);
      currentComp.setPosition(compDir);
      parmCol.get(i, parameters); currentComp.setParameters(parameters);
      add(currentComp);
    }
  }
  if (!readOnly)
    setName(fileName);
}

ComponentList::~ComponentList() {
  if (theFileName.path().expandedName() != "defaultComponentList"){
    // The setName function must have been called. 
    // Assume the file can be created.
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
    Table::TableOption tableStatus;
    if (theFileName.exists()) {
      AlwaysAssert(theFileName.isWritable(), AipsError);
      tableStatus = Table::Update;
    }
    else {
      Directory dirName(theFileName);
      AlwaysAssert(dirName.isWritable(), AipsError);
      tableStatus = Table::New;
    }
    SetupNewTable newTable(theFileName.path().expandedName(), td, tableStatus);
    Table table(newTable, nelements(), True);
    ScalarColumn<String> typeCol(table, "Type");
    ArrayColumn<Double> fluxCol(table, "Flux");
    ArrayColumn<Double> dirCol(table, "Direction");
    ArrayColumn<Double> parmCol(table, "Parameters");

    MDirection compDir;
    Vector<Double> dirn;
    uInt refNum;
    {
      MDirection::Ref refType;
      AlwaysAssert(compDir.giveMe(refFrame, refType), AipsError);
      refNum = refType.getType();
    }
    Vector<Double> compFlux(4), compParms;
    for (uInt i = 0; i < nelements(); i++) {
      typeCol.put(i, component(i).type());
      component(i).flux(compFlux);
      fluxCol.put(i, compFlux);
      component(i).position(compDir);
      if (compDir.getRef().getType() != refNum)
 	compDir = MDirection::Convert(compDir, refNum)();
      dirn = compDir.getAngle().getValue(angleUnits);
      dirCol.put(i, dirn);
      compParms.resize(component(i).nParameters());
      component(i).parameters(compParms);
      parmCol.put(i, compParms);
    }
  }
}

void ComponentList::sample(Vector<Double> & result, const MDirection & samplePos) const {
  result = 0.0;
  Vector<Double> compResult(4);
  for (uInt i = 0; i < nelements(); i++) {
    component(i).sample(compResult, samplePos);
    result.ac() += compResult.ac();
  }
}

void ComponentList::project(ImageInterface<Float> & plane) const {
  for (uInt i = 0; i < nelements(); i++)
    component(i).project(plane);
}

void ComponentList::add(SkyComponent & component) {
  uInt blockSize = theList.nelements();
  if (theNelements == blockSize) {
    uInt newSize = (blockSize < 50) ? 2 * blockSize + 1 : blockSize + 100;
    theList.resize(newSize);
  }
  theList[theNelements] = component;
  theNelements++;
}

void ComponentList::remove(uInt index) {
  theList.remove(index, False);
  theNelements--;
}

uInt ComponentList::nelements() const {
  return theNelements;
}

SkyComponent & ComponentList::component(uInt index) {
  return theList[index];
}

const SkyComponent & ComponentList::component(uInt index) const {
  return theList[index];
}

void ComponentList::setName(const File & fileName) {
  theFileName = fileName;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentList"
// End: 
