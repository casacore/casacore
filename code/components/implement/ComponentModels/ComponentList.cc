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
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/Quantum.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> 
  gpp_measconvert_mdirection_mvdirection_mcdirection;
#endif

ComponentList::ComponentList()
  :theList(),
   theNelements(0),
   theTable(),
   theROFlag(False)
{
  AlwaysAssert(ok(), AipsError);
}

ComponentList::ComponentList(const String & fileName, const Bool readOnly)
  :theList(),
   theNelements(0),
   theTable(),
   theROFlag(False)
{
  {
    if (readOnly) {
      AlwaysAssert(Table::isReadable(fileName), AipsError);
      theTable = Table(fileName, Table::Old);
    }
    else {
      AlwaysAssert(Table::isWritable(fileName), AipsError);
      theTable = Table(fileName, Table::Update);
    }
  }
  const ROScalarColumn<String> typeCol(theTable, "Type");
  const ROArrayColumn<Double> fluxCol(theTable, "Flux");
  const ROArrayColumn<Double> dirCol(theTable, "Direction");
  const TableRecord dirKeywords(dirCol.keywordSet());
  const ROArrayColumn<Double> parmCol(theTable, "Parameters");
  MDirection compDir;
  {
    MDirection::Ref refType;
    String frame;
    dirKeywords.get("Frame", frame);
    AlwaysAssert(compDir.giveMe(frame, refType), AipsError);
    compDir.set(refType);
  }
  Quantum<Vector<Double> > qdir;
  {
    String angleUnit;
    dirKeywords.get("Unit", angleUnit);
    qdir.setUnit(angleUnit);
  }
  const uInt nComp = typeCol.nrow();
  Vector<Double> flux(4), dir(2), parameters;
  String componentName;
  SkyComponent currentComp;
  for (uInt i = 0; i < nComp; i++) {
    typeCol.get(i, componentName);
    currentComp = SkyComponent(ComponentType::type(componentName));
    fluxCol.get(i, flux); currentComp.setFlux(flux);
    dirCol.get(i, dir); qdir.setValue(dir); compDir.set(qdir);
    currentComp.setPosition(compDir);
    parameters.resize(0);
    parmCol.get(i, parameters); currentComp.setParameters(parameters);
    add(currentComp);
  }
  theROFlag = readOnly;
  AlwaysAssert(ok(), AipsError);
}

ComponentList::ComponentList(const ComponentList & other)
  :theList(other.theList),
   theNelements(other.theNelements),
   theTable(other.theTable),  
   theROFlag(other.theROFlag)
{
  DebugAssert(ok(), AipsError);
}

ComponentList::~ComponentList() {
  if ((theROFlag == False) && (theTable.isNull() == False)) {
    AlwaysAssert(theTable.isWritable(), AipsError);
    const uInt nComps = nelements();
    {
      const uInt nRows = theTable.nrow();
      if (nRows < nComps)
	theTable.addRow(nComps-nRows);
      else if (nRows > nComps)
	for (uInt r = nRows-1; r >= nComps; r--)
	  theTable.removeRow(r);
    }
      
    ScalarColumn<String> typeCol(theTable, "Type");
    ArrayColumn<Double> fluxCol(theTable, "Flux");
    ArrayColumn<Double> dirCol(theTable, "Direction");
    TableRecord dirKeywords(dirCol.keywordSet());
    ArrayColumn<Double> parmCol(theTable, "Parameters");
    
    MDirection compDir;
    uInt refNum;
    {
      MDirection::Ref refType;
      String refFrame;
      dirKeywords.get("Frame", refFrame);
      AlwaysAssert(compDir.giveMe(refFrame, refType), AipsError);
      refNum = refType.getType();
    }
    String angleUnits;
    dirKeywords.get("Unit", angleUnits);
    Vector<Double> dirn;
    Vector<Double> compFlux(4), compParms;
    for (uInt i = 0; i < nComps; i++) {
      typeCol.put(i, ComponentType::name(component(i).type()));
      component(i).flux(compFlux); fluxCol.put(i, compFlux);
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
  AlwaysAssert(ok(), AipsError);
}

ComponentList & ComponentList::operator=(const ComponentList & other){
  if (this != &other) {
    theList = other.theList;
    theNelements = other.theNelements;
    theTable = other.theTable;
    theROFlag = other.theROFlag;
  }
  DebugAssert(ok(), AipsError);
  return *this;
};

void ComponentList::sample(Vector<Double> & result, 
			   const MDirection & samplePos,
			   const MVAngle & pixelSize) const {
  AlwaysAssert(result.nelements() == 4, AipsError);
  DebugAssert(ok(), AipsError);
  result = 0.0;
  Vector<Double> compResult(4);
  for (uInt i = 0; i < nelements(); i++) {
    component(i).sample(compResult, samplePos, pixelSize);
    result.ac() += compResult.ac();
  }
}

void ComponentList::project(ImageInterface<Float> & plane) const {
  DebugAssert(ok(), AipsError);
  for (uInt i = 0; i < nelements(); i++)
    component(i).project(plane);
}

void ComponentList::add(SkyComponent component) {
  AlwaysAssert(theROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  uInt blockSize = theList.nelements();
  if (theNelements == blockSize) {
    uInt newSize = (blockSize < 50) ? 2 * blockSize + 1 : blockSize + 100;
    theList.resize(newSize);
  }
  theList[theNelements] = component;
  theNelements++;
}

void ComponentList::remove(uInt index) {
  AlwaysAssert(theROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  theList.remove(index, False);
  theNelements--;
}

uInt ComponentList::nelements() const {
  return theNelements;
}

SkyComponent & ComponentList::component(uInt index) {
  AlwaysAssert(theROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  return theList[index];
}

const SkyComponent & ComponentList::component(uInt index) const {
  DebugAssert(ok(), AipsError);
  return theList[index];
}

void ComponentList::rename(const String & fileName, 
			   const Table::TableOption option) {
  AlwaysAssert(option != Table::Old, AipsError);
  AlwaysAssert(theROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  // See if this list is associated with a Table. 
  if (!theTable.isNull())
      theTable.rename(fileName, option);
  // Otherwise construct a Table to hold the list
  else { 
    // These two constants define the units and frame of the output list
    const String angleUnits("deg");
    const String refFrame("J2000");
    // Build a default table description
    TableDesc td("ComponentListDescription", "1", TableDesc::Scratch);  
    {
      td.comment() = "A description of a component list ";
      ScalarColumnDesc<String> typeCol("Type" ,"Type of the Component");
      td.addColumn (typeCol);
      
      ArrayColumnDesc<Double> fluxCol("Flux" ,"Stokes I,Q,U,V flux in Jy",
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
    SetupNewTable newTable(fileName, td, option);
    theTable = Table(newTable, nelements(), True);
  }
  DebugAssert(ok(), AipsError);
}

ComponentList ComponentList::copy() const {
  DebugAssert(ok(), AipsError);
  ComponentList copiedList;
  SkyComponent currentComp;
  for (uInt c = 0; c < nelements(); c++) {
    currentComp = component(c).copy();
    copiedList.add(currentComp);
  }
  return copiedList;
}

Bool ComponentList::ok() const {
  return True;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentList"
// End: 
