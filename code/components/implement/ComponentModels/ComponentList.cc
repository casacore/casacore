//# ComponentList.cc:  this defines the ComponentList implementation
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

#include <trial/ComponentModels/ComponentList.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/Images/ImageInterface.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/Quantum.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Sort.h>
#include <aips/Utilities/String.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> 
  gpp_measconvert_mdirection_mvdirection_mcdirection;
#endif

ComponentList::ComponentList()
  :itsList(),
   itsNelements(0),
   itsTable(),
   itsROFlag(False),
   itsSelectedFlags(),
   itsOrder()
{
  AlwaysAssert(ok(), AipsError);
}

ComponentList::ComponentList(const String & fileName, const Bool readOnly)
  :itsList(),
   itsNelements(0),
   itsTable(),
   itsROFlag(False),
   itsSelectedFlags(),
   itsOrder()
{
  {
    if (readOnly) {
      AlwaysAssert(Table::isReadable(fileName), AipsError);
//       itsTable = Table(fileName, TableLock::PermanentLocking, Table::Old);
      itsTable = Table(fileName, Table::Old);
    }
    else {
      AlwaysAssert(Table::isWritable(fileName), AipsError);
//       itsTable = Table(fileName, TableLock::PermanentLocking, Table::Update);
      itsTable = Table(fileName, Table::Old);
    }
  }
  const ROScalarColumn<String> typeCol(itsTable, "Type");
  const ROArrayColumn<Double> fluxCol(itsTable, "Flux");
  const TableRecord fluxKeywords(fluxCol.keywordSet());
  const ROArrayColumn<Double> dirCol(itsTable, "Direction");
  const TableRecord dirKeywords(dirCol.keywordSet());
  const ROArrayColumn<Double> parmCol(itsTable, "Parameters");
  const ROScalarColumn<String> labelCol(itsTable, "Label");
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
  Quantum<Vector<Double> > qFlux;
  {
    String fluxUnit;
    fluxKeywords.get("Unit", fluxUnit);
    qFlux.setUnit(fluxUnit);
  }
  const uInt nComp = typeCol.nrow();
  Vector<Double> flux(4), dir(2), parameters;
  String componentName, compLabel;
  SkyComponent currentComp;
  for (uInt i = 0; i < nComp; i++) {
    typeCol.get(i, componentName);
    currentComp = SkyComponent(ComponentType::shape(componentName));
    fluxCol.get(i, flux); qFlux.setValue(flux); currentComp.setFlux(qFlux);
    dirCol.get(i, dir); qdir.setValue(dir); compDir.set(qdir);
    currentComp.setDirection(compDir);
    labelCol.get(i, compLabel);
    currentComp.setLabel(compLabel);
    parameters.resize(0);
    parmCol.get(i, parameters); currentComp.setParameters(parameters);
    add(currentComp);
  }
  itsROFlag = readOnly;
  AlwaysAssert(ok(), AipsError);
}

ComponentList::ComponentList(const ComponentList & other)
  :itsList(other.itsList),
   itsNelements(other.itsNelements),
   itsTable(other.itsTable),  
   itsROFlag(other.itsROFlag),
   itsSelectedFlags(other.itsSelectedFlags),
   itsOrder(other.itsOrder)
{
  DebugAssert(ok(), AipsError);
}

ComponentList::~ComponentList() {
  if ((itsROFlag == False) && (itsTable.isNull() == False))
    writeTable();
  AlwaysAssert(ok(), AipsError);
}

ComponentList & ComponentList::operator=(const ComponentList & other){
  if (this != &other) {
    if ((itsROFlag == False) && (itsTable.isNull() == False))
      writeTable();
    itsList = other.itsList;
    itsNelements = other.itsNelements;
    itsTable = other.itsTable;
    itsROFlag = other.itsROFlag;
    itsSelectedFlags = other.itsSelectedFlags;
    itsOrder = other.itsOrder;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

void ComponentList::sample(Vector<Double> & result, 
			   const MDirection & sampleDir,
			   const MVAngle & pixelSize) const {
  AlwaysAssert(result.nelements() == 4, AipsError);
  DebugAssert(ok(), AipsError);
  result = 0.0;
  Vector<Double> compResult(4);
  for (uInt i = 0; i < nelements(); i++) {
    component(i).sample(compResult, sampleDir, pixelSize);
    result.ac() += compResult.ac();
  }
}

void ComponentList::project(ImageInterface<Float> & plane) const {
  DebugAssert(ok(), AipsError);
  for (uInt i = 0; i < nelements(); i++) {
    component(i).project(plane);
  }
}

void ComponentList::add(SkyComponent component) {
  AlwaysAssert(itsROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  uInt blockSize = itsList.nelements();
  if (itsNelements == blockSize) {
    const uInt newSize 
      = (blockSize < 50) ? 2 * blockSize + 1 : blockSize + 100;
    itsList.resize(newSize);
    itsSelectedFlags.resize(newSize);
    itsOrder.resize(newSize);
  }
  itsList[itsNelements] = component;
  itsSelectedFlags[itsNelements] = False;
  itsOrder[itsNelements] = itsNelements;
  itsNelements++;
}

void ComponentList::remove(const uInt & index) {
  AlwaysAssert(itsROFlag == False, AipsError);
  AlwaysAssert(index < nelements(), AipsError);
  DebugAssert(ok(), AipsError);
  uInt realIndex = itsOrder[index];
  itsSelectedFlags.remove(realIndex, False);
  itsList.remove(realIndex, False);
  itsOrder.remove(index, False);
  itsNelements--;
  for (uInt i = 0; i < nelements(); i++) {
    if (itsOrder[i] > realIndex) {
      itsOrder[i]--;
    }
  }
}

uInt ComponentList::nelements() const {
  return itsNelements;
}

void ComponentList::deselect(const Vector<Int> & indexes) {
  for (uInt i = 0; i < indexes.nelements(); i++) {
    AlwaysAssert(indexes(i) < Int(nelements()), AipsError);
    AlwaysAssert(indexes(i) >= 0, AipsError);
    itsSelectedFlags[itsOrder[indexes(i)]] = False;
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::select(const Vector<Int> & indexes) {
  for (uInt i = 0; i < indexes.nelements(); i++) {
    AlwaysAssert(indexes(i) < Int(nelements()), AipsError);
    AlwaysAssert(indexes(i) >= 0, AipsError);
    itsSelectedFlags[itsOrder[indexes(i)]] = True;
  }
  DebugAssert(ok(), AipsError);
}

Vector<Int> ComponentList::selected() const {
  DebugAssert(ok(), AipsError);
  uInt nSelected = 0;
  for (uInt i = 0; i < nelements(); i++) {
    if (itsSelectedFlags[i] == True) {
      nSelected++;
    }
  }
  Vector<Int> retVal(nSelected);
  uInt s = 0;
  for (uInt j = 0; j < nelements(); j++) {
    if (itsSelectedFlags[j] == True) {
      retVal(s) = j;
      s++;
    }
  }
  return retVal;
}

SkyComponent & ComponentList::component(const uInt & index) {
  AlwaysAssert(itsROFlag == False, AipsError);
  AlwaysAssert(index < nelements(), AipsError);
  DebugAssert(ok(), AipsError);
  return itsList[itsOrder[index]];
}

const SkyComponent & ComponentList::component(const uInt & index) const {
  DebugAssert(ok(), AipsError);
  AlwaysAssert(index < nelements(), AipsError);
  return itsList[itsOrder[index]];
}

void ComponentList::rename(const String & fileName, 
			   const Table::TableOption option) {
  AlwaysAssert(option != Table::Old, AipsError);
  AlwaysAssert(itsROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  if (fileName == "") {
    if (!itsTable.isNull()) {
      itsTable.markForDelete();
      itsTable = Table();
      itsROFlag = False;
    }
    return;
  }
  // See if this list is associated with a Table. 
  if (!itsTable.isNull()) {
    if (itsTable.isWritable() == False)
      itsTable.reopenRW();
    itsTable.rename(fileName, option);
  }
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
      
      ScalarColumnDesc<String> labelCol("Label" ,
					"An arbitrary label for the user");
      td.addColumn (labelCol);

      ArrayColumnDesc<Double> 
	parmCol("Parameters", "Parameters specific to this component type", 1);
      td.addColumn(parmCol);
    }
    SetupNewTable newTable(fileName, td, option);
    itsTable = Table(newTable, TableLock::PermanentLocking, nelements(), True);
  }
  // Ensure that the Table::isReadable(fileName) returns True, otherwise the
  // ok() function will fail.
  itsTable.flush();
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

void ComponentList::sort(ComponentList::SortCriteria criteria) {
  Block<Double> val(nelements());
  Sort::Order order;
  Bool doSort = True;
  switch (criteria) {
  case ComponentList::FLUX: {
    Quantum<Vector<Double> > compFlux;
    for (uInt i = 0; i < nelements(); i++) {
      itsList[i].flux(compFlux);
      val[i] = abs(compFlux.getValue("Jy")(0));
    }
    order = Sort::Descending;
    break;
  }
  case ComponentList::POSITION: {
    MDirection compDir;
    MVDirection refDir(0.0, 0.0);
    Vector<Double> position(2);
    for (uInt i = 0; i < nelements(); i++) {
      itsList[i].direction(compDir);
      val[i] = refDir.separation(compDir.getValue());
    }
    order = Sort::Ascending;
    break;
  }
  case ComponentList::POLARISATION: {
    Quantum<Vector<Double> > compFlux;
    Vector<Double> f;
    for (uInt i = 0; i < nelements(); i++) {
      itsList[i].flux(compFlux);
      f = compFlux.getValue("Jy");
      if (!nearAbs(f(0), 0.0, DBL_MIN)) {
	val[i] = sqrt(f(1)*f(1)+f(2)*f(2)+f(3)*f(3))/f(0);
      }
      else {
	val[i] = 0.0;
      }
    }
    order = Sort::Descending;
    break;
  }
  case ComponentList::UNSORTED: 
  case ComponentList::NUMBER_CRITERIA:
    doSort = False;
    break;
  };
  // The genSort function requires a Vector<uInt> and not a Block<uInt> so
  // I'll create a temporary Vector here which references the data in the
  // 'itsOrder' Block.
  if (doSort) {
    Vector<uInt> vecOrder(IPosition(1,nelements()), itsOrder.storage(), SHARE);
    AlwaysAssert(genSort(vecOrder, val, order) == nelements(), 
		 AipsError);
  }
}

String ComponentList::name(ComponentList::SortCriteria enumerator) {
  switch (enumerator) {
  case ComponentList::FLUX: return "Flux";
  case ComponentList::POSITION: return "Position";
  case ComponentList::POLARISATION: return "Polarisation";
  default: return "unknown";
  };
}

ComponentList::SortCriteria ComponentList::type(const String & criteria) {
  String canonicalCase(criteria);
  canonicalCase.capitalize();
  for (uInt i = 0; i < ComponentList::NUMBER_CRITERIA; i++) {
    if (canonicalCase.
	matches(ComponentList::name((ComponentList::SortCriteria) i))) {
      return (ComponentList::SortCriteria) i;
    }
  }
  return ComponentList::UNSORTED;
}

Bool ComponentList::ok() const {
  // The LogIO class is only constructed if an Error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (itsList.nelements() < itsNelements) {
    LogIO logErr(LogOrigin("ComponentList", "ok()"));
    logErr << LogIO::SEVERE 
	   << "The list size is inconsistant with its cached size"
           << LogIO::POST;
     return False;
  }
  if (itsROFlag == True && itsTable.isNull() == True) {
    LogIO logErr(LogOrigin("ComponentList", "ok()"));
    logErr << LogIO::SEVERE 
	   << "Only ComponentList's associated with a Table can be readonly"
           << LogIO::POST;
     return False;
  }
  if (itsTable.isNull() == False) {
    String tablename = itsTable.tableName();
    if (Table::isReadable(tablename) == False) {
	LogIO logErr(LogOrigin("ComponentList", "ok()"));
	logErr << LogIO::SEVERE 
	       << "Table associated with ComponentList is not readable"
	       << LogIO::POST;
	return False;
    }
    if (itsROFlag == False && Table::isWritable(tablename) == False) {
	LogIO logErr(LogOrigin("ComponentList", "ok()"));
	logErr << LogIO::SEVERE 
	       << "Table associated with ComponentList is not writeable"
	       << LogIO::POST;
	return False;
    }
  }
  for (uInt i = 0; i < itsNelements; i++) {
    if (itsOrder[i] >= itsNelements) {
      LogIO logErr(LogOrigin("ComponentList", "ok()"));
      logErr << LogIO::SEVERE 
	     << "Cannot index to an element that is outside the list!"
	     << LogIO::POST;
      return False;
    }
  }
  return True;
}

void ComponentList::writeTable() {
  if (itsTable.isWritable() == False)
    itsTable.reopenRW();
  DebugAssert(itsTable.isWritable(), AipsError);
  
  {
    const uInt nRows = itsTable.nrow();
    if (nRows < nelements())
      itsTable.addRow(nelements()-nRows);
    else if (nRows > nelements())
      for (uInt r = nRows-1; r >= nelements(); r--)
	itsTable.removeRow(r);
  }
  ScalarColumn<String> typeCol(itsTable, "Type");
  ArrayColumn<Double> fluxCol(itsTable, "Flux");
  TableRecord fluxKeywords(fluxCol.keywordSet());
  ArrayColumn<Double> dirCol(itsTable, "Direction");
  TableRecord dirKeywords(dirCol.keywordSet());
  ScalarColumn<String> labelCol(itsTable, "Label");
  ArrayColumn<Double> parmCol(itsTable, "Parameters");
  
  MDirection compDir;
  uInt refNum;
  {
    MDirection::Ref refType;
    String refFrame;
    dirKeywords.get("Frame", refFrame);
    AlwaysAssert(compDir.giveMe(refFrame, refType), AipsError);
    refNum = refType.getType();
  }
  String fluxUnits;
  fluxKeywords.get("Unit", fluxUnits);
  Quantum<Vector<Double> > compFlux(Vector<Double>(4,0.0), fluxUnits);
  String angleUnits;
  dirKeywords.get("Unit", angleUnits);
  Vector<Double> dirn;
  Vector<Double> compParms;
  String compLabel;
  for (uInt i = 0; i < nelements(); i++) {
    typeCol.put(i, ComponentType::name(component(i).shapeType()));
    component(i).flux(compFlux);fluxCol.put(i, compFlux.getValue(fluxUnits));
    component(i).direction(compDir);
    if (compDir.getRef().getType() != refNum)
      compDir = MDirection::Convert(compDir, refNum)();
    dirn = compDir.getAngle().getValue(angleUnits);
    dirCol.put(i, dirn);
    component(i).label(compLabel);
    labelCol.put(i, compLabel);
    compParms.resize(component(i).nParameters());
    component(i).parameters(compParms);
    parmCol.put(i, compParms);
  }
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentList"
// End: 
