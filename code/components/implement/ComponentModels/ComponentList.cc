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
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <trial/Images/ImageInterface.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <trial/TableMeasures/TableMeasValueDesc.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MVFrequency.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/Unit.h>
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
typedef Flux<Double> gpp_flux_double;
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
  readTable(fileName, readOnly);
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

Flux<Double> ComponentList::sample(const MDirection & sampleDir,
				   const MVAngle & pixelSize,
				   const MFrequency & centerFreq) const {
  DebugAssert(ok(), AipsError);
  const Unit retUnit("Jy");
  const ComponentType::Polarisation retPol(ComponentType::STOKES);
  Vector<DComplex> result(4, DComplex(0,0));
  Flux<Double> compFlux;
  for (uInt i = 0; i < nelements(); i++) {
    compFlux = component(i).sample(sampleDir, pixelSize, centerFreq);
    compFlux.convertUnit(retUnit);
    compFlux.convertPol(retPol);
    result.ac() += compFlux.value().ac();
  }
  return Flux<Double>(result, retPol);
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

void ComponentList::remove(const Vector<Int> & indices) {
  AlwaysAssert(allGE(indices.ac(), 0), AipsError);
  uInt c = indices.nelements();
  Vector<uInt> uIndices(c);
  convertArray(uIndices.ac(), indices.ac());
  GenSort<uInt>::sort(uIndices);
  while (c != 0) {
    c--;
    remove(uIndices(c));
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

void ComponentList::setLabel(const Vector<Int> & which,
			     const String & newLabel) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).label() = newLabel;
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setFlux(const Vector<Int> & which,
			    const Flux<Double> & newFlux) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).flux() = newFlux;
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::convertFluxUnit(const Vector<Int> & which,
				    const Unit & unit) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).flux().convertUnit(unit);
  }
  DebugAssert(ok(), AipsError);
}
  
void ComponentList::convertFluxPol(const Vector<Int> & which,
				   ComponentType::Polarisation pol) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).flux().convertPol(pol);
  }
  DebugAssert(ok(), AipsError);
}
 
void ComponentList::setRefDirection(const Vector<Int> & which,
				    const MVDirection & newDir) {
  uInt c;
  MDirection curDir;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    ComponentShape & curShape = component(c).shape();
    curDir = curShape.refDirection();
    curDir.set(newDir);
    curShape.setRefDirection(curDir);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setRefDirectionFrame(const Vector<Int> & which,
					 MDirection::Types newFrame) {
  uInt c;
  MDirection curDir;
  const MDirection::Ref newRef(newFrame);
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    ComponentShape & curShape = component(c).shape();
    curDir = curShape.refDirection();
    curDir.set(newRef);
    curShape.setRefDirection(curDir);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::convertRefDirection(const Vector<Int> & which,
					MDirection::Types newFrame) {
  uInt c;
  MDirection::Convert converter;
  converter.setOut(newFrame);
  MDirection curDir;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    ComponentShape & curShape = component(c).shape();
    curDir = curShape.refDirection();
    curShape.setRefDirection(converter(curDir));
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setShape(const Vector<Int> & which,
			     const ComponentShape & newShape) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).setShape(newShape);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setShapeParms(const Vector<Int> & which,
				  const ComponentShape & newShape) {
  uInt c;
  MDirection oldDir;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SkyComponent & comp = component(c);
    oldDir = comp.shape().refDirection();
    component(c).setShape(newShape);
    comp.shape().setRefDirection(oldDir);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setSpectrum(const Vector<Int> & which,
				const SpectralModel & newSpectrum) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).setSpectrum(newSpectrum);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setSpectrumParms(const Vector<Int> & which,
				     const SpectralModel & newSpectrum) {
  uInt c;
  MFrequency oldFreq;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SkyComponent & comp = component(c);
    oldFreq = comp.spectrum().refFrequency();
    component(c).setSpectrum(newSpectrum);
    comp.spectrum().setRefFrequency(oldFreq);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setRefFrequency(const Vector<Int> & which, 
				    const MVFrequency & newFreq) {
  uInt c;
  MFrequency curFreq;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SpectralModel & curSpectrum = component(c).spectrum();
    curFreq = curSpectrum.refFrequency();
    curFreq.set(newFreq);
    curSpectrum.setRefFrequency(curFreq);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setRefFrequencyFrame(const Vector<Int> & which,
					 MFrequency::Types newFrame) {
  uInt c;
  MFrequency curFreq;
  const MFrequency::Ref newRef(newFrame);
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SpectralModel & curSpectrum = component(c).spectrum();
    curFreq = curSpectrum.refFrequency();
    curFreq.set(newRef);
    curSpectrum.setRefFrequency(curFreq);
  }
  DebugAssert(ok(), AipsError);
}


void ComponentList::setRefFrequencyUnit(const Vector<Int> & which,
					const Unit & unit) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).spectrum().convertFrequencyUnit(unit);
  }
  DebugAssert(ok(), AipsError);
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
  if (itsTable.isNull()) {
    createTable(fileName, option);
  } else {
    if (!itsTable.isWritable()) itsTable.reopenRW();
    itsTable.rename(fileName, option);
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
  Sort::Order order = Sort::Ascending;
  Bool doSort = True;
  switch (criteria) {
  case ComponentList::FLUX: {
    for (uInt i = 0; i < nelements(); i++) {
      itsList[i].flux().convertPol(ComponentType::STOKES);
      val[i] = abs(itsList[i].flux().value(0u).re);
    }
    order = Sort::Descending;
    break;
  }
  case ComponentList::POSITION: {
    MDirection compDir;
    MVDirection refDir(0.0, 0.0);
    Vector<Double> position(2);
    for (uInt i = 0; i < nelements(); i++) {
      val[i] = refDir.separation(itsList[i].shape().refDirection().getValue());
    }
    order = Sort::Ascending;
    break;
  }
  case ComponentList::POLARISATION: {
    Vector<Double> f(4);
    for (uInt i = 0; i < nelements(); i++) {
      itsList[i].flux().value(f);
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

void ComponentList::createTable(const String & fileName,
				const Table::TableOption option) {
  // Build a default table description
  TableDesc td("ComponentListDescription", "3", TableDesc::Scratch);  
  td.comment() = "A description of a component list";
  {
    {
      const ArrayColumnDesc<DComplex> 
	fluxValCol("Flux", "Flux values", IPosition(1,4), ColumnDesc::Direct);
      td.addColumn(fluxValCol);
      const ScalarColumnDesc<String>
	fluxUnitCol("Flux Unit", "Flux units", ColumnDesc::Direct);
      td.addColumn(fluxUnitCol);
      const ScalarColumnDesc<String> 
	fluxPolCol("Flux Polarisation", "Flux polarisation representation", 
		   ColumnDesc::Direct);
      td.addColumn(fluxPolCol);
    }
    {
      const ScalarColumnDesc<String> 
	shapeCol("Shape", "Shape of the Component", ColumnDesc::Direct);
      td.addColumn(shapeCol);
      const String dirValColName = "Reference Direction";
      const ArrayColumnDesc<Double> 
	dirValCol(dirValColName, "Reference direction values",
		  IPosition(1,3), ColumnDesc::Direct);
      td.addColumn(dirValCol);
      const String dirRefColName = "Direction Frame";
      const ScalarColumnDesc<String>
	dirRefCol(dirRefColName, "The reference direction frame", 
		  ColumnDesc::Direct);
      td.addColumn(dirRefCol);
      const TableMeasRefDesc dirRefTMCol(td, dirRefColName);
      const TableMeasValueDesc dirValTMCol(td, dirValColName);
      TableMeasDesc<MDirection> dirTMCol(dirValTMCol, dirRefTMCol);
      dirTMCol.write(td);
      const ArrayColumnDesc<Double> 
	shapeParmCol("Shape Parameters",
		     "Parameters specific to the component shape", 1);
      td.addColumn(shapeParmCol);
    }
    {
      const ScalarColumnDesc<String>
	freqShapeCol("Spectrum Shape", "Shape of the spectrum", 
		     ColumnDesc::Direct);
      td.addColumn (freqShapeCol);
      const String freqValColName = "Reference Frequency";
      const ArrayColumnDesc<Double>
	freqValCol(freqValColName, "The reference frequency values", 
		     IPosition(1,1), ColumnDesc::Direct);
      td.addColumn(freqValCol);
      const String freqRefColName = "Frequency Frame";
      const ScalarColumnDesc<String>
	freqRefCol(freqRefColName, "The reference frequency frame", 
		   ColumnDesc::Direct);
      td.addColumn(freqRefCol);
      const TableMeasRefDesc freqRefTMCol(td, freqRefColName);
      const TableMeasValueDesc freqValTMCol(td, freqValColName);
      TableMeasDesc<MFrequency> freqTMCol(freqValTMCol, freqRefTMCol);
      freqTMCol.write(td);
      const ArrayColumnDesc<Double> 
	specParmCol("Spectral Shape Parameters", 
		    "Parameters specific to the components spectrum", 1);
      td.addColumn(specParmCol);
    }
    {
      const ScalarColumnDesc<String> 
	labelCol("Label", "An arbitrary label for the user",
		 ColumnDesc::Direct);
      td.addColumn (labelCol);
    }
  }
  SetupNewTable newTable(fileName, td, option);
  itsTable = Table(newTable, TableLock::PermanentLocking, nelements(), True);
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
  ArrayColumn<DComplex> fluxValCol(itsTable, "Flux");
  ScalarColumn<String> fluxUnitCol(itsTable, "Flux Unit");
  ScalarColumn<String> fluxPolCol(itsTable, "Flux Polarisation");
  ScalarColumn<String> shapeCol(itsTable, "Shape");
  ScalarMeasColumn<MDirection,MVDirection> dirCol(itsTable,
						  "Reference Direction");
  ArrayColumn<Double> shapeParmCol(itsTable, "Shape Parameters");
  ScalarColumn<String> specShapeCol(itsTable, "Spectrum Shape");
  ScalarMeasColumn<MFrequency,MVFrequency> freqCol(itsTable,
						  "Reference Frequency");
  ArrayColumn<Double> specShapeParmCol(itsTable, "Spectral Shape Parameters");
  ScalarColumn<String> labelCol(itsTable, "Label");
  
  MDirection compDir;
  Vector<Double> shapeParms, spectralParms;
  String compLabel;
  for (uInt i = 0; i < nelements(); i++) {
    {
      fluxValCol.put(i, component(i).flux().value());
      fluxUnitCol.put(i, component(i).flux().unit().getName());
      fluxPolCol.put(i, ComponentType::name(component(i).flux().pol()));
    }
    {
      const ComponentShape & compShape = component(i).shape();
      shapeCol.put(i, ComponentType::name(compShape.type()));
      compDir = compShape.refDirection();
      dirCol.put(i, compDir);
      shapeParms.resize(compShape.nParameters());
      compShape.parameters(shapeParms);
      shapeParmCol.put(i, shapeParms);
    }
    {
      const SpectralModel & compSpectrum = component(i).spectrum();
      specShapeCol.put(i, ComponentType::name(compSpectrum.type()));
      freqCol.put(i, compSpectrum.refFrequency());
      spectralParms.resize(compSpectrum.nParameters());
      compSpectrum.parameters(spectralParms);
      specShapeParmCol.put(i, spectralParms);
    }
    {
      labelCol.put(i, component(i).label());
    }
  }
}

void ComponentList::readTable(const String & fileName, const Bool readOnly) {
  {
    if (readOnly) {
      AlwaysAssert(Table::isReadable(fileName), AipsError);
//       itsTable = Table(fileName, TableLock::PermanentLocking, Table::Old);
      //      itsTable = Table(fileName, Table::Old);
      itsTable = Table(fileName, Table::Update);
    }
    else {
      AlwaysAssert(Table::isWritable(fileName), AipsError);
//       itsTable = Table(fileName, TableLock::PermanentLocking, Table::Update);
      //      itsTable = Table(fileName, Table::Old);
      itsTable = Table(fileName, Table::Update);
    }
  }
  const ROArrayColumn<DComplex> fluxValCol(itsTable, "Flux");
  const ROScalarColumn<String> fluxUnitCol(itsTable, "Flux Unit");
  const ROScalarColumn<String> fluxPolCol(itsTable, "Flux Polarisation");
  const ROScalarColumn<String> shapeCol(itsTable, "Shape");
  const ROScalarMeasColumn<MDirection,MVDirection> 
    dirCol(itsTable, "Reference Direction");
  const ROArrayColumn<Double> shapeParmCol(itsTable, "Shape Parameters");
  const ROScalarColumn<String> specShapeCol(itsTable, "Spectrum Shape");
  const ROScalarMeasColumn<MFrequency,MVFrequency> 
    freqCol(itsTable, "Reference Frequency");
  const ROArrayColumn<Double> 
    specShapeParmCol(itsTable, "Spectral Shape Parameters");
  const ROScalarColumn<String> labelCol(itsTable, "Label");
  SkyComponent currentComp;
  const uInt nComp = fluxValCol.nrow();
  Vector<DComplex> compFlux(4);
  Vector<Double> shapeParms, spectralParms;
  String compName, compLabel, compFluxPol, compFluxUnit, compSpectrum;
  MDirection compDir;
  MFrequency compFreq;
  for (uInt i = 0; i < nComp; i++) {
    shapeCol.get(i, compName);
    specShapeCol.get(i, compSpectrum);
    currentComp = SkyComponent(ComponentType::shape(compName),
			       ComponentType::spectralShape(compSpectrum));
    {
      fluxValCol.get(i, compFlux);
      currentComp.flux().setValue(compFlux);
      fluxUnitCol.get(i, compFluxUnit); 
      currentComp.flux().setUnit(compFluxUnit);
      fluxPolCol.get(i, compFluxPol); 
      currentComp.flux().setPol(ComponentType::polarisation(compFluxPol));
    }
    {
      dirCol.get(i, compDir);
      ComponentShape & compShape = currentComp.shape();
      compShape.setRefDirection(compDir);
      shapeParms.resize(0);
      shapeParmCol.get(i, shapeParms);
      compShape.setParameters(shapeParms);
    }
    {
      freqCol.get(i, compFreq);
      SpectralModel & compSpectrum = currentComp.spectrum();
      compSpectrum.setRefFrequency(compFreq);
      spectralParms.resize(0);
      specShapeParmCol.get(i, spectralParms);
      compSpectrum.setParameters(spectralParms);
    }
    {
      labelCol.get(i, currentComp.label());
    }
    add(currentComp);
  }
  itsROFlag = readOnly;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentList"
// End: 
