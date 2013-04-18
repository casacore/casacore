//# ComponentList.cc:  this defines the ComponentList implementation
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#include <components/ComponentModels/ComponentList.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/Flux.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScaRecordColDesc.h>
#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/SpectralModel.h>
#include <measures/TableMeasures/ScalarMeasColumn.h>
#include <measures/TableMeasures/ArrayQuantColumn.h>
#include <measures/TableMeasures/ScalarQuantColumn.h>
#include <measures/TableMeasures/TableMeasDesc.h>
#include <measures/TableMeasures/TableMeasRefDesc.h>
#include <measures/TableMeasures/TableMeasValueDesc.h>
#include <measures/TableMeasures/TableQuantumDesc.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Cube.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MCFrequency.h>
#include <casa/Quanta/MVFrequency.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVDirection.h>
#include <measures/Measures/MeasConvert.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/Unit.h>
#include <casa/OS/Path.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ColumnDesc.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ColDescSet.h>
#include <tables/Tables/TableLock.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/TiledCellStMan.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/GenSort.h>
#include <casa/Utilities/Sort.h>
#include <casa/BasicSL/String.h>
#include <casa/Containers/Record.h>

namespace casa { //# NAMESPACE CASA - BEGIN

const String fluxName = "Flux";
const String fluxUnitName = "Flux_Unit";
const String fluxPolName = "Flux_Polarisation";
const String fluxErrName = "Flux_Error";
const String shapeName = "Shape";
const String refDirName = "Reference_Direction";
const String dirErrName = "Direction_Error";
const String dirErrUnitName = "Direction_Error_Units";
const String shapeParName = "Shape_Parameters";
const String shapeErrName = "Shape_Error";
const String spectrumName = "Spectrum_Shape";
const String refFreqName = "Reference_Frequency";
const String freqErrName = "Frequency_Error";
const String spectralRecordName = "Spectral_Record";
const String freqErrUnitName = "Frequency_Error_Units";
const String spectParName = "Spectral_Parameters";
const String spectErrName = "Spectral_Error";
const String labelName = "Label";
const String optParColName = "Optional_Parameters";


ComponentList::ComponentList()
  :itsList(),
   itsNelements(0),
   itsTable(),
   itsROFlag(False),
   itsSelectedFlags(),
   itsOrder(),
   itsAddOptCol(False)
{
  AlwaysAssert(ok(), AipsError);
}

ComponentList::ComponentList(const Path& fileName, const Bool readOnly)
  :itsList(),
   itsNelements(0),
   itsTable(),
   itsROFlag(False),
   itsSelectedFlags(),
   itsOrder(),
   itsAddOptCol(False)
{
  readTable(fileName, readOnly);
  AlwaysAssert(ok(), AipsError);
}

ComponentList::ComponentList(const ComponentList& other)
  :itsList(other.itsList),
   itsNelements(other.itsNelements),
   itsTable(other.itsTable),  
   itsROFlag(other.itsROFlag),
   itsSelectedFlags(other.itsSelectedFlags),
   itsOrder(other.itsOrder),
   itsAddOptCol(other.itsAddOptCol)
{
  DebugAssert(ok(), AipsError);
}

ComponentList::~ComponentList() {
  if ((itsROFlag == False) && (itsTable.isNull() == False)) {
    writeTable();
  }
  AlwaysAssert(ok(), AipsError);
}

ComponentList& ComponentList::operator=(const ComponentList& other){
  if (this != &other) {
    if ((itsROFlag == False) && (itsTable.isNull() == False)) {
      writeTable();
    }
    itsList = other.itsList;
    itsNelements = other.itsNelements;
    itsTable = other.itsTable;
    itsROFlag = other.itsROFlag;
    itsSelectedFlags = other.itsSelectedFlags;
    itsOrder = other.itsOrder;
    itsAddOptCol = other.itsAddOptCol;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

Bool ComponentList::isPhysical(const Vector<Int>& indices) const {
  DebugAssert(ok(), AipsError);
// The static_casts are a workaround for an SGI compiler bug
  DebugAssert(allGE(static_cast<const Vector<Int> &>(indices), 0), AipsError);
  DebugAssert(allLT(static_cast<const Vector<Int> &>(indices), 
		    static_cast<Int>(nelements())), AipsError);
  Bool retVal = True;
  uInt c = indices.nelements();
  while (retVal && c > 0) {
    c--;
    retVal = itsList[indices(c)].isPhysical();
  }
  return retVal;
}

Flux<Double> ComponentList::sample(const MDirection& sampleDir,
				   const MVAngle& pixelLatSize,
				   const MVAngle& pixelLongSize,
				   const MFrequency& centerFreq) const {
  DebugAssert(ok(), AipsError);
  const Unit retUnit("Jy");
  const ComponentType::Polarisation retPol(ComponentType::STOKES);
  Vector<DComplex> result(4, DComplex(0,0));
  Flux<Double> compFlux;
  for (uInt i = 0; i < nelements(); i++) {
    compFlux = component(i).sample(sampleDir, pixelLatSize, pixelLongSize,
				   centerFreq);
    compFlux.convertUnit(retUnit);
    compFlux.convertPol(retPol);
    result += compFlux.value();
  }
  return Flux<Double>(result, retPol);
}

void ComponentList::sample(Cube<Double>& samples,
			   const Unit& reqUnit,
			   const Vector<MVDirection>& directions, 
			   const MeasRef<MDirection>& dirRef, 
			   const MVAngle& pixelLatSize, 
			   const MVAngle& pixelLongSize, 
			   const Vector<MVFrequency>& frequencies,
			   const MeasRef<MFrequency>& freqRef) const {
  samples = 0.0;
  for (uInt i = 0; i < nelements(); i++) {
    component(i).sample(samples, reqUnit, directions, dirRef,
			pixelLatSize, pixelLongSize, frequencies, freqRef);
  }
}

void ComponentList::add(SkyComponent component) {
//  AlwaysAssert(itsROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  uInt blockSize = itsList.nelements();
  if (itsNelements == blockSize) {
    const uInt newSize 
      = (blockSize < 50) ? 2 * blockSize + 1 : blockSize + 100;
    itsList.resize(newSize);
    itsSelectedFlags.resize(newSize);
    itsOrder.resize(newSize);
  }
  // for limb-darkened disk shape, add an optional col 
  if (component.shape().type()==ComponentType::LDISK) {
    itsAddOptCol=True;
  }
  itsList[itsNelements] = component;
  itsSelectedFlags[itsNelements] = False;
  itsOrder[itsNelements] = itsNelements;
  itsNelements++;
}

void ComponentList::addList(const ComponentList& list) {
	for (uInt i=0; i<list.nelements(); i++) {
		add(list.component(i));
	}
}


void ComponentList::remove(const uInt& index) {
//  AlwaysAssert(itsROFlag == False, AipsError);
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

void ComponentList::remove(const Vector<Int>& indices) {
  Vector<Int> zeroCheck(indices);
  AlwaysAssert(allGE(zeroCheck, 0), AipsError);
  uInt c = indices.nelements();
  Vector<uInt> uIndices(c);
  convertArray(uIndices, indices);
  GenSort<uInt>::sort(uIndices);
  while (c != 0) {
    c--;
    remove(uIndices(c));
  }
}

uInt ComponentList::nelements() const {
  return itsNelements;
}

void ComponentList::deselect(const Vector<Int>& indexes) {
  for (uInt i = 0; i < indexes.nelements(); i++) {
    AlwaysAssert(indexes(i) < Int(nelements()), AipsError);
    AlwaysAssert(indexes(i) >= 0, AipsError);
    itsSelectedFlags[itsOrder[indexes(i)]] = False;
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::select(const Vector<Int>& indexes) {
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

void ComponentList::setLabel(const Vector<Int>& which,
			     const String& newLabel) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).label() = newLabel;
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::getFlux(Vector<Quantity>& fluxQuant, const Int& which) const {
   SkyComponent comp = component(which);
   // each element in the returned vector represents a different polarization.
   // NumericTraits::Conjugate is just a confusing way of saying Complex if you
   // look at how comp.flux().value() is implemented.
   Vector<Complex> flux(comp.flux().value().nelements());
   convertArray(flux,comp.flux().value());
   Unit unit = comp.flux().unit();
   fluxQuant.resize(flux.nelements());
   for (uInt i=0; i<flux.nelements(); ++i) {
       fluxQuant[i] = Quantity(real(flux[i]), unit);
   }
}

void ComponentList::getFlux(Vector<Quantum<Complex> >& fluxQuant, const Int& which) {
   SkyComponent comp = component(which);
   Vector<Complex> flux(comp.flux().value().nelements());
   convertArray(flux,comp.flux().value());
   Unit unit = comp.flux().unit();
   fluxQuant.resize(flux.nelements());
   for (uInt i=0; i<flux.nelements(); ++i) {
       fluxQuant[i] = Quantum<Complex>(flux[i], unit);
   }
}

   

void ComponentList::setFlux(const Vector<Int>& which,
			    const Flux<Double>& newFlux) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).flux() = newFlux;
  }
  DebugAssert(ok(), AipsError);
}

Vector<String> ComponentList::getStokes(const Int& which) const {
    SkyComponent comp = component(which);
    ComponentType::Polarisation stokesType = comp.flux().pol();
    Vector<String> polarization(4);
    // the polarization determination logic needs to be refactored into
    // a method in a more appropriate class
    if (stokesType == ComponentType::STOKES) {
        polarization[0] = "I";
        polarization[1] = "Q";
        polarization[2] = "U";
        polarization[3] = "V";
    }
    else if (stokesType == ComponentType::LINEAR) {
        polarization[0] = "XX";
        polarization[1] = "XY";
        polarization[2] = "YX";
        polarization[3] = "YY";
    }
    else if (stokesType == ComponentType::CIRCULAR) {
        polarization[0] = "RR";
        polarization[1] = "RL";
        polarization[2] = "LR";
        polarization[3] = "LL";
    }
    else {
        polarization.set("UNKNOWN");
    }
    return polarization;
}

void ComponentList::convertFluxUnit(const Vector<Int>& which,
				    const Unit& unit) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).flux().convertUnit(unit);
  }
  DebugAssert(ok(), AipsError);
}
  
void ComponentList::convertFluxPol(const Vector<Int>& which,
				   ComponentType::Polarisation pol) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).flux().convertPol(pol);
  }
  DebugAssert(ok(), AipsError);
}
 
void ComponentList::setRefDirection(const Vector<Int>& which,
				    const MVDirection& newDir) {
  uInt c;
  MDirection curDir;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    ComponentShape& curShape = component(c).shape();
    curDir = curShape.refDirection();
    curDir.set(newDir);
    curShape.setRefDirection(curDir);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setRefDirectionFrame(const Vector<Int>& which,
					 MDirection::Types newFrame) {
  uInt c;
  MDirection curDir;
  const MDirection::Ref newRef(newFrame);
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    ComponentShape& curShape = component(c).shape();
    curDir = curShape.refDirection();
    curDir.set(newRef);
    curShape.setRefDirection(curDir);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::convertRefDirection(const Vector<Int>& which,
					MDirection::Types newFrame) {
  uInt c;
  MDirection::Convert converter;
  converter.setOut(newFrame);
  MDirection curDir;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    ComponentShape& curShape = component(c).shape();
    curDir = curShape.refDirection();
    curShape.setRefDirection(converter(curDir));
  }
  DebugAssert(ok(), AipsError);
}

MDirection ComponentList::getRefDirection(Int which) const {
    const ComponentShape& compShape = component(which).shape();
    MDirection refDir = compShape.refDirection();
    return refDir;
}

void ComponentList::setShape(const Vector<Int>& which,
			     const ComponentShape& newShape) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).setShape(newShape);
    //for limb-darkened disk shape
    if (newShape.type()==ComponentType::LDISK) {
      itsAddOptCol=True;
    }
  }
  DebugAssert(ok(), AipsError);
}

const ComponentShape* ComponentList::getShape(Int which) const {
    return component(which).shape().getPtr();
}


void ComponentList::setShapeParms(const Vector<Int>& which,
				  const ComponentShape& newShape) {
  uInt c;
  MDirection oldDir;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SkyComponent& comp = component(c);
    oldDir = comp.shape().refDirection();
    component(c).setShape(newShape);
    comp.shape().setRefDirection(oldDir);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setOptParms(const Vector<Int>& which,
                                const ComponentShape& newShape) {
  uInt c;
  Vector<Double> optparms;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SkyComponent& comp = component(c);
    component(c).setShape(newShape);
    if (comp.shape().type()==ComponentType::LDISK) {
      optparms = comp.shape().optParameters(); 
      //comp.shape().setOptParameters(optparms);
      component(c).optionalParameters()=optparms;
      itsAddOptCol=True;
    }
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setSpectrum(const Vector<Int>& which,
				const SpectralModel& newSpectrum) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).setSpectrum(newSpectrum);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setSpectrumParms(const Vector<Int>& which,
				     const SpectralModel& newSpectrum) {
  uInt c;
  MFrequency oldFreq;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SkyComponent& comp = component(c);
    oldFreq = comp.spectrum().refFrequency();
    component(c).setSpectrum(newSpectrum);
    comp.spectrum().setRefFrequency(oldFreq);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setRefFrequency(const Vector<Int>& which, 
				    const MVFrequency& newFreq) {
  uInt c;
  MFrequency curFreq;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SpectralModel& curSpectrum = component(c).spectrum();
    curFreq = curSpectrum.refFrequency();
    curFreq.set(newFreq);
    curSpectrum.setRefFrequency(curFreq);
  }
  DebugAssert(ok(), AipsError);
}

void ComponentList::setRefFrequencyFrame(const Vector<Int>& which,
					 MFrequency::Types newFrame) {
  uInt c;
  MFrequency curFreq;
  const MFrequency::Ref newRef(newFrame);
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    SpectralModel& curSpectrum = component(c).spectrum();
    curFreq = curSpectrum.refFrequency();
    curFreq.set(newRef);
    curSpectrum.setRefFrequency(curFreq);
  }
  DebugAssert(ok(), AipsError);
}


void ComponentList::setRefFrequencyUnit(const Vector<Int>& which,
					const Unit& unit) {
  uInt c;
  for (uInt i = 0; i < which.nelements(); i++) {
    AlwaysAssert(which(i) >= 0, AipsError);
    c = which(i);
    component(c).spectrum().convertFrequencyUnit(unit);
  }
  DebugAssert(ok(), AipsError);
}

SkyComponent& ComponentList::component(const uInt& index) {
//  AlwaysAssert(itsROFlag == False, AipsError);
  AlwaysAssert(index < nelements(), AipsError);
  DebugAssert(ok(), AipsError);
  return itsList[itsOrder[index]];
}

const SkyComponent& ComponentList::component(const uInt& index) const {
  DebugAssert(ok(), AipsError);
  AlwaysAssert(index < nelements(), AipsError);
  return itsList[itsOrder[index]];
}

void ComponentList::rename(const Path& fileName, 
			   const Table::TableOption option) {
  AlwaysAssert(option != Table::Old, AipsError);
  AlwaysAssert(itsROFlag == False, AipsError);
  DebugAssert(ok(), AipsError);
  if (fileName.length() != 0) {
    // See if this list is associated with a Table. 
    if (itsTable.isNull()) {
      //createTable(fileName, option, addOptCol_p);
      createTable(fileName, option);
    } else {
      if (!itsTable.isWritable()) itsTable.reopenRW();
      itsTable.rename(fileName.absoluteName(), option);
    }

    // Ensure that the Table::isReadable(fileName) returns True, otherwise the
    // ok() function will fail.
    itsTable.flush();
    DebugAssert(ok(), AipsError);
  } else {
    if (!itsTable.isNull()) {
      itsTable.markForDelete();
      itsTable = Table();
      itsROFlag = False;
    }
  }
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
      val[i] = abs(itsList[i].flux().value(0u).real());
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
  case ComponentList::POLARISATION: return "Polarization";
  default: return "unknown";
  };
}

ComponentList::SortCriteria ComponentList::type(const String& criteria) {
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

void ComponentList::createTable(const Path& fileName,
				const Table::TableOption option) {
	//			const Table::TableOption option,
                    //            const Bool addOptCol) {
  // Build a default table description
  TableDesc td("ComponentListDescription", "4", TableDesc::Scratch);  
  td.comment() = "A description of a component list";
  {
    {
      const ArrayColumnDesc<DComplex> 
	fluxValCol(fluxName, "Flux values", IPosition(1,4),ColumnDesc::Direct);
      td.addColumn(fluxValCol);
      const ScalarColumnDesc<String>
	fluxUnitCol(fluxUnitName, "Flux units", ColumnDesc::Direct);
      td.addColumn(fluxUnitCol);
      const ScalarColumnDesc<String> 
	fluxPolCol(fluxPolName, "Flux polarisation representation", 
		   ColumnDesc::Direct);
      td.addColumn(fluxPolCol);
      const ArrayColumnDesc<DComplex> 
 	fluxErrCol(fluxErrName, "Flux errors", IPosition(1,4), 
 		   ColumnDesc::Direct);
      td.addColumn(fluxErrCol);
    }
    {
      const ScalarColumnDesc<String> 
	shapeCol(shapeName, "Shape of the Component", ColumnDesc::Direct);
      td.addColumn(shapeCol);
      const String dirValColName = refDirName;
      const ArrayColumnDesc<Double> 
	dirValCol(dirValColName, "Reference direction values",
		  IPosition(1,2), ColumnDesc::Direct);
      td.addColumn(dirValCol);
      const String dirRefColName = "Direction_Frame";
      const ScalarColumnDesc<String>
	dirRefCol(dirRefColName, "The reference direction frame", 
		  ColumnDesc::Direct);
      td.addColumn(dirRefCol);
      {
	const TableMeasRefDesc dirRefTMCol(td, dirRefColName);
	const TableMeasValueDesc dirValTMCol(td, dirValColName);
	TableMeasDesc<MDirection> dirTMCol(dirValTMCol, dirRefTMCol);
	dirTMCol.write(td);
      }
      const ArrayColumnDesc<Double>
 	dirErrCol(dirErrName, "Error in the reference direction values",
 		  IPosition(1,2), ColumnDesc::Direct);
      td.addColumn(dirErrCol);
      const ArrayColumnDesc<String>
	dirErrUnitCol(dirErrUnitName, "Units of the direction error", 
		      IPosition(1,2), ColumnDesc::Direct);
      td.addColumn(dirErrUnitCol);
      {
	TableQuantumDesc dirErrTMCol(td, dirErrName, dirErrUnitName);
	dirErrTMCol.write(td);
      }

      const ArrayColumnDesc<Double> 
	shapeParmCol(shapeParName,
		     "Parameters specific to the component shape", 1);
      td.addColumn(shapeParmCol);
      const ArrayColumnDesc<Double> 
	shapeErrCol(shapeErrName,
		     "Error in the shape parameters", 1);
      td.addColumn(shapeErrCol);
    }
    {
      const ScalarColumnDesc<String>
	freqShapeCol(spectrumName, "Shape of the spectrum", 
		     ColumnDesc::Direct);
      td.addColumn (freqShapeCol);
      const String freqValColName = refFreqName;
      const ScalarColumnDesc<Double>
	freqValCol(freqValColName, "The reference frequency values", 
		   ColumnDesc::Direct);
      td.addColumn(freqValCol);
      const String freqRefColName = "Frequency_Frame";
      const ScalarColumnDesc<String>
	freqRefCol(freqRefColName, "The reference frequency frame", 
		   ColumnDesc::Direct);
      td.addColumn(freqRefCol);
      {
	const TableMeasRefDesc freqRefTMCol(td, freqRefColName);
	const TableMeasValueDesc freqValTMCol(td, freqValColName);
	TableMeasDesc<MFrequency> freqTMCol(freqValTMCol, freqRefTMCol);
	freqTMCol.write(td);
      }
      const ScalarColumnDesc<Double> 
 	freqErrCol(freqErrName, "Error in the reference frequency",
		   ColumnDesc::Direct);
      td.addColumn(freqErrCol);
      const ScalarColumnDesc<String>
	freqErrUnitCol(freqErrUnitName, "Units of the frequency error", 
		       ColumnDesc::Direct);
      td.addColumn(freqErrUnitCol);
      {
	TableQuantumDesc freqErrTMCol(td, freqErrName, freqErrUnitName);
	freqErrTMCol.write(td);
      }
      const ArrayColumnDesc<Double> 
	specParmCol(spectParName, 
		    "Parameters specific to the components spectrum", 1);
      td.addColumn(specParmCol);
      const ArrayColumnDesc<Double> 
 	specErrCol(spectErrName, 
		   "Errors in the spectral parameters", 1);
      td.addColumn(specErrCol);
    }
    {
      const ScalarColumnDesc<String> 
	labelCol(labelName, "An arbitrary label for the user",
		 ColumnDesc::Direct);
      td.addColumn (labelCol);
    }
  }
  if (itsAddOptCol) {
    const ArrayColumnDesc<Double>
      optParCol(optParColName,"optional parameter column",1,ColumnDesc::Undefined);
    td.addColumn (optParCol);
    td.defineHypercolumn("TiledOptParms",1,stringToVector(optParColName));
  } 
  SetupNewTable newTable(fileName.absoluteName(), td, option);

  if (itsAddOptCol) {
    TiledCellStMan optcolsm("TiledOptParms",IPosition(1,1));
    newTable.bindColumn(optParColName,optcolsm);
  }
  itsTable = Table(newTable, TableLock::AutoLocking, nelements(), False);
  {
    TableInfo& info(itsTable.tableInfo());
    info.setType(TableInfo::type(TableInfo::COMPONENTLIST));
    info.readmeAddLine(String(
    "This is a ComponentList Table containing parameterised representations"));
    info.readmeAddLine(String("of the sky brightness."));
  }
}

void ComponentList::writeTable() {
  if (itsTable.isWritable() == False) {
    itsTable.reopenRW();
  }
  DebugAssert(itsTable.isWritable(), AipsError);
  {
    const uInt nRows = itsTable.nrow();
    const uInt nelem = nelements();
    if (nRows < nelem) {
      itsTable.addRow(nelem - nRows);
    } else if (nRows > nelem) {
      Vector<uInt> rows(nRows - nelem);
      indgen(rows, nelem);
      itsTable.removeRow(rows);
    }
  }
  ArrayColumn<DComplex> fluxValCol(itsTable, fluxName);
  ScalarColumn<String> fluxUnitCol(itsTable, fluxUnitName);
  ScalarColumn<String> fluxPolCol(itsTable, fluxPolName);
  ScalarColumn<String> shapeCol(itsTable, shapeName);
  MDirection::ScalarColumn dirCol(itsTable, refDirName);
  ArrayColumn<Double> shapeParmCol(itsTable, shapeParName);
  ScalarColumn<String> specShapeCol(itsTable, spectrumName);
  MFrequency::ScalarColumn freqCol(itsTable, refFreqName);
  ArrayColumn<Double> specShapeParmCol(itsTable, spectParName); 
  ScalarColumn<String> labelCol(itsTable, labelName);
  ArrayColumn<DComplex> fluxErrCol;
  ArrayQuantColumn<Double> dirErrCol;
  ArrayColumn<Double> shapeErrCol;
  ScalarQuantColumn<Double> freqErrCol;
  ArrayColumn<Double> spectErrCol;
  ScalarColumn<TableRecord> specRecord;
  ArrayColumn<Double> optParCol;
  {
    const ColumnDescSet& cds=itsTable.tableDesc().columnDescSet();
    if (!cds.isDefined(spectralRecordName)) {
      itsTable.addColumn(ScalarRecordColumnDesc(spectralRecordName));
    }
    specRecord.attach(itsTable, spectralRecordName);
    if (!cds.isDefined(fluxErrName)) {
      itsTable.addColumn
	(ArrayColumnDesc<DComplex>(fluxErrName, "Flux errors", IPosition(1,4), 
				   ColumnDesc::Direct));
    }
    
    fluxErrCol.attach(itsTable, fluxErrName);
    if (!cds.isDefined(dirErrName)) {
      itsTable.addColumn
	(ArrayColumnDesc<Double>(dirErrName,
				 "Error in the reference direction values",
				 IPosition(1,2), ColumnDesc::Direct));
      itsTable.addColumn
	(ArrayColumnDesc<String>(dirErrUnitName,
				 "Units of the direction error",
				 IPosition(1,2), ColumnDesc::Direct));
      TableQuantumDesc dirErrTMCol(itsTable.tableDesc(), dirErrName,
				   dirErrUnitName);
      dirErrTMCol.write(itsTable);
    }
    dirErrCol.attach(itsTable, dirErrName);
    if (!cds.isDefined(shapeErrName)) {
      itsTable.addColumn
	(ArrayColumnDesc<Double>(shapeErrName,
				 "Error in the shape parameters", 1));
    }
    shapeErrCol.attach(itsTable, shapeErrName);

    if (!cds.isDefined(freqErrName)) {
      itsTable.addColumn
	(ScalarColumnDesc<Double>(freqErrName, 
				  "Error in the reference frequency",
				  ColumnDesc::Direct));
      itsTable.addColumn
	(ScalarColumnDesc<String>(freqErrUnitName,
				  "Units of the frequency error", 
				  ColumnDesc::Direct));
      TableQuantumDesc freqErrTMCol(itsTable.tableDesc(), freqErrName,
				    freqErrUnitName);
      freqErrTMCol.write(itsTable);
    }
    freqErrCol.attach(itsTable, freqErrName);
    if (!cds.isDefined(spectErrName)) {
      itsTable.addColumn
	(ArrayColumnDesc<Double>(spectErrName,
				 "Error in the spectral parameters", 1));
    }
    spectErrCol.attach(itsTable, spectErrName);

    if (itsAddOptCol) {
      if (!cds.isDefined(optParColName)) {
        itsTable.addColumn
          (ArrayColumnDesc<Double>(optParColName,
                                 "Optional parameters", 1));
        //cerr<<"added optional parameter col"<<endl;
      }
      optParCol.attach(itsTable, optParColName); 
    }
  }
  
  Vector<Quantum<Double> > dirErr(2);
  for (uInt i = 0; i < nelements(); i++) {
    {
      const Flux<Double>& flux = component(i).flux();
      fluxValCol.put(i, flux.value());
      fluxUnitCol.put(i, flux.unit().getName());
      fluxPolCol.put(i, ComponentType::name(flux.pol()));
      fluxErrCol.put(i, flux.errors());
    }
    {
      const ComponentShape& compShape = component(i).shape();
      shapeCol.put(i, compShape.ident());
      dirCol.put(i, compShape.refDirection());
      if (!dirErrCol.isNull()) {
	dirErr(0) = compShape.refDirectionErrorLat();
	dirErr(1) = compShape.refDirectionErrorLong();
	dirErrCol.put(i, dirErr);
      }
      shapeParmCol.put(i, compShape.parameters());
      shapeErrCol.put(i, compShape.errors());
    }
    {
      const SpectralModel& compSpectrum = component(i).spectrum();
      specShapeCol.put(i, compSpectrum.ident());
      TableRecord rec;
      String err;
      if(compSpectrum.toRecord(err, rec))
	specRecord.put(i,rec);
      freqCol.put(i, compSpectrum.refFrequency());
      if (!freqErrCol.isNull()) {
	freqErrCol.put(i, compSpectrum.refFrequencyError());
      }
      specShapeParmCol.put(i, compSpectrum.parameters());
      spectErrCol.put(i, compSpectrum.errors());
    }
    {
      labelCol.put(i, component(i).label());
    }
    if (itsAddOptCol) {
      const ComponentShape& compShape2 = component(i).shape();
      if (compShape2.type()==ComponentType::LDISK) { 
        //optParCol.put(i,compShape2.optParameters());
        optParCol.put(i,component(i).optionalParameters());
      }
    }
  }
}

void ComponentList::readTable(const Path& fileName, const Bool readOnly) {
  {
    const String& fullName = fileName.absoluteName();
    if (readOnly) {
      AlwaysAssert(Table::isReadable(fullName), AipsError);
      itsTable = Table(fullName, Table::Old);
    }
    else {
      AlwaysAssert(Table::isWritable(fullName), AipsError);
      itsTable = Table(fullName, TableLock(TableLock::AutoLocking),
		       Table::Update);
    }
  }
  const ROArrayColumn<DComplex> fluxValCol(itsTable, fluxName);
  const ROScalarColumn<String> fluxUnitCol(itsTable, fluxUnitName);
  const ROScalarColumn<String> fluxPolCol(itsTable, fluxPolName);
  const ROScalarColumn<String> shapeCol(itsTable, shapeName);
  const MDirection::ROScalarColumn dirCol(itsTable, refDirName);
  const ROArrayColumn<Double> shapeParmCol(itsTable, shapeParName);
  const ROScalarColumn<String> specShapeCol(itsTable, spectrumName);
  const MFrequency::ROScalarColumn freqCol(itsTable, refFreqName);
  const ROArrayColumn<Double> spectralParmCol(itsTable,spectParName); 
  const ROScalarColumn<String> labelCol(itsTable, labelName);

  ROArrayColumn<DComplex> fluxErrCol;
  ROArrayQuantColumn<Double> dirErrCol;
  ROArrayColumn<Double> shapeErrCol;
  ROScalarQuantColumn<Double> freqErrCol;
  ROArrayColumn<Double> spectralErrCol;
  ROScalarColumn<TableRecord> specRecord;
  ROArrayColumn<Double> optParmCol;
  {// Old componentlist tables may not have the error columns
    const ColumnDescSet& cds=itsTable.tableDesc().columnDescSet();
    if (cds.isDefined(fluxErrName)) {
      fluxErrCol.attach(itsTable, fluxErrName);
    }
    if (cds.isDefined(dirErrName)) {
      dirErrCol.attach(itsTable, dirErrName);
    }
    if (cds.isDefined(shapeErrName)) {
      shapeErrCol.attach(itsTable, shapeErrName);
    }
    if (cds.isDefined(freqErrName)) {
      freqErrCol.attach(itsTable, freqErrName);
    }
    if (cds.isDefined(spectErrName)) {
      spectralErrCol.attach(itsTable, spectErrName);
    }
    if (cds.isDefined(spectralRecordName)) {
      specRecord.attach(itsTable, spectralRecordName);
    }
    // new optional parameter column
    if (cds.isDefined(optParColName)) {
      optParmCol.attach(itsTable,optParColName);
    }
  }

  SkyComponent currentComp;
  const uInt nComp = fluxValCol.nrow();
  Vector<DComplex> compFluxValue(4);
  Vector<Double> shapeParms, spectralParms, optParms;
  String compName, compLabel, compFluxPol, compFluxUnit, compSpectrum;
  MDirection compDir;
  MFrequency compFreq;
  Vector<Quantum<Double> > newDirErr(2);
  Quantum<Double> newFreqErr;
  for (uInt i = 0; i < nComp; i++) {
    shapeCol.get(i, compName);
    specShapeCol.get(i, compSpectrum);
    //cout << "CompSpec " << compSpectrum << " shape " <<ComponentType::spectralShape(compSpectrum) <<  endl;
    currentComp = SkyComponent(ComponentType::shape(compName),
 			       ComponentType::spectralShape(compSpectrum));
    {
      Flux<Double>& compFlux = currentComp.flux();
      fluxValCol.get(i, compFluxValue);
      compFlux.setValue(compFluxValue);
      fluxUnitCol.get(i, compFluxUnit); 
      compFlux.setUnit(compFluxUnit);
      fluxPolCol.get(i, compFluxPol); 
      compFlux.setPol(ComponentType::polarisation(compFluxPol));
      if (!fluxErrCol.isNull()) {
	fluxErrCol.get(i, compFluxValue);
	compFlux.setErrors(compFluxValue(0), compFluxValue(1),
			   compFluxValue(2), compFluxValue(3));
      }
    }
    {
      ComponentShape& compShape = currentComp.shape();
      dirCol.get(i, compDir);
      compShape.setRefDirection(compDir);
      if (!dirErrCol.isNull()) {
	dirErrCol.get(i, newDirErr);
	compShape.setRefDirectionError(newDirErr(0), newDirErr(1));
      }
      shapeParmCol.get(i, shapeParms, True);
      compShape.setParameters(shapeParms);
      if (!shapeErrCol.isNull()) {
	shapeErrCol.get(i, shapeParms);
	compShape.setErrors(shapeParms);
      }
    }
    {
      freqCol.get(i, compFreq);
      SpectralModel& compSpectrum = currentComp.spectrum();
      compSpectrum.setRefFrequency(compFreq);
      if (!specRecord.isNull()) {
	if(specRecord.isDefined(i)){
	  TableRecord rec;
	  specRecord.get(i, rec);
	  String err;
	  compSpectrum.fromRecord(err, rec);
	}
      }
      if (!freqErrCol.isNull()) {
	freqErrCol.get(i, newFreqErr);
	compSpectrum.setRefFrequencyError(newFreqErr);
      }
      spectralParmCol.get(i, spectralParms, True);
      compSpectrum.setParameters(spectralParms);
      if (!spectralErrCol.isNull()) {
	spectralErrCol.get(i, spectralParms);
	compSpectrum.setErrors(spectralParms);
      }
    }
    {
      labelCol.get(i, currentComp.label());
    }
    {
      if (!optParmCol.isNull()) {
        if (optParmCol.isDefined(i)) {
          ComponentShape& compShape2 = currentComp.shape();
          optParmCol.get(i,optParms,True);
          compShape2.setOptParameters(optParms); 
          currentComp.optionalParameters()=optParms;
        }
      }
    }
  
    add(currentComp);
  }
  itsROFlag = readOnly;
}

Bool ComponentList::toRecord(String& error, RecordInterface& outRec) const {

  Bool retval=True;

  outRec.define("nelements", itsNelements);
  for (uInt k=0; k < itsNelements; ++k){ 
    Record componentContainer;
    retval= (retval && component(k).toRecord(error, componentContainer));
    String componentId=String("component")+String::toString(k);
    outRec.defineRecord(componentId, componentContainer);

  }
  return retval;

}

Bool ComponentList::fromRecord(String& error, const RecordInterface& inRec){

	Bool retval= True;
	if(itsNelements > 0){
		LogIO logErr(LogOrigin("ComponentList", "fromRecord()"));
		logErr << LogIO::SEVERE
		<< "Trying to overwrite a non-empty componentList  from Record"
		<< LogIO::POST;
		return False;

	}

	uInt nelements=0;
	if (inRec.isDefined("nelements")) {
		inRec.get("nelements", nelements);
		if(nelements >0){
			for(uInt k=0; k < nelements; ++k){
				String componentId=String("component")+String::toString(k);
				Record componentRecord=inRec.asRecord(componentId);
				SkyComponent  tempComponent;
				retval=(retval && tempComponent.fromRecord(error, componentRecord));
				if(retval){
					add(tempComponent);
				}
				else{
					return retval;
				}
			}
		}
	}
	return retval;
}

String ComponentList::summarize(uInt index) const {
	  AlwaysAssert(index < nelements(), AipsError);
	  DebugAssert(ok(), AipsError);
	  return component(index).summarize();
}


// Local Variables: 
// compile-command: "gmake ComponentList"
// End: 

} //# NAMESPACE CASA - END

