//# CoordinateUtil.cc: 
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003,2004
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

#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/Projection.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/sstream.h>

namespace casacore { // begin namespace casa


void CoordinateUtil::addDirAxes(CoordinateSystem & coords){
  Matrix<Double> xform(2, 2); xform = 0.0; xform.diagonal() = 1.0;
  DirectionCoordinate dirAxes(MDirection::J2000, 
			       Projection(Projection::SIN),
			       0.0, 0.0, // Ref is at RA = 0, Dec = 0
			       1.0, 1.0, // The increment is overwritten below
			       xform,    // Rotation matrix
			       0.0, 0.0, // Ref pixel is 0,0
			       999.0, 999.0);
  // reset the increment to 1 minute of arc on both axes
  Vector<String> units(2); units = String("'"); 
  Vector<Double> inc(2); inc(0) = -1.0; inc(1) = 1.0;
  dirAxes.setWorldAxisUnits(units);
  AlwaysAssert(dirAxes.setIncrement(inc) == True, AipsError);
  // Add the direction coordinates to the system. 
  coords.addCoordinate(dirAxes);
}
void CoordinateUtil::addIQUVAxis(CoordinateSystem & coords){
  Vector<Int> pols(4);
  pols(0) = Stokes::I;
  pols(1) = Stokes::Q;
  pols(2) = Stokes::U;
  pols(3) = Stokes::V;
  StokesCoordinate polAxis(pols);
  // Add the stokes coordinates to the system. 
  coords.addCoordinate(polAxis);
}
void CoordinateUtil::addIAxis(CoordinateSystem & coords){
  Vector<Int> pols(1);
  pols(0) = Stokes::I;
  StokesCoordinate polAxis(pols);
  // Add the stokes coordinates to the system. 
  coords.addCoordinate(polAxis);
}

Bool CoordinateUtil::addStokesAxis(CoordinateSystem & coords,
                                   uInt shape)
{
   if (shape<1 || shape>4) return False;
//
   Vector<Int> which;
   if (shape==1) {
      which.resize(1);
      which(0) = Stokes::I;
   } else if (shape==2) {
      which.resize(2);
      which(0) = Stokes::I;
      which(1) = Stokes::Q;
   } else if (shape==3) {
      which.resize(3);
      which(0) = Stokes::I;
      which(1) = Stokes::Q;
      which(2) = Stokes::U;
   } else if (shape==4) {
      which.resize(4);
      which(0) = Stokes::I;
      which(1) = Stokes::Q;
      which(2) = Stokes::U;
      which(3) = Stokes::V;
   }
   StokesCoordinate sc(which);
   coords.addCoordinate(sc);
   return True;
}


void CoordinateUtil::addFreqAxis(CoordinateSystem & coords)
{
  SpectralCoordinate freqAxis(MFrequency::LSRK,               // Local standard of rest
			      1415E6,                         // ref. freq. = 1415MHz
			      1E3,                            // 1 kHz bandwidth/channel
			      0.0,                            // channel 0 is the ref.
                              QC::HI.getValue(Unit("Hz")));   // HI
  coords.addCoordinate(freqAxis);
}

void CoordinateUtil::addLinearAxes(CoordinateSystem & coords, 
                                   const Vector<String>& names,
                                   const IPosition& shape)
{
    const uInt n = names.nelements();
//
    Vector<String> units(n);
    Vector<Double> refVal(n);
    Vector<Double> refPix(n);
    Vector<Double> inc(n);
//     
    for (uInt i=0; i<n; i++) {
       refVal(i) = 0.0;
       inc(i) = 1.0; 
       if (shape.nelements()==n) {
          refPix(i) = Double(Int((shape(i) + 1)/2));
       } else {
          refPix(i) = 0.0;
       }
       units(i) = String("km");
    } 
    Matrix<Double> pc(n, n);
    pc = 0.0; 
    pc.diagonal() = 1.0;
//
    LinearCoordinate lc(names, units, refVal, inc, pc, refPix);
    coords.addCoordinate(lc);
}

CoordinateSystem CoordinateUtil::defaultCoords2D(){
  CoordinateSystem coords;
  CoordinateUtil::addDirAxes(coords);
  return coords;
}
CoordinateSystem CoordinateUtil::defaultCoords3D(){
  CoordinateSystem coords;
  CoordinateUtil::addDirAxes(coords);
  CoordinateUtil::addFreqAxis(coords);
  return coords;
}
CoordinateSystem CoordinateUtil::defaultCoords4D(){
  CoordinateSystem coords;
  CoordinateUtil::addDirAxes(coords);
  CoordinateUtil::addIQUVAxis(coords);
  CoordinateUtil::addFreqAxis(coords);
  return coords;
}

CoordinateSystem CoordinateUtil::defaultCoords(uInt dims){
  switch (dims){
  case 2:
    return CoordinateUtil::defaultCoords2D();
  case 3:
    return CoordinateUtil::defaultCoords3D();
  case 4:
    return CoordinateUtil::defaultCoords4D();
  default:
    throw(AipsError("defaultCoords() - cannot create cordinates except "
		    "for a 2, 3 or 4-dimensional image"));
    // The following line is just to suppress a compiler warning that this
    // function does not always return a CoordinateSystem. It is never
    // executed.
    return CoordinateUtil::defaultCoords2D();
  }
}

uInt CoordinateUtil::addAxes (
	CoordinateSystem& csys,
	Bool direction,
	Bool spectral, const String& stokes,
	Bool linear, Bool tabular,
	Bool silent
) {
	uInt nExtra = 0;
	if (direction) {
		if (! csys.hasDirectionCoordinate()) {
			addDirAxes(csys);
			nExtra += 2;
		}
		else if(!silent){
			throw AipsError("Image already contains a DirectionCoordinate");
		}
	}
	if (spectral) {
		if (! csys.hasSpectralAxis()) {
			addFreqAxis(csys);
			nExtra++;
		}
		else if(!silent){
			throw AipsError("Image already contains a SpectralCoordinate");
		}
	}
	if (! stokes.empty()) {
		if (! csys.hasPolarizationCoordinate()) {
			Vector<Int> which(1);
			String tmp = upcase(stokes);
			which(0) = Stokes::type(tmp);
			StokesCoordinate sc(which);
			csys.addCoordinate(sc);
			nExtra++;
		}
		else if(!silent){
			throw AipsError("Image already contains a StokesCoordinate");
		}
	}
	if (linear) {
		if (! csys.hasLinearCoordinate()) {
			Vector<String> names(1);
			Vector<String> units(1);
			Vector<Double> refVal(1);
			Vector<Double> refPix(1);
			Vector<Double> incr(1);
			names(0) = "Axis1";
			units(0) = "km";
			refVal(0) = 0.0;
			refPix(0) = 0.0;
			incr(0) = 1.0;
			Matrix<Double> pc(1,1);
			pc.set(0.0);
			pc.diagonal() = 1.0;
			LinearCoordinate lc(names, units, refVal, incr, pc, refPix);
			csys.addCoordinate(lc);
			nExtra++;
		}
		else if(!silent){
			throw AipsError("Image already contains a LinearCoordinate");
		}
	}
	if (tabular) {
		Int afterCoord = -1;
		Int iC = csys.findCoordinate(Coordinate::TABULAR, afterCoord);
		if (iC<0) {
			TabularCoordinate tc;
			csys.addCoordinate(tc);
			nExtra++;
		}
		else if(!silent){
			throw AipsError("Image already contains a TabularCoordinate");
		}
	}
	ThrowIf(
		nExtra == 0 && ! silent,
		"No degenerate axes specified"
	);
	return nExtra;
}

Int CoordinateUtil::findSpectralAxis(const CoordinateSystem & coords) 
{
  const Int coordinate = coords.findCoordinate(Coordinate::SPECTRAL);
  if (coordinate < 0) return coordinate;
//
  AlwaysAssert(coords.findCoordinate(Coordinate::SPECTRAL, coordinate)
	       == -1, AipsError);
  const Vector<Int> pixelAxes = coords.pixelAxes(coordinate);
  AlwaysAssert(pixelAxes.nelements() == 1, AipsError);
  return pixelAxes(0);
}

void CoordinateUtil::findSpectralAxis(Int& pixelAxis, Int& worldAxis, 
                                      Int& coordinate, const CoordinateSystem & coords)
{
  pixelAxis = -1;
  worldAxis = -1;
  coordinate = coords.findCoordinate(Coordinate::SPECTRAL);
  if (coordinate < 0) return;
//
  AlwaysAssert(coords.findCoordinate(Coordinate::SPECTRAL, coordinate)
               == -1, AipsError);
//
  const Vector<Int> pixelAxes = coords.pixelAxes(coordinate);
  AlwaysAssert(pixelAxes.nelements() == 1, AipsError);
  pixelAxis = pixelAxes(0);
//
  const Vector<Int> worldAxes = coords.worldAxes(coordinate);
  AlwaysAssert(worldAxes.nelements() == 1, AipsError);
  worldAxis = worldAxes(0);
//
  return;  
}



Vector<Int> CoordinateUtil::findDirectionAxes(const CoordinateSystem & coords)
{
  const Int coordinate = coords.findCoordinate(Coordinate::DIRECTION);
  Vector<Int> retVal;
  if (coordinate < 0)  return retVal;
//
  AlwaysAssert(coords.findCoordinate(Coordinate::DIRECTION, coordinate)
	       == -1, AipsError);
  retVal = coords.pixelAxes(coordinate);
  return retVal;
}


void CoordinateUtil::findDirectionAxes(Vector<Int>& pixelAxes,
                                       Vector<Int>& worldAxes,
                                       Int& coordinate,
                                       const CoordinateSystem & coords) 
{
  pixelAxes.resize(0);
  worldAxes.resize(0);
  coordinate = coords.findCoordinate(Coordinate::DIRECTION);
  if (coordinate < 0) return;
//
  AlwaysAssert(coords.findCoordinate(Coordinate::DIRECTION, coordinate)
	       == -1, AipsError);
//
  pixelAxes = coords.pixelAxes(coordinate);
  AlwaysAssert(pixelAxes.nelements() == 2, AipsError);
//
  worldAxes = coords.worldAxes(coordinate);
  AlwaysAssert(worldAxes.nelements() == 2, AipsError);
//
   return;
}


Int CoordinateUtil::findStokesAxis(Vector<Stokes::StokesTypes>& whichPols, 
                                   const CoordinateSystem& coords)
{
  const Int coordinate = coords.findCoordinate(Coordinate::STOKES);
  if (coordinate < 0) {
    whichPols.resize(1);
    whichPols(0) = Stokes::I;
    return coordinate;
  }
  AlwaysAssert(coords.findCoordinate(Coordinate::STOKES, coordinate) == -1, 
	       AipsError);
  const Vector<Int> pixelAxes = coords.pixelAxes(coordinate);
  AlwaysAssert(pixelAxes.nelements() == 1, AipsError);
  const StokesCoordinate& polCoord = coords.stokesCoordinate(coordinate);
  const Vector<Int> polsAsInts = polCoord.stokes();
  const uInt nStokes = polsAsInts.nelements();
  whichPols.resize(nStokes);
  for (uInt i = 0; i < nStokes; i++) {
    whichPols(i) = (Stokes::StokesTypes) polsAsInts(i);
  }
  return pixelAxes(0);
}


void CoordinateUtil::findStokesAxis(Int& pixelAxis, Int& worldAxis, 
                                    Int& coordinate, const CoordinateSystem & coords)
{
  pixelAxis = -1;
  worldAxis = -1;
  coordinate = coords.findCoordinate(Coordinate::STOKES);
  if (coordinate < 0) return;
//
  AlwaysAssert(coords.findCoordinate(Coordinate::STOKES, coordinate)
               == -1, AipsError);
//
  const Vector<Int> pixelAxes = coords.pixelAxes(coordinate);
  AlwaysAssert(pixelAxes.nelements() == 1, AipsError);
  pixelAxis = pixelAxes(0);
//
  const Vector<Int> worldAxes = coords.worldAxes(coordinate);
  AlwaysAssert(worldAxes.nelements() == 1, AipsError);
  worldAxis = worldAxes(0);
//
  return;  
}



Bool CoordinateUtil::removeAxes(CoordinateSystem& csys,
                                Vector<Double>& worldReplacement,
                                const Vector<Int>& worldAxes,
                                const Bool removeThem)
//
// Remove all the world axes and associated pixel axes
// derived from the given list (a list to keep or remove)
// of world axes.
// This is awkward because as soon as you remove an 
// axis, they all shuffle down one !  The replacement values
// are optional.  If these vectors are the wrong length,
// (e.g. 0), the reference pixels/values are used.  The used
// values are returned.
//
{
// Bug out if nothing to do

   if (worldAxes.nelements() == 0) return True;

// Make sure the world axes are valid

   uInt i,j;
   for (i=0; i<worldAxes.nelements(); i++) {
      if (worldAxes(i) >= Int(csys.nWorldAxes())) return False;
   }

// Make a list of the axes to remove in ascending order
// with no duplicates

   Vector<Int> remove(csys.nWorldAxes());
   if (removeThem) {
      remove.resize(worldAxes.nelements());
      remove = worldAxes;
      GenSort<Int>::sort(remove, Sort::Ascending, Sort::NoDuplicates);
   } else {
      for (i=0,j=0; i<csys.nWorldAxes(); i++) {
         if (!anyEQ(worldAxes, Int(i))) remove(j++) = i;
      }
      remove.resize(j,True);
   }
   const uInt nRemove = remove.nelements();
   if (nRemove==0) return True;

// Set the replacement values for the removal world axes
// if the user didn't give any or got it wrong

   if (worldReplacement.nelements() != nRemove) {
      worldReplacement.resize(nRemove);
      for (uInt i=0; i<nRemove; i++) {
         worldReplacement(i) = csys.referenceValue()(remove(i));
      }
   }


// Now for each world axis in the list, get rid of the world and associated
// pixel axis

   for (Int k=(nRemove-1); k>=0; k--) {
      if (!csys.removeWorldAxis(remove(k), worldReplacement(k))) return False;
    }
   return True;
}      



Bool CoordinateUtil::removePixelAxes(CoordinateSystem& csys,
                                     Vector<Double>& pixelReplacement,
                                     const Vector<Int>& pixelAxes,
                                     const Bool removeThem)
{
// Bug out if nothing to do

   if (pixelAxes.nelements() == 0) return True;

// Make sure the pixel axes are valid

   uInt i,j;
   for (i=0; i<pixelAxes.nelements(); i++) {
      if (pixelAxes(i) >= Int(csys.nPixelAxes())) return False;
   }

// Make a list of the axes to remove in ascending order
// with no duplicates

   Vector<Int> remove(csys.nPixelAxes());
   if (removeThem) {
      remove.resize(pixelAxes.nelements());
      remove = pixelAxes;
      GenSort<Int>::sort(remove, Sort::Ascending, Sort::NoDuplicates);
   } else {
      for (i=0,j=0; i<csys.nPixelAxes(); i++) {
         if (!anyEQ(pixelAxes, Int(i))) remove(j++) = i;
      }
      remove.resize(j,True);
   }
   const uInt nRemove = remove.nelements();
   if (nRemove==0) return True;

// Set the replacement values for the removed pixel axes
// if the user didn't give any or got it wrong

   if (pixelReplacement.nelements() != nRemove) {
      pixelReplacement.resize(nRemove);
      for (i=0; i<nRemove; i++) {
         pixelReplacement(i) = csys.referencePixel()(remove(i));
      }
   }


// Now for each pixel axis in the list, get rid of it
 
   for (Int k=(nRemove-1); k>=0; k--) {
      if (!csys.removePixelAxis(remove(k), pixelReplacement(k))) return False;
   }
   return True;
}      



CoordinateSystem CoordinateUtil::makeCoordinateSystem(const IPosition& shape,
                                                      Bool doLinear)
{         
   const uInt n = shape.nelements();
   CoordinateSystem csys;

// Attach an ObsInfo record so images that are made
// with this have something sensible

   ObsInfo obsInfo;
   obsInfo.setObserver(String("Karl Jansky"));
   obsInfo.setTelescope(String("ALMA"));

// It must be easier than this...  USe 0.0001
// so that roundoff does not tick the 0 to 24

   Time time(2000, 1, 1, 0, 0, 0.0001);
   MVTime time2(time);
   MVEpoch time4(time2);
   MEpoch date(time4);
   obsInfo.setObsDate(date);
   csys.setObsInfo(obsInfo);
//
   if (doLinear) {
      Vector<String> names(n);
      for (uInt i=0; i<n; i++) {
         ostringstream oss;
         oss << (i+1);
         String t(oss);
         names(i) = "linear" + t;
      }
      CoordinateUtil::addLinearAxes(csys, names, shape);
      return csys;
   }
//
   Bool doneStokes = False;
   Bool doneFreq = False;
//
   if (n==1) {
      CoordinateUtil::addFreqAxis(csys);
      return csys;
   }
//          
   if (n>=2) {
      CoordinateUtil::addDirAxes(csys);
   }
//
   if (n>=3) {
      if (CoordinateUtil::addStokesAxis(csys, uInt(shape(2)))) {
         doneStokes = True;
      } else {
         CoordinateUtil::addFreqAxis(csys);
         doneFreq = True;   
      }  
   }
//
   uInt nDone = 0;
   if (n>=4) {
      nDone = 4;
      if (doneStokes) {
         CoordinateUtil::addFreqAxis(csys);
         doneFreq = True;
      } else {
         if (CoordinateUtil::addStokesAxis(csys, uInt(shape(3)))) {
            doneStokes = True;
         } else {
            if (!doneFreq) {
               CoordinateUtil::addFreqAxis(csys);
               doneFreq = True;
            } else {
               nDone = 3;
            }
         } 
      }
   }


// Linear for the rest

   if (nDone==3 || n >=5) {
      const uInt nLeft = n - nDone;
      if (nLeft > 0) {
         IPosition shape2(nLeft);
         Vector<String> names(nLeft);
         for (uInt i=0; i<nLeft; i++) {
            shape2(i) = shape(i+nDone);
            ostringstream oss;
            oss << (i+1);
            String t(oss);
            names(i) = "linear" + t;
         }
         CoordinateUtil::addLinearAxes(csys, names, shape2);
      }
   }
   return csys;
} 



Bool CoordinateUtil::makeDirectionMachine(LogIO& os, MDirection::Convert& machine,
                                          const DirectionCoordinate& dirCoordTo,
                                          const DirectionCoordinate& dirCoordFrom,
                                          const ObsInfo& obsTo,
                                          const ObsInfo& obsFrom) 
{
   const MDirection::Types& typeFrom = dirCoordFrom.directionType();
   const MDirection::Types& typeTo = dirCoordTo.directionType();
   Bool typesEqual = (typeTo==typeFrom);
//
   MEpoch epochFrom = obsFrom.obsDate();
   MEpoch epochTo = obsTo.obsDate();
   Double t1 = epochFrom.getValue().get();
   Double t2 = epochTo.getValue().get();
   Bool epochEqual = near(t1,t2);
//
   String telFrom = obsFrom.telescope();
   String telTo = obsTo.telescope();
   Bool posEqual = (telFrom==telTo);

// Everything is the same for input and output so we don't 
// need a machine to convert anything

   if (typesEqual && epochEqual && posEqual) return False;

// Start with simplest machine, just MDirection::Types.  If it does 
// the conversion, that's all we need.  If not, we need more.

   MDirection::Ref refFrom(typeFrom);
   MDirection::Ref refTo(typeTo);
   machine = MDirection::Convert(refFrom, refTo);
//
   MDirection fromMD;
   dirCoordFrom.toWorld(fromMD, dirCoordFrom.referencePixel());
   Bool ok = True;
   try {
      MDirection toMD = machine(fromMD);
   } catch (AipsError x) {
      ok = False;
   }
   if (ok) {
      if (typeFrom==typeTo) {
         return False;
      } else {          
         return True;
      }
   }

// The conversion failed, so we need either or both of epoch 
// and position in the machine.  

   os << LogOrigin("CoordinateUtil", "makeDirectionMachine");
   if (epochFrom.getValue().get() < 0.0 || epochTo.getValue().get() < 0.0)
      os << "The output CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;

// Now add the epoch to the machine and see if that works

   {
      MeasFrame frameFrom;
      MeasFrame frameTo;
//
      frameFrom.set(epochFrom);
      frameTo.set(epochTo);
      MDirection::Ref refFrom(typeFrom, frameFrom);
      MDirection::Ref refTo(typeTo, frameTo);
      machine = MDirection::Convert(refFrom, refTo);
//
      ok = True;
      try {
         MDirection toMD = machine(fromMD);
      } catch (AipsError x) {
         ok = False;
      }
      if (ok) return True;
   }

// Now add the position to the machine and see if that works

   if (telFrom==String("UNKNOWN")) {
      os << 
	"The output CoordinateSystem has no valid observatory name - cannot divine its position"
	 << LogIO::EXCEPTION;
   }
   if (telTo==String("UNKNOWN")) {
      os <<
	"The input CoordinateSystem has no valid observatory name - cannot divine its position"
	 << LogIO::EXCEPTION;
   }
//
   MPosition posFrom, posTo;
   findObservatoryOrRaiseException(os, posFrom, telFrom);
   findObservatoryOrRaiseException(os, posTo,   telTo);
//
   {
      MeasFrame frameFrom;
      MeasFrame frameTo;
//
      frameFrom.set(posFrom);
      frameTo.set(posTo);
      MDirection::Ref refFrom(typeFrom, frameFrom);
      MDirection::Ref refTo(typeTo, frameTo);
      machine = MDirection::Convert(refFrom, refTo);
//
      ok = True;
      try {
         MDirection toMD = machine(fromMD);
      } catch (AipsError x) {
         ok = False;
      }
      if (ok) return True;
   }

// Well looks like we need both

   {
      MeasFrame frameFrom(posFrom, epochFrom);
      MeasFrame frameTo(posTo, epochTo);
//
      MDirection::Ref refFrom(typeFrom, frameFrom);
      MDirection::Ref refTo(typeTo, frameTo);
//
      machine = MDirection::Convert(refFrom, refTo);
      ok = True;
      try {
         MDirection toMD = machine(fromMD);
      } catch (AipsError x) {
         ok = False;
      }
      if (!ok) {
         os << "Unable to convert between the inputand output  " <<
               "DirectionCoordinates - this is surprising !" << LogIO::EXCEPTION;
      }
   }
//
   return True;
}
 

Bool CoordinateUtil::makeFrequencyMachine(LogIO& os, MFrequency::Convert& machine,
                                          Int, Int,
                                          const CoordinateSystem& coordsTo,
                                          const CoordinateSystem& coordsFrom, 
                                          const Unit& unit)
{
   MDirection dirTo, dirFrom;
   {
      Coordinate::Type type = Coordinate::DIRECTION;
      Int afterCoord = -1;
      Int c = coordsTo.findCoordinate(type, afterCoord);
      if (c<0) {
         os << "No Direction coordinate in 'to' CoordinateSystem" << LogIO::EXCEPTION;
      }
      const DirectionCoordinate& dCoord = coordsTo.directionCoordinate(c);   
      const Vector<Double>& rp = dCoord.referencePixel();   
      if (!dCoord.toWorld(dirTo, rp)) {
         os << dCoord.errorMessage() << LogIO::EXCEPTION;
      }
   }
//
   {
      Coordinate::Type type = Coordinate::DIRECTION;
      Int afterCoord = -1;
      Int c = coordsFrom.findCoordinate(type, afterCoord);
      if (c<0) {
         os << "No Direction coordinate in 'from' CoordinateSystem" << LogIO::EXCEPTION;
      }
      const DirectionCoordinate& dCoord = coordsFrom.directionCoordinate(c);   
      const Vector<Double>& rp = dCoord.referencePixel();   
      if (!dCoord.toWorld(dirFrom, rp)) {
         os << dCoord.errorMessage() << LogIO::EXCEPTION;
      }
   }
//
   MFrequency::Types typeTo, typeFrom;
   {
      Coordinate::Type type = Coordinate::SPECTRAL;
      Int afterCoord = -1;
      Int c = coordsTo.findCoordinate(type, afterCoord);
      if (c<0) {
         os << "No Spectral coordinate in 'to' CoordinateSystem" << LogIO::EXCEPTION;
      }
      const SpectralCoordinate& sCoord = coordsTo.spectralCoordinate(c);   
      typeTo = sCoord.frequencySystem();
   }
   {
      Coordinate::Type type = Coordinate::SPECTRAL;
      Int afterCoord = -1;
      Int c = coordsFrom.findCoordinate(type, afterCoord);
      if (c<0) {
         os << "No Spectral coordinate in 'from' CoordinateSystem" << LogIO::EXCEPTION;
      }
      const SpectralCoordinate& sCoord = coordsFrom.spectralCoordinate(c);   
      typeFrom = sCoord.frequencySystem();
   }
//
   const ObsInfo& obsInfoTo = coordsTo.obsInfo();
   const ObsInfo& obsInfoFrom = coordsFrom.obsInfo();
//   
   String telFrom = obsInfoFrom.telescope();
   String telTo = obsInfoTo.telescope();
   MPosition posFrom, posTo;

   findObservatoryOrRaiseException(os, posFrom, telFrom);
   findObservatoryOrRaiseException(os, posTo,   telTo);
//
   return makeFrequencyMachine(os, machine, typeTo, typeFrom,
                               dirTo, dirFrom, 
                               obsInfoTo.obsDate(), 
                               obsInfoFrom.obsDate(), 
                               posTo, posFrom, unit);
}

void CoordinateUtil::findObservatoryOrRaiseException(LogIO& os,
						     MPosition& pos,
						     const String& tel)
{
  Bool found = MeasTable::Observatory(pos, tel);

  if(!found){
    os << "Cannot find the observatory name " << tel << " in the CASA" << endl;
    os << "database.  Please request that it be added." << LogIO::EXCEPTION;
  }
}
  

Bool CoordinateUtil::makeFrequencyMachine(LogIO& os, MFrequency::Convert& machine,
                                          MFrequency::Types typeTo,
					  MFrequency::Types typeFrom,
                                          const MDirection& dirTo,
					  const MDirection& dirFrom,
                                          const MEpoch& epochTo,
					  const MEpoch& epochFrom,
                                          const MPosition& posTo,
					  const MPosition& posFrom,
                                          const Unit& unit)
{
// Create frames

   MeasFrame frameFrom;
   MeasFrame frameTo;

// Add Direction

   frameFrom.set(dirFrom);
   frameTo.set(dirTo);

// Add Epoch   

   os << LogOrigin("CoordinateUtil", "makeFrequencyMachine");
   if(epochFrom.getValue().get() < 0.0)
      os << "The output CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;
   if(epochTo.getValue().get() < 0.0)
      os << "The input CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;
   frameFrom.set(epochFrom);
   frameTo.set(epochTo);

// Add the position 

   frameFrom.set(posFrom);
   frameTo.set(posTo);

// Make the machine

   MFrequency::Ref refFrom(typeFrom, frameFrom);
   MFrequency::Ref refTo(typeTo, frameTo);
   machine = MFrequency::Convert(unit, refFrom, refTo);

// Test a conversion

   Bool ok = True;
   MFrequency freqTo;
   Quantum<Double> freq(1.0e9, Unit(String("Hz")));
   MFrequency freqFrom(freq, typeFrom);
   try {
      freqTo = machine(freqFrom);
   } catch (AipsError x) {
      ok = False;
   }
   if (!ok) {
      os << LogIO::WARN;
      os << "Unable to convert between the input and output SpectralCoordinates" << endl;
      os << "this probably means one is in the REST frame which requires" << endl;
      os << "the radial velocity - this is not implemented yet" << LogIO::POST;
   }
//
   return ok;
}

Bool CoordinateUtil::holdsSky (Bool& holdsOneSkyAxis, const CoordinateSystem& csys, Vector<Int> pixelAxes)
{
   AlwaysAssert(pixelAxes.nelements()==2, AipsError);
//
   holdsOneSkyAxis = False;
   Int dirCoordinate = csys.findCoordinate(Coordinate::DIRECTION);
   if (dirCoordinate!=-1) {
      Vector<Int> dirPixelAxes = csys.pixelAxes(dirCoordinate);
      if ( (dirPixelAxes(0)==pixelAxes(0) && dirPixelAxes(1)==pixelAxes(1)) ||
           (dirPixelAxes(0)==pixelAxes(1) && dirPixelAxes(1)==pixelAxes(0))) {
         return True;
      }
//
      if ( (pixelAxes(0)==dirPixelAxes(0) && pixelAxes(1)!=dirPixelAxes(1)) ||
           (pixelAxes(0)!=dirPixelAxes(0) && pixelAxes(1)==dirPixelAxes(1)) ||
           (pixelAxes(0)==dirPixelAxes(1) && pixelAxes(1)!=dirPixelAxes(0)) ||
           (pixelAxes(0)!=dirPixelAxes(1) && pixelAxes(1)==dirPixelAxes(0)) ) {
         holdsOneSkyAxis = True;
      }
   }
   return False;
}


void CoordinateUtil::setNiceAxisLabelUnits(CoordinateSystem& csys)
{  
   for (uInt i = 0; i < csys.nCoordinates(); i++) {
     Coordinate::Type type = csys.type(i);
     if (type==Coordinate::DIRECTION) {
        setDirectionUnit (csys, String("deg"), i);
     } else if (type==Coordinate::SPECTRAL) {
        SpectralCoordinate coord(csys.spectralCoordinate(i));
        Vector<String> str(coord.nWorldAxes());
        for (uInt j = 0; j < str.nelements(); j++) str(j) = "km/s";
        MDoppler::Types oldDoppler = coord.velocityDoppler();
        coord.setVelocity (String("km/s"), oldDoppler);
        csys.replaceCoordinate(coord, i);
     }
   }
}


Bool CoordinateUtil::findSky(String&errorMessage, Int& dC, Vector<Int>& pixelAxes,
                             Vector<Int>& worldAxes, const CoordinateSystem& csys)
//    
// Assumes only one DirectionCoordinate .   {pixel,world}Axes says where
// in the CS the DirectionCoordinate axes are
//
{
   CoordinateUtil::findDirectionAxes (pixelAxes, worldAxes, dC, csys);
   if (dC<0 || pixelAxes.nelements()!=2 || worldAxes.nelements()!=2) {
      errorMessage = "Image does not have 2 sky coordinate axes";
      return False;
   }
// 
   for (uInt i=0; i<2; i++) {
      if (pixelAxes(i)==-1 || worldAxes(i)==-1) {
         errorMessage = "Image does not have 2 sky coordinate axes";
         return False;
      }
   }
//
   return True;
}


Stokes::StokesTypes CoordinateUtil::findSingleStokes (LogIO& os, const CoordinateSystem& csys,
                                                      uInt pixel)
{  
   Stokes::StokesTypes stokes(Stokes::Undefined);
   Int stokesCoordinateNumber = csys.findCoordinate(Coordinate::STOKES);
   if (stokesCoordinateNumber==-1) {
      os << LogIO::WARN
         << "There is no Stokes coordinate in the CoordinateSystem - assuming Stokes I"
         << LogIO::POST;
      stokes = Stokes::I;
   } else {
      StokesCoordinate stokesCoordinate = csys.stokesCoordinate(stokesCoordinateNumber);
// 
// Find out what Stokes the specified pixel belongs to.  
// 
      if (!stokesCoordinate.toWorld(stokes, Int(pixel))) {
         os << "StokesCoordinate conversion failed because "
            << stokesCoordinate.errorMessage() << LogIO::EXCEPTION;
      }
   }
   return stokes;
}

String CoordinateUtil::formatCoordinate(
	const IPosition& pixel, const CoordinateSystem& csys, Int precision
) {
	Vector<Double> pixel2(csys.nPixelAxes());
	for (uInt i=0; i<pixel2.nelements(); i++) {
		pixel2(i) = pixel(i);
	}
	return CoordinateUtil::formatCoordinate(pixel2, csys, precision);
}
   

String CoordinateUtil::formatCoordinate (
	const Vector<Double>& pixel, const CoordinateSystem& csys, Int precision
) {
	Vector<Double> world;

	if (!csys.toWorld(world, pixel)) {
		String err = String("Error converting coordinate position because ") + csys.errorMessage();
		throw(AipsError(err));
	}

	String s2;
	for (uInt i=0; i<world.nelements(); i++) {
		String u;
		String tmp = csys.format(
			u, Coordinate::DEFAULT, world(i), i,
			True, True, precision
		);
		String s = (u.empty()) ? tmp : tmp + u;

		s2 += (i == 0) ? s: ", " + s;

	}

	return s2;
}


Int CoordinateUtil::compareCoordinates (const CoordinateSystem& thiscsys,
					const CoordinateSystem& thatcsys)
{
  // This is the real conformance checker.
  /////  return coordinates().nearPixel (other.coordinates());    
  // See how the coordinate systems compare.
  Vector<Int> thisWorldAxes;
  Vector<Int> thatWorldAxes;
  Vector<Bool> refChange;
  if (! thiscsys.worldMap (thatWorldAxes, thisWorldAxes,
			   refChange, thatcsys)) {
    return 9;
  }
  // This must be a subset of that or that a subset of this.
  // We are interested in pixel axes only, so transform the world axes
  // to pixel axes and remove world axes without pixel axes.
  Vector<Int> thisPixelAxes = toPixelAxes (thiscsys, thatcsys, thisWorldAxes);
  Vector<Int> thatPixelAxes = toPixelAxes (thatcsys, thiscsys, thatWorldAxes);
  // thisPixelAxes tells which pixel axes of this are part of that.
  // thatPixelAxes tells which pixel axes of that are part of this.
  // Check if the axes are in the correct order (ascending).
  // I.e. it is not supported that this and that have the same axes,
  // but in a different order.
  if (! checkOrder (thisPixelAxes)) {
    return 9;
  }
  if (! checkOrder (thatPixelAxes)) {
    return 9;
  }
  // Only one of the coordinate systems can be a subset.
  Bool thisIsSubSet = anyLT (thatPixelAxes, 0);
  Bool thatIsSubSet = anyLT (thisPixelAxes, 0);
  if (thisIsSubSet) {
    if (thatIsSubSet) {
      return 9;
    }
    return -1;
  } else if (thatIsSubSet) {
    return 1;
  }
  return 0;      //equal
}

Vector<Int> CoordinateUtil::toPixelAxes (const CoordinateSystem& thiscsys,
					const CoordinateSystem& thatcsys,
					const Vector<Int>& worldAxes)
{
  // Map the world axes to pixel axes.
  Vector<Int> pixelAxes(thiscsys.nPixelAxes(), -1);
  for (uInt i=0; i<worldAxes.nelements(); i++) {
    if (worldAxes(i) >= 0) {
      Int pixAxis = thiscsys.worldAxisToPixelAxis (i);
      if (pixAxis >= 0) {
	pixelAxes(pixAxis) = thatcsys.worldAxisToPixelAxis (worldAxes(i));
      }
    }
  }
  return pixelAxes;
}

Bool CoordinateUtil::checkOrder (const Vector<Int>& pixelAxes)
{
  // Check if the mapped axes are in ascending order. I.e. we do not allow
  // that the order of axes in 2 images is different.
  Int last = -1;
  for (uInt i=0; i<pixelAxes.nelements(); i++) {
    if (pixelAxes(i) >= 0) {
      if (pixelAxes(i) <= last) {
	return False;
      }
      last = pixelAxes(i);
    }
  }
  return True;
}


Bool CoordinateUtil::findExtendAxes (IPosition& newAxes,
				     IPosition& stretchAxes,
				     const IPosition& newShape,
				     const IPosition& oldShape,
				     const CoordinateSystem& newcsys,
				     const CoordinateSystem& oldcsys)
{
  Vector<Int> oldWorldAxes;
  Vector<Int> newWorldAxes;
  Vector<Bool> refChange;
  if (! oldcsys.worldMap (newWorldAxes, oldWorldAxes,
			  refChange, newcsys)) {
    return False;
  }
  // Old must be a subset of new.
  // We are interested in pixel axes only, so transform the world axes
  // to pixel axes and remove world axes without pixel axes.
  Vector<Int> oldPixelAxes = toPixelAxes (oldcsys, newcsys, oldWorldAxes);
  Vector<Int> newPixelAxes = toPixelAxes (newcsys, oldcsys, newWorldAxes);
  // oldPixelAxes tells which pixel axes of old are not part of new.
  // newPixelAxes tells which pixel axes of new are not part of old.
  // Check if the axes are in the correct order.
  if (! checkOrder (oldPixelAxes)) {
    return False;
  }
  if (! checkOrder (newPixelAxes)) {
    return False;
  }
  // Old must be a subset of new.
  if (anyLT (oldPixelAxes, 0)) {
    return False;
  }
  // Find the new and stretch axes.
  uInt nrdim = newPixelAxes.nelements();
  if (nrdim != newShape.nelements()) {
    return False;
  }
  newAxes.resize (nrdim);
  stretchAxes.resize (nrdim);
  uInt nrn = 0;
  uInt nrs = 0;
  for (uInt i=0; i<nrdim; i++) {
    if (newPixelAxes(i) < 0) {
      newAxes(nrn++) = i;
    } else {
      if (i-nrn > oldShape.nelements()) {
	return False;
      }
      if (oldShape(i-nrn) == 1  &&  newShape(i) > 1) {
	stretchAxes(nrs++) = i;
      }
    }
  }
  newAxes.resize (nrn);
  stretchAxes.resize (nrs);
  return True;
}


Bool CoordinateUtil::cylindricalFix (CoordinateSystem& csys, String& errorMessage,
                                     const IPosition& shape)
{
   Vector<Int> pixelAxes, worldAxes;
   Int coord;
   findDirectionAxes(pixelAxes, worldAxes, coord, csys);
   if (coord < 0) return True;
//
   if (pixelAxes.nelements()<2  || worldAxes.nelements()<2) {
      errorMessage = String("not enough pixel or world axes in DirectionCoordinate");
      return False;
   }
   
// check shape here

   DirectionCoordinate dirCoord (csys.directionCoordinate(coord));
   if (pixelAxes[0] < 0 || pixelAxes[1] < 0 || !dirCoord.cylindricalFix (shape(pixelAxes[0]), shape(pixelAxes[1]))) {
      errorMessage = dirCoord.errorMessage();
      return False;      
   }
//
   csys.replaceCoordinate (dirCoord, coord);
   return True;
}

Bool CoordinateUtil::setVelocityState (String& errorMsg, CoordinateSystem& csys,
                                       const String& unit,
                                       const String& spcquant)
{
   static Unit kms(String("km/s"));
//
   Int after = -1;
   Int iS = csys.findCoordinate(Coordinate::SPECTRAL, after);
   if (iS>=0) {
      SpectralCoordinate sCoord = csys.spectralCoordinate(iS);

// Get current state
      //cout << "setVelocityState unit: " << unit << " spcquant: " << spcquant << endl;
      MDoppler::Types oldDoppler = sCoord.velocityDoppler();
      String oldVelUnit = sCoord.velocityUnit();
      SpectralCoordinate::SpecType oldspcType = sCoord.nativeType();

// Prepare new state

      MDoppler::Types newDoppler(oldDoppler);
      String newVelUnit(oldVelUnit);
      SpectralCoordinate::SpecType newspcType(oldspcType);

// Find new Doppler or spectral state, if any

      if (!spcquant.empty()) {
      	if (!MDoppler::getType(newDoppler, spcquant)
      	&& !SpectralCoordinate::stringtoSpecType(newspcType, spcquant)) {
      		errorMsg = String("Illegal velocity Doppler/spectral type");
            return False;
         }
      }

// Find new spectral unit if any

     if (!unit.empty()) {
        newVelUnit = unit;
     }

// Set new doppler.

     if (!sCoord.setVelocity (newVelUnit, newDoppler)) {
        errorMsg = sCoord.errorMessage();
        return False;
     }

// Set new spectral type.

     if (!sCoord.setNativeType(newspcType)) {
   	  errorMsg = sCoord.errorMessage();
   	  return False;
     }

// Replace in CS

      csys.replaceCoordinate(sCoord, iS);
   }
   return True;
}
   

Bool CoordinateUtil::setSpectralState (String& errorMsg, CoordinateSystem& csys,
                                       const String& unit,
                                       const String& spcquant)
{
   static Unit KMS(String("km/s"));
   static Unit HZ(String("GHz"));
   static Unit M(String("m"));
//

   //cout << "setSpectralState unit: " << unit << " spcype: " << spcquant << endl;

   Int after = -1;
   Int iS = csys.findCoordinate(Coordinate::SPECTRAL, after);
   if (iS>=0) {
      SpectralCoordinate sCoord = csys.spectralCoordinate(iS);

// Prepare new state

      MDoppler::Types newDoppler(sCoord.velocityDoppler());
      String newVelUnit(sCoord.velocityUnit());
      String newWaveUnit(sCoord.wavelengthUnit());
      SpectralCoordinate::SpecType newspcType = sCoord.nativeType();
      Vector<String> newWorldAxisUnits(sCoord.worldAxisUnits().copy());

// Find new Doppler, if any

      if (!spcquant.empty()) {
         if (!MDoppler::getType(newDoppler, spcquant)
         && !SpectralCoordinate::stringtoSpecType(newspcType, spcquant)) {
            errorMsg = String("Illegal velocity Doppler/spectral type");
            return False;
         }
      }

// If the spectral unit is consistent with Hz, we update the world
// axis units.  If it is consistent with km/s we update the 
// velocity state


     if (!unit.empty()) {
        Unit t(unit);
        if (t == HZ) {
        	//cout << "New HZ" << endl;
           newWorldAxisUnits[0] = unit;         
        } else if (t == KMS) {
        	//cout << "New velocity" << endl;
           newVelUnit = unit;
        } else if (t == M) {
        	//cout << "New wavelength unit " <<endl;
        	newWaveUnit = unit;
            //newWorldAxisUnits[0] = "Hz";
        } else {
           errorMsg = String("Illegal spectral unit");
           return False;
        }
     }

// Set new state.  

     if (!sCoord.setVelocity (newVelUnit, newDoppler)) {
        errorMsg = sCoord.errorMessage();
        return False;
     }
//
     if (!sCoord.setWorldAxisUnits(newWorldAxisUnits)) {
        errorMsg = sCoord.errorMessage();
        return False;
     }

//
     if (!sCoord.setWavelengthUnit(newWaveUnit)) {
    	 errorMsg = sCoord.errorMessage();
    	 return False;
     }

// Set spectral type.
     if (!sCoord.setNativeType(newspcType)) {
   	  errorMsg = sCoord.errorMessage();
   	  return False;
     }

// Replace in CS

      csys.replaceCoordinate(sCoord, iS);
   }
   return True;
}

Bool CoordinateUtil::setSpectralFormatting (String& errorMsg, 
                                            CoordinateSystem& csys,
                                            const String& unit,
                                            const String& spcquant)
//
// This function sets the default formatting unit of the SpectralCoordinate in a
// CoordinateSystem to that given.  It also updates the internal state
// of the velocity machine in that SC to reflect the given Doppler.  This
// is because SC::format used the SC::frequencytovelocity functions to convert
// to velocity and the Doppler is embedded in this.  To get this dependency out,
// the formatter would need its own velocity machine and interface to set
// the frame and Doppler.
//
{
   Int after = -1;
   Int iS = csys.findCoordinate(Coordinate::SPECTRAL, after);
      if (iS>=0) {
      SpectralCoordinate sCoord = csys.spectralCoordinate(iS);
     
// Set format Unit
      //cout << "setSpectralFormatting unit: " << unit << " spcquant: " << spcquant << endl;
           
      sCoord.setFormatUnit (unit);
      
// Velocity State

      MDoppler::Types oldDoppler = sCoord.velocityDoppler();
      String oldVelUnit = sCoord.velocityUnit();
      SpectralCoordinate::SpecType oldspcType = sCoord.nativeType();
//  
      MDoppler::Types newDoppler(oldDoppler);
      String newVelUnit(oldVelUnit);
      SpectralCoordinate::SpecType newspcType(oldspcType);

// Find new Doppler, if any
                                        
      if (!spcquant.empty()) {
         if (!MDoppler::getType(newDoppler, spcquant)
         && !SpectralCoordinate::stringtoSpecType(newspcType, spcquant)){
            errorMsg = String("Illegal velocity Doppler/spectral state - no change");
            newDoppler = oldDoppler;
            newspcType = oldspcType;
            return False;
         }
      }
//
     if (oldDoppler != newDoppler) {
        if (!sCoord.setVelocity (newVelUnit, newDoppler)) {
           errorMsg = sCoord.errorMessage();
           return False;
        }
     }

// Set spectral type.
     if (newspcType != oldspcType){
   	  if (!sCoord.setNativeType(newspcType)) {
   		  errorMsg = sCoord.errorMessage();
   		  return False;
   	  }
     }

// Replace in CS
  
      csys.replaceCoordinate(sCoord, iS);
   }
//
   return True;
}
   

Bool CoordinateUtil::isSky (LogIO& os, const CoordinateSystem& cSys) {   
    const uInt nPixelAxes = cSys.nPixelAxes();

    if (nPixelAxes != 2) {
        os << "The CoordinateSystem is not two dimensional. It has " 
            << nPixelAxes << " dimensions" << LogIO::EXCEPTION;
    }  
    Bool xIsLong = True;
    Int dirCoordinate = cSys.findCoordinate(Coordinate::DIRECTION);
    if (dirCoordinate==-1) {
        os << "There is no DirectionCoordinate (sky) in this CoordinateSystem" << LogIO::EXCEPTION;
    }
    Vector<Int> dirPixelAxes = cSys.pixelAxes(dirCoordinate);
    if (dirPixelAxes(0) == -1 || dirPixelAxes(1) == -1) {
        os << "The pixel axes for the DirectionCoordinate have been removed" << LogIO::EXCEPTION;
    }
 
    // Which axis is longitude and which is latitude

    if(dirPixelAxes(0)==0 && dirPixelAxes(1)==1) {
        xIsLong = True;
    } else {
        xIsLong = False;
    }
    return xIsLong;
} 

Bool CoordinateUtil::setRestFrequency (String& errorMsg, CoordinateSystem& cSys,
                                       const String& unit,
                                       const Double& value)
{
   static Unit HZ(String("GHz"));
   static Unit M(String("m"));
//


   Int after = -1;
   Int iS = cSys.findCoordinate(Coordinate::SPECTRAL, after);
   if (iS>=0) {
      SpectralCoordinate sCoord = cSys.spectralCoordinate(iS);

// Check for weird value

      if (value < 0.0){
      	errorMsg = String("The rest frequency/wavelength is below zero!");
      	return False;
      }
      else if (isNaN(value)){
      	errorMsg = String("The rest frequency/wavelength is NaN!");
      	return False;
      }
      else if (isInf(value)){
      	errorMsg = String("The rest frequency/wavelength is InF!");
      	return False;
      }

// Get the old rest frequency and unit

      Double oldValue = sCoord.restFrequency();
      Unit   oldUnit  = Unit(sCoord.worldAxisUnits()(0));

// Check whether something has to be done

      if (!unit.empty() && (value != oldValue) && (value>0 || oldValue>0)){

// Make sure the unit conforms with m or Hz
      	Unit t(unit);
         if (t != HZ && t!= M) {
            errorMsg = String("Illegal spectral unit");
            return False;
         }

// Compute the rest frequency in the given units from the input

      	Quantity newQuant=Quantity(value, Unit(unit));
			MVFrequency newFreq = MVFrequency(newQuant);
			Double newValue = newFreq.get(oldUnit).getValue();

// Exclude weird numbers

	      if (isNaN(newValue)){
	      	errorMsg = String("The new rest frequency/wavelength is NaN!");
	      	return False;
	      }
	      else if (isInf(newValue)){
	      	errorMsg = String("The new rest frequency/wavelength is InF!");
	      	return False;
	      }

// Set the new rest frequency

	      if (!sCoord.setRestFrequency(newValue)) {
				errorMsg = sCoord.errorMessage();
				return False;
			}
      }

// Replace in CS

      cSys.replaceCoordinate(sCoord, iS);
   }
   return True;
}

Bool CoordinateUtil::setSpectralConversion (String& errorMsg, 
                                            CoordinateSystem& cSys,
                                            const String frequencySystem)
{
// Set conversion type.  This lets the SC convert to other frequency systems  
// We need some extra info from the ObsInfo for the Spectral conversion layer
// We avoid trying to fish it out unless we have to, because it might
// not be present and we would get unecessary failures.
             
   Int after = -1;
   Int iS = cSys.findCoordinate(Coordinate::SPECTRAL, after);
   if (iS>=0) {
      SpectralCoordinate coord(cSys.spectralCoordinate(iS));
      MFrequency::Types oldctype;
      MEpoch epoch;
      MPosition position;
      MDirection direction;
      coord.getReferenceConversion(oldctype, epoch, position, direction);
//
      MFrequency::Types ctype;
      if (!MFrequency::getType(ctype, frequencySystem)) {
         errorMsg = String("invalid frequency system");
         return False;
      }
// 
      if (ctype!=oldctype) {

// We also need a direction. Use the reference if we can find one

         after = -1;
         Int cD = cSys.findCoordinate(Coordinate::DIRECTION, after);
         if (cD<0) {
            errorMsg = String("No DirectionCoordinate; cannot set Spectral conversion layer");
            return False;
         } else {
            const DirectionCoordinate& dCoord = cSys.directionCoordinate(cD);
            const Vector<Double>& rp = dCoord.referencePixel();
            if (!dCoord.toWorld(direction, rp)) {
               errorMsg = dCoord.errorMessage();
               return False;
            }   
       
// Now find the epoch and position

            const ObsInfo& oi = cSys.obsInfo();
            String telescope = oi.telescope();
            if (!MeasTable::Observatory(position, telescope)) {
               errorMsg = String("Cannot find observatory; cannot set Spectral conversion layer");
               return False;
            } else {
               epoch = oi.obsDate();
               Double t = epoch.getValue().get();
               if (t <= 0.0) {
                  errorMsg = String("Epoch not valid; cannot set Spectral conversion layer");
                  return False;
               } else {
                  coord.setReferenceConversion(ctype, epoch, position, direction);
               }
            }
         }
      }
// 
      cSys.replaceCoordinate(coord, iS);
   }
   return True;
} 


Bool CoordinateUtil::setDirectionUnit (CoordinateSystem& csys, const String& unit, Int which)
{

// FInd DC

   Vector<Int> pixelAxes, worldAxes;
   Int iC = which;
   if (iC < 0) {
      CoordinateUtil::findDirectionAxes (pixelAxes, worldAxes, iC, csys);
   } else {
      worldAxes = csys.worldAxes (iC);
   }
//
   if (iC >= 0) {

// Fill  a vector of units for the unremoved DC axes

      uInt nWorldAxes = 0;
      for (uInt i=0; i<worldAxes.nelements(); i++) {
        if (worldAxes[i] >= 0) nWorldAxes++;
      }
      Vector<String> units(nWorldAxes);
      units = unit;

// Now set them

      return CoordinateUtil::setCoordinateUnits (csys, units, iC);
   }
   return True;
}

 
Bool CoordinateUtil::setDirectionConversion (String& errorMsg, 
                                             CoordinateSystem& csys,
                                             const String directionSystem)
{
   Int after = -1;
   Int iS = csys.findCoordinate(Coordinate::DIRECTION, after);
   if (iS>=0) {

// Convert code from String

      String code = directionSystem;
      code.upcase();
      MDirection::Types type;
      if (!MDirection::getType(type, code)) {
         errorMsg = String("Invalid direction reference system");
         return False;
      }

// Update and replace

      DirectionCoordinate coord = csys.directionCoordinate (iS);
      coord.setReferenceConversion(type);
      csys.replaceCoordinate(coord, iS);
   }
   return True;
} 


Bool CoordinateUtil::setCoordinateUnits (CoordinateSystem& csys, const Vector<String>& units, uInt which)
{
  AlwaysAssert(which<csys.nCoordinates(), AipsError);

// Find the world axes for this coordinate

  Vector<Int> worldAxes = csys.worldAxes(which);
  uInt nWorldAxes = 0;
  for (uInt i=0; i<worldAxes.nelements(); i++) {
     if (worldAxes[i] >=0) nWorldAxes++;
  }

// Make sure we have the right number

  AlwaysAssert(nWorldAxes==units.nelements(), AipsError);

// Find the world units vector for this CS

  Vector<String> tUnits = csys.worldAxisUnits().copy();

// Now slot in the new units.  For the removed axes, their units
// are unchanged.  They can never be brought back so it doesn't matter.

  uInt j = 0;
  for (uInt i=0; i<worldAxes.nelements(); i++) {
     if (worldAxes[i] >= 0) {
        tUnits[worldAxes[i]] = units[j];
        j++;
     }
  }
//
  return csys.setWorldAxisUnits (tUnits);
}


Coordinate::Type CoordinateUtil::findPixelAxis (const CoordinateSystem& csys, Int axis)
{
   Int coord, axisInCoordinate;
   csys.findPixelAxis(coord, axisInCoordinate, axis);
   if (coord<0) {
      throw(AipsError("Given pixel axis does not exist in CoordinateSystem"));
   }
//
   return csys.type (coord);
}

Coordinate::Type CoordinateUtil::findWorldAxis (const CoordinateSystem& csys, Int axis)
{
   Int coord, axisInCoordinate;
   csys.findWorldAxis(coord, axisInCoordinate, axis);
   if (coord<0) {
      throw(AipsError("Given world axis does not exist in CoordinateSystem"));
   }
//
   return csys.type (coord);
}


Bool CoordinateUtil::dropRemovedAxes (
	CoordinateSystem& csysOut,
	const CoordinateSystem& csysIn,
	Bool preserveAxesOrder
) {
   Bool dropped = False;
   CoordinateSystem tmp;
   csysOut = tmp;

   csysOut.setObsInfo(csysIn.obsInfo());

   Vector<Int> removeWorld(csysIn.nPixelAxes());
   Vector<Int> removePixel(csysIn.nWorldAxes());

   uInt k = 0;
   uInt l = 0;
   vector<Int> worldAxesOrder;
   vector<Int> pixelAxesOrder;
   for (uInt i=0; i<csysIn.nCoordinates(); i++) {

      const Vector<Int>& pixelAxesIn = csysIn.pixelAxes(i);
      const Vector<Int>& worldAxesIn = csysIn.worldAxes(i);
      AlwaysAssert(pixelAxesIn.nelements()==worldAxesIn.nelements(), AipsError);

      Bool allRemoved = allEQ(pixelAxesIn, -1) && allEQ(worldAxesIn,-1);
      if (allRemoved) {
        dropped = True;
      } else {
        csysOut.addCoordinate(csysIn.coordinate(i));
       if (preserveAxesOrder) {
        	for (uInt m=0; m<pixelAxesIn.size(); m++) {
        		if (worldAxesIn[m] >= 0) {
        			worldAxesOrder.push_back(worldAxesIn[m]);
        		}
        		if (pixelAxesIn[m] >= 0) {
        			pixelAxesOrder.push_back(pixelAxesIn[m]);
        		}
        	}
      }

// Maintain a list of axes to do virtual removal of

         Int c = csysOut.nCoordinates() - 1;
         Vector<Int> pixelAxesOut = csysOut.pixelAxes(c);
         Vector<Int> worldAxesOut = csysOut.worldAxes(c);
         AlwaysAssert(pixelAxesOut.nelements()==worldAxesOut.nelements(), AipsError);
         AlwaysAssert(pixelAxesIn.nelements()==worldAxesIn.nelements(), AipsError);
         const uInt n = worldAxesOut.nelements();

         for (uInt j=0; j<n; j++) {
            if (worldAxesIn(j)<0) {                  // Both world & pixel removed
               removeWorld(k) = worldAxesOut(j);
               k++;
            } else if (pixelAxesIn(j)<0) {
               removePixel(l) = pixelAxesOut(j);
               l++;
            }
         }
      }
   }

// There should be no axes in common in these two lists because
// when a world axis is removed, so is its pixel axis

   Double replacement;

   if (k>0) {
      removeWorld.resize(k, True);
      GenSort<Int>::sort(removeWorld, Sort::Descending, Sort::NoDuplicates);

      for (uInt i=0; i<removeWorld.nelements(); i++) {
         replacement = csysIn.referenceValue()(removeWorld[i]);
         csysOut.removeWorldAxis(removeWorld[i], replacement);
      }
   }
   if (l>0) {
      removePixel.resize(l, True);
      GenSort<Int>::sort(removePixel, Sort::Descending, Sort::NoDuplicates);
//
      for (uInt i=0; i<removePixel.nelements(); i++) {
         replacement = csysIn.referencePixel()(removePixel[i]);
         csysOut.removePixelAxis(removePixel[i], replacement);
      }
   }
   if (preserveAxesOrder) {
	   csysOut.transpose(Vector<Int>(worldAxesOrder), Vector<Int>(pixelAxesOrder));
   }
   return dropped;
}



CoordinateSystem CoordinateUtil::makeBinnedCoordinateSystem (const IPosition& factors,
                                                             const CoordinateSystem& csysIn,
                                                             Bool failOnStokes)
{
   const uInt nDim = factors.nelements();
   AlwaysAssert(csysIn.nPixelAxes()==nDim,AipsError);

// Check Stokes.

   if (failOnStokes) {  
      Int coord, axisInCoord;
      for (uInt i=0; i<nDim; i++) {
         if (factors(i) != 1) {
            csysIn.findPixelAxis(coord, axisInCoord, i);
            if (csysIn.type(coord) == Coordinate::STOKES) {
               throw (AipsError ("You cannot rebin a Stokes axis"));
            }
         }
      }
   }
   
// Set output values

   Vector<Double> incrIn(csysIn.increment().copy());
   Vector<Double> incrOut(incrIn.copy());
   Vector<Double> refPixIn(csysIn.referencePixel().copy());
   Vector<Double> refPixOut(refPixIn.copy());

// Loop over pixel axes

   for (uInt pA=0; pA<nDim; pA++) {
     refPixOut(pA) = (refPixIn(pA) + 0.5) / factors[pA] - 0.5;
//
     Int wA = csysIn.pixelAxisToWorldAxis(pA);
     if (wA>=0) {
        incrOut(wA) *= Double(factors[pA]);
     }
   }
//  
    CoordinateSystem csysOut(csysIn);
    csysOut.setReferencePixel(refPixOut);
    csysOut.setIncrement(incrOut);
//
    return csysOut;
}


 
String CoordinateUtil::axisLabel (const Coordinate& coord, uInt axis, 
                                  Bool doWorld, Bool doAbs, Bool doVel)
{
  String axisName = coord.worldAxisNames()(axis);
//
  String nativeUnit = coord.worldAxisUnits()(axis);
//
  Coordinate::Type ctype = coord.type();
  String base;
//
  if (ctype == Coordinate::DIRECTION) {
     const DirectionCoordinate& dcoord = dynamic_cast<const DirectionCoordinate&>(coord);
//
     MDirection::Types dtype = dcoord.directionType();
     MDirection::Types ctype;
     dcoord.getReferenceConversion(ctype);
//
     Bool isLong = (axis==0);

// Depending on the requested labelling type, we convert
// the axis unit name to something sensible. This is
// because it's confusing to see Galactic coordinates
// called 'Right Ascension' say.

     uInt ctypeI = static_cast<uInt>(ctype);
     MDirection::GlobalTypes gType = MDirection::globalType(ctypeI);
     if (dtype != ctype) {
        if (gType==MDirection::GRADEC) {
           if (isLong) {
              axisName = "Right Ascension";
           } else {
              axisName = "Declination";
           }
        } else if (gType==MDirection::GHADEC) {
           if (isLong) {
              axisName = "Hour Angle";
           } else {
              axisName = "Declination";
           }
        } else if (gType==MDirection::GAZEL) {
           if (isLong) {
              axisName = "Azimuth";
           } else {
              axisName = "Elevation";
           }
        } else if (gType==MDirection::GLONGLAT) {
           if (isLong) {
              axisName = "Longitude";
           } else {
              axisName = "Latitude";
           }
        }
     }
//
     String stype = MDirection::showType(ctype);
     if (doAbs) {
        if (doWorld) {
           base = stype + String(" ") + axisName;
        } else {
           base = stype + String(" ") + axisName + String(" (pixels)");
        }
     } else {
        base = String("Relative ") + stype + String(" ") + axisName +
               String(" (") + nativeUnit + String(")");
     }
  } else if (ctype == Coordinate::SPECTRAL) {
    const SpectralCoordinate& dcoord = dynamic_cast<const SpectralCoordinate&>(coord);

// Get frame conversion state

    MFrequency::Types ctype;
    MEpoch epoch;
    MPosition position;
    MDirection direction;
    dcoord.getReferenceConversion(ctype, epoch, position, direction);
    String freqType = MFrequency::showType(ctype);

// Get velocity state

//
    if (doWorld) {

// We must avoid making a unit from the String 'pixels'

       if (doVel) {
          String velUnit = dcoord.velocityUnit();
          String doppler = MDoppler::showType(dcoord.velocityDoppler());
          base = freqType + String(" ") + doppler + String(" velocity (") + velUnit + String(")");
       } else {
          base = freqType + String(" ") + axisName + String(" (") + nativeUnit + String(")");
       }
    } else {
       base = freqType + String(" ") + axisName + String(" (pixels)");
    }
//
    if (!doAbs) {
      base = String("Relative ") + base;
    }
  } else if (ctype==Coordinate::STOKES) {
     base = axisName;
     if (doWorld) {
        if (!doAbs) base = String("Relative ") + base;
     } else {
        if (!doAbs) base = String("Relative ") + base + 
          String(" (") + nativeUnit + String(")");
     }
  } else {
    base = axisName + String(" (") + nativeUnit + String(")");
    if (!doAbs) base = String("Relative ") + base;
  }
  return base;
}

} // end namespace casacore
