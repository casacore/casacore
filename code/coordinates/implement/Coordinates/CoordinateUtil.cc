//# CoordinateUtils.cc: 
//# Copyright (C) 1996,1997,1998,1999,2000
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

#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/Projection.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MeasTable.h>
//#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/OS/Time.h>
#include <aips/Quanta/QC.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/String.h>

#include <strstream.h>


void CoordinateUtil::addDirAxes(CoordinateSystem & coords){
  Matrix<Double> xform(2, 2); xform = 0.0; xform.diagonal() = 1.0;
  DirectionCoordinate dirAxes(MDirection::J2000, 
			       Projection(Projection::SIN),
			       0.0, 0.0, // Ref is at RA = 0, Dec = 0
			       1.0, 1.0, // The increment is overwritten below
			       xform,    // Rotation matrix
			       0.0, 0.0  // Ref pixel is 0,0
			       );
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
  SpectralCoordinate freqAxis(MFrequency::LSRK, // Local standard of rest
			      1415E6,          // ref. freq. = 1415MHz
			      1E3,             // 1 kHz bandwidth/channel
			      0.0);            // channel 0 is the ref.
  freqAxis.setRestFrequency(QC::HI.getValue(Unit("Hz")));    // HI
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
       units(i) = String("arcsec");
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



Bool CoordinateUtil::removeAxes(CoordinateSystem& cSys,
                                Vector<Double>& worldReplacement,
                                const Vector<uInt>& worldAxes,
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
      if (worldAxes(i) >= cSys.nWorldAxes()) return False;         
   }

// Make a list of the axes to remove in ascending order
// with no duplicates

   Vector<uInt> remove(cSys.nWorldAxes());
   if (removeThem) {
      remove.resize(worldAxes.nelements());
      remove = worldAxes;
      GenSort<uInt>::sort(remove, Sort::Ascending, Sort::NoDuplicates);
   } else {
      for (i=0,j=0; i<cSys.nWorldAxes(); i++) {
         if (!anyEQ(worldAxes, i)) remove(j++) = i;
      }
      remove.resize(j,True);
   }
   const uInt nRemove = remove.nelements();
   if (nRemove==0) return True;

// Set the replacement values for the removal world axes
// if the user didn't give any or got it wrong

   if (worldReplacement.nelements() != nRemove) {
      worldReplacement.resize(nRemove);
      for (i=0; i<nRemove; i++) {
         worldReplacement(i) = cSys.referenceValue()(remove(i));
      }
   }


// Now for each world axis in the list, get rid of the world and associated
// pixel axis
 
   uInt worldAxis = remove(0);
   for (i=0; i<nRemove; i++) {

// Remove the axis

      if (!cSys.removeWorldAxis(worldAxis, worldReplacement(i))) return False;

// Find the next world axis to eradicate

      if (i+1<remove.nelements()) worldAxis = remove(i+1) - 1;
   }
   return True;
}      






CoordinateSystem CoordinateUtil::makeCoordinateSystem(const IPosition& shape,
                                                      Bool doLinear)
{         
   const uInt n = shape.nelements();
   CoordinateSystem cSys;

// Attach an ObsInfo record so images that are made
// with this have something sensible

   ObsInfo obsInfo;
   obsInfo.setObserver(String("NoY2K"));
   obsInfo.setTelescope(String("ATCA"));

// It must be easier than this...  USe 0.0001
// so that roundoff does not tick the 0 to 24

   Time time(2000, 1, 1, 0, 0, 0.0001);
   MVTime time2(time);
   MVEpoch time4(time2);
   MEpoch date(time4);
   obsInfo.setObsDate(date);
   cSys.setObsInfo(obsInfo);
//
   if (doLinear) {
      Vector<String> names(n);
      for (uInt i=0; i<n; i++) {
         ostrstream oss;
         oss << (i+1);
         String t(oss);
         names(i) = "linear" + t;
      }
      CoordinateUtil::addLinearAxes(cSys, names, shape);
      return cSys;
   }
//
   Bool doneStokes = False;
   Bool doneFreq = False;
//
   if (n==1) {
      CoordinateUtil::addFreqAxis(cSys);
      return cSys;
   }
//          
   if (n>=2) {
      CoordinateUtil::addDirAxes(cSys);
   }
//
   if (n>=3) {
      if (CoordinateUtil::addStokesAxis(cSys, uInt(shape(2)))) {
         doneStokes = True;
      } else {
         CoordinateUtil::addFreqAxis(cSys);
         doneFreq = True;   
      }  
   }
//
   uInt nDone = 0;
   if (n>=4) {
      Bool ok = True;
      nDone = 4;
      if (doneStokes) {
         CoordinateUtil::addFreqAxis(cSys);
         doneFreq = True;
      } else {
         if (CoordinateUtil::addStokesAxis(cSys, uInt(shape(3)))) {
            doneStokes = True;
         } else {
            if (!doneFreq) {
               CoordinateUtil::addFreqAxis(cSys);
               doneFreq = True;
            } else {
               ok = False;
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
            ostrstream oss;
            oss << (i+1);
            String t(oss);
            names(i) = "linear" + t;
         }
         CoordinateUtil::addLinearAxes(cSys, names, shape2);
      }
   }
   return cSys;
} 



Bool CoordinateUtil::makeDirectionMachine(LogIO& os, MDirection::Convert& machine,
                                          const DirectionCoordinate& dirCoordTo,
                                          const DirectionCoordinate& dirCoordFrom,
                                          const ObsInfo& obsTo,
                                          const ObsInfo& obsFrom) 
//
// We need MDirection type, position on earth and epoch.  But 
// maybe not all of them...
//
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

   Double t = epochFrom.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The output CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;
   }
   t = epochTo.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The input CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;
   }

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
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The output CoordinateSystem has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
   if (telTo==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The input CoordinateSystem has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
//
   MPosition posFrom, posTo;
   Bool found = MeasTable::Observatory(posFrom, telFrom);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telFrom << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
   found = MeasTable::Observatory(posTo, telTo);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telTo << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
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
                                          Int coordinateTo, Int coordinateFrom,
                                          const CoordinateSystem& coordsTo,
                                          const CoordinateSystem& coordsFrom)
//
// We need MDirection type, position on earth and epoch.  But 
// maybe not all of them...
//
{
   const ObsInfo& obsTo = coordsTo.obsInfo();
   const ObsInfo& obsFrom = coordsFrom.obsInfo();
   const SpectralCoordinate& specCoordTo = coordsTo.spectralCoordinate(coordinateTo);
   const SpectralCoordinate& specCoordFrom = coordsFrom.spectralCoordinate(coordinateFrom);
//
// We need to have a DirectionCoordinate without which we can't make any
// conversions

   Int afterCoord = -1;
   Int coordinateDirTo = coordsTo.findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (coordinateDirTo==-1) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "No DirectionCoordinate was found in the input CoordinateSystem" << LogIO::EXCEPTION;
   }
   afterCoord = -1;
   Int coordinateDirFrom = coordsFrom.findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (coordinateDirFrom==-1) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "No DirectionCoordinate was found in the output CoordinateSystem" << LogIO::EXCEPTION;
   }
//
   const DirectionCoordinate& dirCoordTo = coordsTo.directionCoordinate(coordinateDirTo);
   const DirectionCoordinate& dirCoordFrom = coordsFrom.directionCoordinate(coordinateDirFrom);
   Bool dirCoordEqual = dirCoordTo.near(dirCoordFrom);

// See if we need machine

   const MFrequency::Types typeTo = specCoordTo.frequencySystem();
   const MFrequency::Types typeFrom = specCoordFrom.frequencySystem();
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

// Bug out if everything is the same

   if (dirCoordEqual && typesEqual && epochEqual && posEqual) return False;

// Create frames

   MeasFrame frameFrom;
   MeasFrame frameTo;

// Add Direction

   MDirection MDFrom, MDTo;
   if (!dirCoordFrom.toWorld(MDFrom, dirCoordFrom.referencePixel())) {
      os << "DirectionCoordinate coordinate conversion failed because " +
            dirCoordFrom.errorMessage() << LogIO::EXCEPTION;
   }
   if (!dirCoordFrom.toWorld(MDFrom, dirCoordFrom.referencePixel())) {
      os << "DirectionCoordinate coordinate conversion failed because " +
            dirCoordTo.errorMessage() << LogIO::EXCEPTION;
   }
   frameFrom.set(MDFrom);
   frameTo.set(MDTo);

// Add Epoch   

   Double t = epochFrom.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "The output CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;
   }
   t = epochTo.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "The input CoordinateSystem has no valid epoch" << LogIO::EXCEPTION;
   }
   frameFrom.set(epochFrom);
   frameTo.set(epochTo);

// Add the position 

   if (telFrom==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The output CoordinateSystem has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
   if (telTo==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The input CoordinateSystem has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
//
   MPosition posFrom, posTo;
   Bool found = MeasTable::Observatory(posFrom, telFrom);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telFrom << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
   found = MeasTable::Observatory(posTo, telTo);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telTo << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
   frameFrom.set(posFrom);
   frameTo.set(posTo);

// Make the machine

   MFrequency::Ref refFrom(typeFrom, frameFrom);
   MFrequency::Ref refTo(typeTo, frameTo);
   machine = MFrequency::Convert(refFrom, refTo);

// Test conversion

   Bool ok = True;
   MFrequency MFTo, MFFrom;
   if (!specCoordFrom.toWorld(MFFrom, specCoordFrom.referencePixel()(0))) {
      os << "SpectralCoordinate coordinate conversion failed because " +
            specCoordFrom.errorMessage() << LogIO::EXCEPTION;
   }
   try {
      MFTo = machine(MFFrom);
   } catch (AipsError x) {
      ok = False;
   }
   if (!ok) {
      os << "Unable to convert between the input and output SpectralCoordinates" << endl;
      os << "SpectralCoordinates - this probably means one is in the REST frame" << endl;
      os << "which requires the radial velocity (not implemented yet)" << LogIO::EXCEPTION;
   }
//
   return True;
}
