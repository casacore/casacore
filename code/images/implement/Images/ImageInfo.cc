//# ImageInfo.cc: Miscellaneous information related to an image
//# Copyright (C) 1998,1999
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
//#
//# $Id$

#include <trial/Images/ImageInfo.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/QuantumHolder.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>

#include <iostream.h>

Vector<Quantum<Double> > ImageInfo::defaultRestoringBeam()
{
    Vector<Quantum<Double> > tmp;
    return tmp;
}

ImageInfo::ImageInfo()
: itsRestoringBeam(defaultRestoringBeam())
{}

ImageInfo::~ImageInfo()
{}

void ImageInfo::copy_other(const ImageInfo &other)
{
    if (this != &other) {
       itsRestoringBeam.resize(other.itsRestoringBeam.nelements());
       itsRestoringBeam = other.itsRestoringBeam.copy();
    }
}

ImageInfo::ImageInfo(const ImageInfo &other)
{
    copy_other(other);
}

ImageInfo &ImageInfo::operator=(const ImageInfo &other)
{
    copy_other(other);
    return *this;
}

Vector<Quantum<Double> > ImageInfo::restoringBeam() const
{
    return itsRestoringBeam;
}

ImageInfo& ImageInfo::setRestoringBeam(const Vector<Quantum<Double> >& beam)
{
    if (beam.nelements()!=0 && beam.nelements()!=3) {
      throw (AipsError (String("ImageInfo::setRestoringBeam - beam ") +
                         String("vector must be of length 0 or 3")));
    }
//
    if (beam.nelements>0) {
       setRestoringBeam(beam(0), beam(1), beam(2));
    } else {
       itsRestoringBeam.resize(0);
    }
    return *this;
}

ImageInfo& ImageInfo::setRestoringBeam(const Quantum<Double>& major,
                                       const Quantum<Double>& minor,
                                       const Quantum<Double>& pa)
{
   if (major.getValue()<=0.0 || minor.getValue()<=0.0) {
         throw (AipsError (String("ImageInfo::setRestoringBeam - ") +
                         String("the beam size must be positive")));
   }
//
   Unit arcsec("arcsec");
   if (major.getFullUnit()!=arcsec || minor.getFullUnit()!=arcsec ||
       pa.getFullUnit()!=arcsec) {
      throw (AipsError (String("ImageInfo::setRestoringBeam - the beam ") +
             String("units must be angular")));
   }
//
   Double majord = major.getValue(arcsec);
   Double minord = minor.getValue(arcsec);
   if (majord<=minord) {
      throw (AipsError (String("ImageInfo::setRestoringBeam - the major ") +
             String("axis must be greater than the minor axis")));
   }
//
   itsRestoringBeam.resize(3);
   itsRestoringBeam(0) = major;
   itsRestoringBeam(1) = minor;
//
// When I consolidate Gaussian definitions, this
// should be converted to some standard P.A. range
//
   itsRestoringBeam(2) = pa;
//
    return *this;
}

Bool ImageInfo::toRecord(String & error, RecordInterface & outRecord) const
{
    error = "";
    Bool ok = True;
//
// If the beam is null, dont do anything as it will get
// restored as null as well if ist not in the record
//
    if (itsRestoringBeam.nelements()>0) {
       Record restoringBeamRecord;
       Vector<String> names(3);
       names(0) = "major"; names(1) = "minor"; names(2) = "positionangle";
       for (uInt i=0; i<3; i++) {
          QuantumHolder qh(itsRestoringBeam(i));
          Record tmp;
          ok = qh.toRecord(error, tmp);
          if (!ok) return False;
          restoringBeamRecord.defineRecord(names(i), tmp);
       }
       outRecord.defineRecord("restoringbeam", restoringBeamRecord);
    }
//
    return ok;
}

Bool ImageInfo::fromRecord(String & error, const RecordInterface & inRecord)
//
// Returns default object if none in record
//
{
    error = "";
    ImageInfo tmp;
    (*this) = tmp; // Make sure we are "empty" first;
//
    QuantumHolder qh;
    Bool ok;
    if (inRecord.isDefined("restoringbeam")) {
       itsRestoringBeam.resize(3);
       const RecordInterface& subRec = inRecord.asRecord("restoringbeam");
       if (subRec.nfields()!=3) {
          error = "Restoring beam record does not contain 3 fields";
          return False;
       }
//
       if (subRec.isDefined("major")) {
          const RecordInterface& subRec0 = subRec.asRecord("major");
          ok = qh.fromRecord(error, subRec0);
          if (ok) itsRestoringBeam(0) = qh.asQuantumDouble();
       } else {
          error = "Field major missing from restoring beam record";
          ok = False;
       }
       if (!ok) {
          (*this) = tmp;
          return False;
       }
//
       if (subRec.isDefined("minor")) {
          const RecordInterface& subRec1 = subRec.asRecord("minor");
          ok = qh.fromRecord(error, subRec1);
          if (ok) itsRestoringBeam(1) = qh.asQuantumDouble();
       } else {
          error = "Field minor missing from restoring beam record";
          ok = False;
       }
       if (!ok) {
          (*this) = tmp;
          return False;
       }
//
       if (subRec.isDefined("positionangle")) {
          const RecordInterface& subRec2 = subRec.asRecord("positionangle");
          ok = qh.fromRecord(error, subRec2);
          if (ok) itsRestoringBeam(2) = qh.asQuantumDouble();
       } else {
          error = "Field positionangle missing from restoring beam record";
       }
       if (!ok) {
          (*this) = tmp;
          return False;
       }
   }
   return True;
}

ostream &operator<<(ostream &os, const ImageInfo &info)
{
    if (info.restoringBeam().nelements()>0) {
       os << "Restoring beam : " << info.restoringBeam()(0) << ", " 
          << info.restoringBeam()(1) << ", " << info.restoringBeam()(2);
    }
    return os;
}


