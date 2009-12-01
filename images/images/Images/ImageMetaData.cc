//# ImageMetaData.cc: Meta information for Images
//# Copyright (C) 2009
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


#include <casa/aips.h>

#include <images/Images/ImageMetaData.h>

namespace casa { //# NAMESPACE CASA - BEGIN

    Int ImageMetaData::spectralCoordinateNumber() const {
        // don't do a hasSpectralAxis() check or you will go down an infinite recursion path
        return itsCoordinates.findCoordinate(Coordinate::SPECTRAL);
    }

    Bool ImageMetaData::hasSpectralAxis() const {
        Int spectralCoordNum = spectralCoordinateNumber();
        return (spectralCoordNum >= 0 && spectralCoordNum < (Int)itsCoordinates.nCoordinates());
    } 

    Int ImageMetaData::spectralAxisNumber() const {
        if (! hasSpectralAxis()) {
            return -1;
        }
        Int specIndex = itsCoordinates.findCoordinate(Coordinate::SPECTRAL);
        return itsCoordinates.pixelAxes(specIndex)[0];
    }    

    uInt ImageMetaData::nChannels() const {
        if (! hasSpectralAxis()) {
            return 0;
        }
        return itsShape[spectralAxisNumber()];
    }

    Bool ImageMetaData::isChannelNumberValid(const uInt chan) const {
        if (! hasSpectralAxis()) {
            return False;
        }
        return (chan < nChannels());
    }

    Int ImageMetaData::polarizationCoordinateNumber() const {
        // don't do hasPolarizationAxis check or you will go down an infinite recursion path :)
        return itsCoordinates.findCoordinate(Coordinate::STOKES);
    }

    Bool ImageMetaData::hasPolarizationAxis() const {
        Int polarizationCoordNum = polarizationCoordinateNumber();
        return (
            polarizationCoordNum >= 0 
            && polarizationCoordNum < (Int)itsCoordinates.nCoordinates()
        );
    } 

    Int ImageMetaData::polarizationAxisNumber() const {
        if (! hasPolarizationAxis()) {
            return -1;
        }
        return itsCoordinates.pixelAxes(polarizationCoordinateNumber())[0];
    }       

    uInt ImageMetaData::nStokes() const {
        if (! hasPolarizationAxis()) {
            return 0;
        }
        return itsShape[polarizationAxisNumber()];
    }

    Int ImageMetaData::stokesPixelNumber(const String& stokesString) const {
        if (! hasPolarizationAxis()) {
            return -1;
        }
        Int polCoordNum = polarizationCoordinateNumber();
        StokesCoordinate stokesCoord = itsCoordinates.stokesCoordinate(polCoordNum);
        Int stokesPix;
        stokesCoord.toPixel(stokesPix, Stokes::type(stokesString));
        if (stokesPix < 0 || stokesPix >= (Int)nStokes()) {
            return -1;
        }
        return stokesPix;
    }

    Bool ImageMetaData::isStokesValid(const String& stokesString) const {
        if (! hasPolarizationAxis()) {
            return False;
        }
        Int stokesPixNum = stokesPixelNumber(stokesString);
        return stokesPixNum >= 0 && stokesPixNum < (Int)nStokes(); 
    }

    Int ImageMetaData::directionCoordinateNumber() const {
        // don't do a hasDirectionCoordinate() check or you will go down an infinite recursion path
        return itsCoordinates.findCoordinate(Coordinate::DIRECTION);
    }

    Bool ImageMetaData::hasDirectionCoordinate() const {
        Int directionCoordNum = directionCoordinateNumber();
        return (
            directionCoordNum >= 0 
            && directionCoordNum < (Int)itsCoordinates.nCoordinates()
        );
    } 

    Vector<Int> ImageMetaData::directionAxesNumbers() const {
        if (! hasDirectionCoordinate()) {
          return Vector<Int>();
        }
        return itsCoordinates.pixelAxes(directionCoordinateNumber());
    }    

    Vector<Int> ImageMetaData::directionShape() const {
        Vector<Int> dirAxesNums = directionAxesNumbers();
        if (dirAxesNums.nelements() == 0) {
            return Vector<Int>();
        }
        Vector<Int> dirShape(2);
        dirShape[0] = itsShape[dirAxesNums[0]];
        dirShape[1] = itsShape[dirAxesNums[1]];
        return dirShape;
    }

    Bool ImageMetaData::areChannelAndStokesValid(
        String& message, const uInt chan, const String& stokesString
    ) const {
        ostringstream os;
        Bool areValid = True;
        if (! isChannelNumberValid(chan)) {
            os << "Zero-based channel number " << chan << " is too large. There are only "
                << nChannels() << " spectral channels in this image.";
            areValid = False;
        }    
        if (! isStokesValid(stokesString)) {
            if (! areValid) {
                os << " and ";
            }
            os << "Stokes parameter " << stokesString << " is not in image";
            areValid = False;
        }
        if (! areValid) {
            message = os.str();
        }    
        return areValid;
    }
} //# NAMESPACE CASA - END

