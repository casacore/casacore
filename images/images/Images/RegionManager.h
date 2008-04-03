//# RegionManager.h: framework independent class that provides 
//# functionality to tool of same name
//# Copyright (C) 2007
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef IMAGES_REGIONMANAGER_H
#define IMAGES_REGIONMANAGER_H

#include <lattices/Lattices/RegionType.h>


//# put includes here


namespace casa {

  //# Forward declarations
  class LogIO;
  class String;
  class Record;
  template<class T> class Vector;
  class CoordinateSystem;
  class WCRegion;
  class WCUnion;
  template<class T> class PtrBlock;
  class ImageRegion;
  template<class T> class Quantum;

  // image component class 
  //
  // This is a casa based class to provide the funtionality to the 
  // RegionManager Tool
  //
  // @author
  // @version 

  class RegionManager
    {
      
      
    public:
      //blank constructor
      RegionManager();
      RegionManager(CoordinateSystem& csys);
      virtual ~RegionManager();
      String absreltype(const Int absrelval=0);
      //LCSlicer box
      Record* box(const Vector<Double>& blc, const Vector<Double>& trc, 
		  const Vector<Double>& inc, const String& absrel, 
		  Bool frac, const String& comment="");
      //LCBox box
      Record* box(const Vector<Double>& blc, const Vector<Double>& trc, 
		  const Vector<Int>& shape, const String& comment="");
      Record* wbox(const Vector<Quantity>& blc, 
			const Vector<Quantity>& trc, 
			const Vector<Int>& pixelaxes, 
			const CoordinateSystem& csys,
			const String& absrel, const String& comment);
      Record* wbox(const Vector<Quantity>& blc, 
			const Vector<Quantity>& trc, 
			const Vector<Int>& pixelaxes, 
			const String& absrel, const String& comment);
      ImageRegion* wbox(const Vector<Quantity>& blc, 
			const Vector<Quantity>& trc, 
			const Vector<Int>& pixelaxes, 
			const CoordinateSystem& csys,
			const String& absrel="abs" );
      //Wpolygon with coordsys and if pixelaxes[0] is -1 then its assumed
      //to be 0,1,...
      ImageRegion* wpolygon(const Vector<Quantity>& x, 
			    const Vector<Quantity>& y, 
			    const Vector<Int>& pixelaxes, 
			    const CoordinateSystem& csys, 
			    const String& absrel);
      //wpolygon version without csys...throws an exception if 
      //setcoordsys is not run
      ImageRegion* wpolygon(const Vector<Quantity>& x, 
			    const Vector<Quantity>& y, 
			    const Vector<Int>& pixelaxes,  
			    const String& absrel);
      //Different versions of unioning regions
      ImageRegion*  doUnion(const PtrBlock<const WCRegion*>& reg1);
      ImageRegion*  doUnion(const WCRegion& reg1, const WCRegion& reg2);
      ImageRegion*  doUnion(const ImageRegion& reg1, const ImageRegion& reg2);
      void setcoordsys(const CoordinateSystem& csys);

    private:
      RegionType::AbsRelType regionTypeFromString(const String& absreltype);
      LogIO *itsLog;
      CoordinateSystem* itsCSys;
      
    };
} // casa namespace
#endif

