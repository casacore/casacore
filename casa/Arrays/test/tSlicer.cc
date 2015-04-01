//# tSlicer.cc: This program tests the Slicer class
//# Copyright (C) 1994,1995,1996,1999,2000,2001
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
//#
//# $Id$

//# Includes

#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary> Test the Slicer class </summary>

//# Forward Declarations

void a();

int main()
{
    try {
	a();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void a()
{
    // The following (outcommented) constructor results in a compile
    // error, because this private constructor prevents an automatic
    // conversion of Int to IPosition.
    //# Slicer xxx(0);

    // Define the shape of an array.
    // Also define an origin.
    IPosition shape(2,20,30);
    IPosition origin(2,-5,15);
    // Now define some Slicer's and apply the shape and/or origin.
    IPosition blc,trc,inc;
    Slicer ns0 (IPosition(2,0,24));
    cout << ns0.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns0.inferShapeFromSource (shape, origin, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns1 (IPosition(2,3,5), IPosition(2,13,21),
		Slicer::endIsLast);
    cout << ns1.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns2 (IPosition(2,3,5), IPosition(2,13,21), IPosition(2,3,2),
		Slicer::endIsLast);
    cout << ns2.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns2.ndim() << ns2.start() << ns2.end() << ns2.stride()
	 << ns2.length() << endl;
    cout << ns2 << endl;

    // Define some Slicer's via an output length.
    Slicer ns10 (IPosition(2,0,24));
    cout << ns10.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns10.inferShapeFromSource (shape, origin, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns11 (IPosition(2,3,5), IPosition(2,13,21));
    cout << ns11.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns12 (IPosition(2,3,5), IPosition(2,4,11), IPosition(2,3,2));
    cout << ns12.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns12.ndim() << ns12.start() << ns12.end() << ns12.stride()
	 << ns12.length() << endl;

    // Define some Slicer's with an undetermined blc and/or trc.
    Slicer ns20 (IPosition(2,Slicer::MimicSource,24));
    cout << ns20.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns20.inferShapeFromSource (shape, origin, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns21 (IPosition(2,3,5), IPosition(2,13,Slicer::MimicSource),
		 Slicer::endIsLast);
    cout << ns21.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns22 (IPosition(2,Slicer::MimicSource,5), IPosition(2,13,21),
		 IPosition(2,3,2), Slicer::endIsLast);
    cout << ns22.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns22.ndim() << ns22.start() << ns22.end() << ns22.stride()
	 << ns22.length() << endl;

    // Define some Slicer's with an undetermined blc and/or length.
    Slicer ns30 (IPosition(2,Slicer::MimicSource,24));
    cout << ns30.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns30.inferShapeFromSource (shape, origin, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns31 (IPosition(2,3,5), IPosition(2,13,Slicer::MimicSource));
    cout << ns31.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns32 (IPosition(2,Slicer::MimicSource,5), IPosition(2,5,11),
		 IPosition(2,3,2));
    cout << ns32.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns32.ndim() << ns32.start() << ns32.end() << ns32.stride()
	 << ns32.length() << endl;

    Slicer ns40 (Slice(0), Slice(24));
    cout << ns40.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns40.inferShapeFromSource (shape, origin, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns41 (Slice(3,13), Slice(5,21));
    cout << ns41.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    Slicer ns42 (Slice(3,5,3), Slice(5,11,2));
    cout << ns42.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns42.ndim() << ns42.start() << ns42.end() << ns42.stride()
	 << ns42.length() << endl;
    Slice tmp1, tmp2(5,11,2);
    Slicer ns43(tmp1, tmp2);
    cout << ns43.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns43.ndim() << ns43.start() << ns43.end() << ns43.stride()
	 << ns43.length() << endl;

    // Try length 0.
    Slicer ns50 (Slice(0,5,2), Slice(24,0,3));
    cout << ns50.inferShapeFromSource (shape, blc,trc,inc)
	 << blc<<trc<<inc << endl;
    cout << ns50.ndim() << ns50.start() << ns50.end() << ns50.stride()
	 << ns50.length() << endl;

    // Try copy constructor.
    Slicer ns3(ns2);
    cout << ns3.ndim() << ns3.start() << ns3.end() << ns3.stride()
	 << ns3.length() << endl;
    // Try assignment (the difference in ndim should work fine).
    Slicer ns4(IPosition(1,0));
    cout << ns4.ndim() << endl;
    ns4 = ns3;
    cout << ns4.ndim() << ns4.start() << ns4.end() << ns4.stride()
	 << ns4.length() << endl;

    // Try equality
    cout << (ns4==ns3) << endl;
    cout << (ns4==ns50) << endl;

    // Do some erronous constructions.
    try {
	Slicer ns(IPosition(2,0,0), IPosition(3,0,0,0));
    }catch (AipsError x) {                   // different lengths
	cout << x.getMesg() << endl;
    } 
    try {
	Slicer ns(IPosition(2,0,0), IPosition(3,0,0,0), Slicer::endIsLast);
    }catch (AipsError x) {                   // different lengths
	cout << x.getMesg() << endl;
    } 
    try {
	Slicer ns(IPosition(2,0,1), IPosition(2,0,0), Slicer::endIsLast);
    }catch (AipsError x) {                   // trc < blc
	cout << x.getMesg() << endl;
    } 
    try {
	Slicer ns(IPosition(2,0,1), IPosition(2,0,-1), Slicer::endIsLength);
    }catch (AipsError x) {                   // length < 0
	cout << x.getMesg() << endl;
    } 
    try {
	Slicer ns(IPosition(2,0,1), IPosition(2,1,1), IPosition(2,-1,0));
    }catch (AipsError x) {                   // inc < 0
	cout << x.getMesg() << endl;
    } 
    try {
	Slicer ns(IPosition(2,0,1), IPosition(2,1,1), IPosition(2,0,0));
    }catch (AipsError x) {                   // inc < 0
	cout << x.getMesg() << endl;
    } 

    // Check if changing length of trc,blc,inc works fine.
    Slicer ns90;
    cout << ns90.inferShapeFromSource (IPosition(1,10), blc, trc, inc)
	 << blc << trc << inc << endl;
    cout << ns90.ndim() << ns90.start() << ns90.end() << ns90.stride()
	 << ns90.length() << endl;
    // Do some erronous infers.
    try {
	ns90.inferShapeFromSource (shape, blc, trc, inc);
    }catch (AipsError x) {                   // shape length invalid
	cout << x.getMesg() << endl;
    } 
    try {
	ns90.inferShapeFromSource (IPosition(1,10), origin, blc, trc, inc);
    }catch (AipsError x) {                   // origin length invalid
	cout << x.getMesg() << endl;
    } 

    // Do some erronous infers resulting in too small or large blc/trc.
    // The correct ones are just on the edge.
    Slicer ns91 (IPosition(1,-10), IPosition(1,Slicer::MimicSource));
    ns91.inferShapeFromSource (IPosition(1,100), IPosition(1,-10),
			       blc, trc, inc);
    try {
	ns91.inferShapeFromSource (IPosition(1,100), IPosition(1,-9),
				   blc, trc, inc);
    }catch (AipsError x) {                   // blc < 0
	cout << x.getMesg() << endl;
    } 
    ns91.inferShapeFromSource (IPosition(1,10), IPosition(1,-19),
			       blc, trc, inc);
    try {
	ns91.inferShapeFromSource (IPosition(1,10), IPosition(1,-20),
				   blc, trc, inc);
    }catch (AipsError x) {                   // blc > shp
	cout << x.getMesg() << endl;
    } 

    Slicer ns92 (IPosition(1,Slicer::MimicSource), IPosition(1,-10),
		 Slicer::endIsLast);
    ns92.inferShapeFromSource (IPosition(1,10), IPosition(1,-19),
			       blc, trc, inc);
    try {
	ns92.inferShapeFromSource (IPosition(1,10), IPosition(1,-20),
				   blc, trc, inc);
    }catch (AipsError x) {                   // trc > shp
	cout << x.getMesg() << endl;
    } 
    ns92.inferShapeFromSource (IPosition(1,10),IPosition(1,-9),blc,trc,inc);
    try {
	ns92.inferShapeFromSource (IPosition(1,10), IPosition(1,-8),
				   blc, trc, inc);
    }catch (AipsError x) {                   // trc < blc-1
	cout << x.getMesg() << endl;
    } 
    
}
