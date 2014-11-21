//# tLatticePerf.cc: Test performance of lattices
//# Copyright (C) 2010
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

#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/HDF5Lattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TileStepper.h>
#include <casacore/lattices/Lattices/TiledLineStepper.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for performance of PagedArray and HDF5Lattice
// </summary>


// Create the lattice cube.
void makeCube (bool useHDF, const IPosition& cubeShape,
               const IPosition& tileShape)
{
  TiledShape tshape(cubeShape, tileShape);
  Lattice<Float>* lattice = 0;
  if (useHDF) {
    cout << "Creating tLatticePerf_tmp.hdf with shape " << cubeShape
         << " and tile shape " << tileShape << endl;
    cout << "HDF5    ";
    lattice = new HDF5Lattice<Float>(tshape, "tLatticePerf_tmp.hdf");
  } else {
    cout << "Creating tLatticePerf_tmp.tab with shape " << cubeShape
         << " and tile shape " << tileShape << endl;
    cout << "CCTS    ";
    lattice = new PagedArray<Float> (tshape, "tLatticePerf_tmp.tab");
  }
  Timer timer;
  lattice->set (0);
  delete lattice;
  timer.show ("create  ");
}

void getLine (const Lattice<Float>& lattice, uInt axis)
{
  Timer timer;
  TiledLineStepper nav(lattice.shape(), lattice.niceCursorShape(), axis);
  RO_LatticeIterator<Float> iter(lattice, nav);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.cursor();
  }
  timer.show ("getLine ");
}

void getPlane (const Lattice<Float>& lattice, uInt nonAxis)
{
  Timer timer;
  IPosition cursorShape = lattice.shape();
  cursorShape[nonAxis] = 1;
  LatticeStepper nav(lattice.shape(), cursorShape);
  RO_LatticeIterator<Float> iter(lattice, nav);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.cursor();
  }
  timer.show ("getPlane");
}

void getTiles (const Lattice<Float>& lattice)
{
  Timer timer;
  TileStepper nav(lattice.shape(), lattice.niceCursorShape());
  RO_LatticeIterator<Float> iter(lattice, nav);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.cursor();
  }
  timer.show ("getTiles");
}

void getCube (const Lattice<Float>& lattice, const String& trav)
{
  if (trav == "x") {
    cout << "x  ";
    getLine (lattice, 0);
  } else if (trav == "y") {
    cout << "y  ";
    getLine (lattice, 1);
  } else if (trav == "z") {
    cout << "z  ";
    getLine (lattice, 2);
  } else if (trav == "xy") {
    cout << "xy ";
    getPlane (lattice, 2);
  } else if (trav == "xz") {
    cout << "xz ";
    getPlane (lattice, 1);
  } else if (trav == "yz") {
    cout << "yz ";
    getPlane (lattice, 0);
  } else {
    cout << "   ";
    getTiles (lattice);
  } 
}

int main (int argc, char* argv[])
{
  if (argc <= 1) {
    cerr << "Run as:  tLatticePerf nx ny nz ntx nty ntz [hdf5]   to create"
         << endl;
    cerr << "or       tLatticePerf type [hdf5]                   to read back"
         << endl;
    cerr << "  hdf5  1          use HDF5Lattice<Float>" <<endl;
    cerr << "        else       use PagedArray<Float> (is default)" << endl;
    cerr << "  type  x,y,z      read vectors along this axis" << endl;
    cerr << "        xy,xz,yz   read planes along these axes" << endl;
    cerr << "        else       read tile by tile" << endl;
    exit(0);
  }
  try {
    if (argc > 6) {
      IPosition cubeShape(3, atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
      IPosition tileShape(3, atoi(argv[4]), atoi(argv[5]), atoi(argv[6]));
      Bool useHDF = (argc > 7  &&  argv[7][0] == '1');
      makeCube (useHDF, cubeShape, tileShape);
    } else {
      Bool useHDF = (argc > 2  &&  argv[2][0] == '1');
      if (useHDF) {
        cout << "HDF5 ";
        getCube (HDF5Lattice<Float>("tLatticePerf_tmp.hdf"), argv[1]);
      } else {
        cout << "CCTS ";
        getCube (PagedArray<Float>("tLatticePerf_tmp.tab"), argv[1]);
      }
    }
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
