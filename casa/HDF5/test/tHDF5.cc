//# tHDF5.cc: Test program for HDF5 performance of tiling
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

#include <iostream>

#ifndef HAVE_HDF5
int main()
{
  std::cout << "HDF5 not configured in, so cannot run tHDF5" << std::endl;
  return 0;
}

#else

#include <vector>
#include <math.h>
#include <hdf5.h>
#include <stdlib.h>

using namespace std;

// Quick and dirty.
// Define some global hid variables.
hid_t fileId=-1;     // file
hid_t setId=-1;      // dataset
hid_t dsId=-1;       // dataspace
hid_t plId=-1;       // dataset property list
hid_t daplId=-1;     // dataset access
hid_t typeIdMem=-1;
hid_t typeIdFile=-1;

template<typename T>
void check (T v, const char* message)
{
  if (v < 0) {
    cerr << message << endl;
    exit(1);
  }
}

void createFile()
{
  // Use 8 byte offets and blocks of 32768 bytes.
  hid_t create_plist = H5Pcreate(H5P_FILE_CREATE);
  check (create_plist, "Failed to create creation plist");
  check (H5Pset_sizes(create_plist, 8, 8), "Failed to set offset size");
  check (H5Pset_userblock(create_plist, 32768), "Failed to set block size");
  // Use unbuffered IO.
  hid_t access_plist = H5Pcreate(H5P_FILE_ACCESS);
  check (access_plist, "Failed to create access plist");
  check (H5Pset_fapl_sec2(access_plist), "Failed to set access mode");
  // Create the file.
  fileId = H5Fcreate ("tHDF5_tmp.dat", H5F_ACC_TRUNC,
		      create_plist, access_plist);
  check (fileId, "Failed to create the file");
  H5Pclose (create_plist);
  H5Pclose (access_plist);
}

void openFile()
{
  // Use unbuffered IO.
  hid_t access_plist = H5Pcreate(H5P_FILE_ACCESS);
  check (access_plist, "Failed to create access plist");
  check (H5Pset_fapl_sec2(access_plist), "Failed to set access mode");
  fileId = H5Fopen ("tHDF5_tmp.dat", H5F_ACC_RDONLY, access_plist);
  check (fileId, "Failed to create the file");
  H5Pclose (access_plist);
}

void closeFile()
{
  if (fileId >= 0) H5Fclose (fileId);
  fileId = -1;
}

void createDataSet (int nx, int ny, int nz, int ntx, int nty, int ntz)
{
  // Create hids for the data type on disk and in memory.
  typeIdFile = H5Tcopy (H5T_NATIVE_FLOAT);
  check (typeIdFile, "Failed to create file datatype id");
  typeIdMem  = H5Tcopy (typeIdFile);
  check (typeIdMem, "Failed to create memory datatype id");
  // Create access property for later setting of cache size.
  daplId = H5Pcreate (H5P_DATASET_ACCESS);
  check (daplId, "Failed to create dataset access property list");
  // Create the data space for the array.
  int rank = 3;
  hsize_t shp[3];
  shp[0] = nz;
  shp[1] = ny;
  shp[2] = nx;
  dsId = H5Screate_simple(rank, shp, NULL);
  check (dsId, "Failed to create dataspace");
  // Create the properties to hold the chunk shape.
  plId = H5Pcreate (H5P_DATASET_CREATE);
  check (plId, "Failed to create dataset property list");
  hsize_t chunkShp[3];
  chunkShp[0] = ntz;
  chunkShp[1] = nty;
  chunkShp[2] = ntx;
  H5Pset_chunk (plId, rank, chunkShp);
  // Create the data set.
  setId = H5Dcreate2 (fileId, "dataset", typeIdFile,
		      dsId, 0, plId, 0);
  check (setId, "Failed to create dataset");
}

void openDataSet()
{
  // Create hids for the data type on disk and in memory.
  typeIdFile = H5Tcopy (H5T_NATIVE_FLOAT);
  typeIdMem  = H5Tcopy (typeIdFile);
  // Open the dataset.
  setId = H5Dopen2 (fileId, "dataset", 0);
  check (setId, "Failed to open dataset");
  // Get the data space (for the shape).
  dsId = H5Dget_space(setId);
  check (dsId, "Failed to get dataspace");
  // Create access property for later setting of cache size.
  daplId = H5Pcreate (H5P_DATASET_ACCESS);
  check (daplId, "Failed to create dataset access property list");
}

void closeDataSet()
{
  if (setId >= 0) H5Dclose (setId);
  setId = -1;
  if (plId >= 0) H5Pclose (plId);
  plId = -1;
  if (dsId >= 0) H5Sclose (dsId);
  dsId = -1;
  if (daplId >= 0) H5Pclose (daplId);
  daplId = -1;
  if (typeIdFile >= 0) H5Tclose (typeIdFile);
  typeIdFile = -1;
  if (typeIdMem >= 0) H5Tclose (typeIdMem);
  typeIdMem = -1;
}

bool isPrime (int v)
{
  if (v%2 == 0) return false;
  // Very simple implementation, but suffices.
  int iend = int(sqrt(float(v)));
  for (int i=3; i<=iend; i+=2) {
    if (v%i == 0) return false;
  }
  return true;
}

void setCacheSize (int nchunks, int nbytes)
{
  int nhash = 100*nchunks+1;
  while (!isPrime(nhash)) {
    nhash += 2;
  }
  cout << "setting cache to " << nchunks << " chunks (" << nbytes
       << " bytes) with " << nhash << " slots"
       << endl;
  // Setting the cache size takes only effect when opening the dataset.
  // So close it first.
  H5Dclose (setId);
  // Use LRU caching (4th argument is 0).
  check (H5Pset_chunk_cache (daplId, nhash, nbytes, 0.),
         "Failed to set chunk cache");
  // Reopen the dataset with cache size in daplId.
  setId = H5Dopen2 (fileId, "dataset", daplId);
  check (setId, "Failed to reopen file");
}

void get (int stx, int sty, int stz, int nx, int ny, int nz, void* buf)
{
  // Define the data set selection.
  hsize_t offset[3], count[3], stride[3];
  offset[0] = stz; count[0] = nz; stride[0] = 1;
  offset[1] = sty; count[1] = ny; stride[1] = 1;
  offset[2] = stx; count[2] = nx; stride[2] = 1;
  check (H5Sselect_hyperslab (dsId, H5S_SELECT_SET, offset,
                              stride, count, NULL),
         "invalid data set array selection");
  // Define a data space for the memory buffer.
  hid_t dsmemid = H5Screate_simple (3, count, NULL);
  check (dsmemid, "Failed to create memory data space");
  // Define memory selection.
  offset[0]=0; offset[1]=0; offset[2] = 0;
  check (H5Sselect_hyperslab (dsmemid, H5S_SELECT_SET, offset,
                              NULL, count, NULL),
         "Failed to set slab of memory buffer");
  // Read the data.
  check (H5Dread (setId, typeIdMem, dsmemid, dsId,
                  H5P_DEFAULT, buf),
         "Failed to read slab from data set array");
  H5Sclose (dsmemid);
}

void put (int stx, int sty, int stz, int nx, int ny, int nz, const void* buf)
{
  // Define the data set selection.
  hsize_t offset[3], count[3], stride[3];
  offset[0] = stz; count[0] = nz; stride[0] = 1;
  offset[1] = sty; count[1] = ny; stride[1] = 1;
  offset[2] = stx; count[2] = nx; stride[2] = 1;
  check (H5Sselect_hyperslab (dsId, H5S_SELECT_SET, offset,
                              stride, count, NULL),
         "invalid data set array selection");
  // Define a data space for the memory buffer.
  hid_t dsmemid = H5Screate_simple (3, count, NULL);
  check (dsmemid, "Failed to create memory data space");
  // Define memory selection.
  offset[0]=0; offset[1]=0; offset[2] = 0;
  check (H5Sselect_hyperslab (dsmemid, H5S_SELECT_SET, offset,
                              NULL, count, NULL),
         "Failed to set slab of memory buffer");
  // Write the data.
  check (H5Dwrite (setId, typeIdMem, dsmemid, dsId,
                   H5P_DEFAULT, buf),
         "Failed to write slab into data set array");
  H5Sclose (dsmemid);
}

void create (int nx, int ny, int nz, int ntx, int nty, int ntz)
{
  // Create the file and data set.
  createFile();
  createDataSet (nx, ny, nz, ntx, nty, ntz);
  // Fill the data set chunk by chunk.
  int sz = ntx*nty*ntz;
  vector<float> buf(sz);
  for (int iz=0; iz<nz; iz+=ntz) {
    for (int iy=0; iy<ny; iy+=nty) {
      for (int ix=0; ix<nx; ix+=ntx) {
        int i=0;
        for (int jz=0; jz<ntz; ++jz) {
          for (int jy=0; jy<nty; ++jy) {
            int val = ((iz+jz)*ny + iy+jy)*nx + ix;
            for (int jx=0; jx<ntx; ++jx) {
              buf[i] = val+jx;
              ++i;
            }
          }
        }
        put (ix, iy, iz, ntx, nty, ntz, &buf[0]);
      }
    }
  }
}

void readTiles (int nx, int ny, int nz, int ntx, int nty, int ntz, bool checkv)
{
  // Read the data back.
  int sz = ntx*nty*ntz;
  vector<float> buf(sz);
  for (int iz=0; iz<nz; iz+=ntz) {
    for (int iy=0; iy<ny; iy+=nty) {
      for (int ix=0; ix<nx; ix+=ntx) {
        get (ix, iy, iz, ntx, nty, ntz, &buf[0]);
        if (checkv) {
          // Check values.
          int i=0;
          for (int jz=0; jz<ntz; ++jz) {
            for (int jy=0; jy<nty; ++jy) {
              int val = ((iz+jz)*ny + iy+jy)*nx + ix;
              for (int jx=0; jx<ntx; ++jx) {
                if (buf[i] != val) {
                  cerr << "Expected " << val << " but read " << buf[i]<< endl;
                }
                ++i;
                ++val;
              }
            }
          }
        }
      }
    }
  }
}

void readX (int nx, int ny, int nz, int ntx, int nty, int ntz, bool checkv)
{
  // First set the chunk cache.
  int nchunks = nx/ntx;
  int nbytes = nchunks*ntx*nty*ntz*sizeof(float);
  setCacheSize (nchunks, nbytes);
  // Read the data back (vector by vector in tile order).
  vector<float> buf(nx);
  for (int iz=0; iz<nz; iz+=ntz) {
    for (int iy=0; iy<ny; iy+=nty) {
      for (int jz=0; jz<ntz; ++jz) {
        for (int jy=0; jy<nty; ++jy) {
          get (0, iy+jy, iz+jz, nx, 1, 1, &buf[0]);
          if (checkv) {
            // Check the values.
            int val = ((iz+jz)*ny + iy+jy)*nx;
            for (int i=0; i<nx; ++i) {
              if (buf[i] != val) {
                cerr << "Expected " << val << " but read " << buf[i] << endl;
              }
              val++;
            }
          }
        }
      }
    }
  }
}

void readY (int nx, int ny, int nz, int ntx, int nty, int ntz, bool checkv)
{
  // First set the chunk cache.
  int nchunks = ny/nty;
  int nbytes = nchunks*ntx*nty*ntz*sizeof(float);
  setCacheSize (nchunks, nbytes);
  // Read the data back (vector by vector in tile order).
  vector<float> buf(ny);
  for (int iz=0; iz<nz; iz+=ntz) {
    for (int ix=0; ix<nx; ix+=ntx) {
      for (int jz=0; jz<ntz; ++jz) {
        for (int jx=0; jx<ntx; ++jx) {
          get (ix+jx, 0, iz+jz, 1, ny, 1, &buf[0]);
          if (checkv) {
            // Check the values.
            int val = (iz+jz)*ny*nx + ix+jx;
            for (int i=0; i<ny; ++i) {
              if (buf[i] != val) {
                cerr << "Expected " << val << " but read " << buf[i] << endl;
              }
              val += nx;
            }
          }
        }
      }
    }
  }
}

void readZ (int nx, int ny, int nz, int ntx, int nty, int ntz, bool checkv)
{
  // First set the chunk cache.
  int nchunks = nz/ntz;
  int nbytes = nchunks*ntx*nty*ntz*sizeof(float);
  setCacheSize (nchunks, nbytes);
  // Read the data back (vector by vector in tile order).
  vector<float> buf(nz);
  for (int iy=0; iy<ny; iy+=nty) {
    for (int ix=0; ix<nx; ix+=ntx) {
      for (int jy=0; jy<nty; ++jy) {
        for (int jx=0; jx<ntx; ++jx) {
          get (ix+jx, iy+jy, 0, 1, 1, nz, &buf[0]);
          if (checkv) {
            // Check the values.
            int val = (iy+jy)*nx + ix+jx;
            for (int i=0; i<nz; ++i) {
              if (buf[i] != val) {
                cerr << "Expected " << val << " but read " << buf[i] << endl;
              }
              val += nx*ny;
            }
          }
        }
      }
    }
  }
}

void readback (int nx, int ny, int nz, int ntx, int nty, int ntz,
               char type, bool checkv)
{
  openFile();
  openDataSet();
  if (type == 'x') {
    readX (nx, ny, nz, ntx, nty, ntz, checkv);
  } else if (type == 'y') {
    readY (nx, ny, nz, ntx, nty, ntz, checkv);
  } else if (type == 'z') {
    readZ (nx, ny, nz, ntx, nty, ntz, checkv);
  } else {
    readTiles (nx, ny, nz, ntx, nty, ntz, checkv);
  }
}

int main(int argc, char* argv[])
{
  if (argc <= 6) {
    cerr << "Run as:   tHDF5 nx ny nz ntx nty ntz         to create" << endl;
    cerr << "or        tHDF5 nx ny nz ntx nty ntz type ch to read back" << endl;
    cerr << "  type x,y or z   read vectors along that axis" << endl;
    cerr << "       else       read tile by tile" << endl;
    cerr << "  ch 1 means check the values read; else is no checking" << endl;
    exit(0);
  }
  int nx = atoi(argv[1]);
  int ny = atoi(argv[2]);
  int nz = atoi(argv[3]);
  int ntx = atoi(argv[4]);
  int nty = atoi(argv[5]);
  int ntz = atoi(argv[6]);
  if (argc <= 7) {
    cout << "Creating tHDF5_tmp.dat with shape [" << nx<<','<<ny<<','<<nz
         << "] and chunk shape [" << ntx <<',' << nty << ',' << ntz << ']'
         << endl;
    create (nx, ny, nz, ntx, nty, ntz);
  } else {
    bool checkv = (argc > 8  &&  argv[8][0] == '1');
    readback (nx, ny, nz, ntx, nty, ntz, argv[7][0], checkv);
  }
  exit(0);
}

#endif
