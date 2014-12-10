//# FITSReader.cc: Parse a FITS disk file.
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2001,2003
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


#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/fits/FITS/FITSReader.h>

namespace casacore {

// Display basic info and the keyword list
void showHDU(HeaderDataUnit *h) {
    LogIO os;
    os << LogOrigin("FITSReader", "showHDU", WHERE)
       << LogIO::DEBUGGING
       << "Data type   " << h->datatype() << "\n"
       << "Data size   " << (uInt)(h->fitsdatasize()) << "\n"
       << "Dimensions  " << h->dims() << "\n"
       << LogIO::POST;
    for (int n = 0; n < h->dims(); n++) {
       os << LogOrigin("FITSReader", "showHDU", WHERE)
          << LogIO::NORMAL5
          << "Axis " << (n + 1) << " size "
          << h->dim(n) << LogIO::POST;
     }
     //os << LogOrigin("FITSReader", "showHDU", WHERE)
     //   << LogIO::NORMAL << "Keyword List:" << LogIO::POST;

     //ostringstream oss;
     //oss << *h << '\n';
     //os << LogOrigin("FITSReader", "showHDU", WHERE)
     //   << LogIO::NORMAL << String(oss) << '\n';

     //FitsKeywordList *k = new FitsKeywordList(h->kwlist());
     //k->first();
     //ostringstream oss;
     //FitsKeyword *x = k->next();
     //for (int i = 1; x != 0; ++i, x = k->next())
     //   oss << /*i << ". " <<*/ *x;
     //os << LogOrigin("FITSReader", "showHDU", WHERE)
     //   << LogIO::NORMAL << String(oss) << '\n';

     h->firstkw();
     ostringstream oss;
     for (const FitsKeyword *x = h->nextkw(); x != 0; 
          x = h->nextkw()) {
        int m = 0;
        if (x->kw().name() == FITS::ERRWORD) {
            oss << "ERROR!";
        }
        else {
           oss << x->name();
           m = String(x->name()).length(); 
           if (x->index() != 0) {
              oss << x->index();
              m += String::toString(x->index()).length();
           }
           int n;
           Complex c; 
           DComplex y; 
           IComplex z;
              if (x->value()) {
              for (int i = 0; i < 8 - m; i++) 
                  oss << ' '; 
              oss << "= ";
                    switch (x->type()) {
                 case FITS::NOVALUE: break;
                 case FITS::LOGICAL: 
                    oss.width(22);
                    oss << right;
                    oss << ((*((Bool *)x->value()) == True) ? "T" : "F"); break;
                 case FITS::BIT: 
                    oss.width(22);
                    oss << right;
                    oss << "*****"; break;
                 case FITS::CHAR: 
                    oss.width(22);
                    oss << right;
                    oss << *((char *)x->value()); break;
                 case FITS::BYTE: 
                    oss.width(22);
                    oss << right;
                    n = *((uChar *)x->value()); oss << n; break;
                 case FITS::SHORT: 
                    oss.width(22);
                    oss << right;
                    oss << *((short *)x->value()); break;
                 case FITS::LONG: 
                    oss.width(22);
                    oss << right;
                    oss << *((FitsLong *)x->value()); break;
                 case FITS::STRING: 
                    oss << left;
                    oss << "\'" << (char*)x->value() << "\'"; 
                    break;
                 case FITS::FLOAT: 
                    oss.width(22);
                    oss << right;
                    oss << *((float *)x->value()); break;
                 case FITS::DOUBLE: 
                    oss.width(22);
                    oss << right;
                    oss << *((double *)x->value()); break;
                 case FITS::COMPLEX: 
                    oss.width(22);
                    oss << right;
                    c = *(Complex *)x->value();
                    oss << "(" << c.real() << "," << c.imag() << ")"; break;
                 case FITS::DCOMPLEX: 
                    y = *(DComplex *)x->value();
                    oss.width(22);
                    oss << right;
                    oss << "(" << y.real() << "," << y.imag() << ")"; break;
                 case FITS::ICOMPLEX: 
                    z = *(IComplex *)x->value();
                    oss.width(22);
                    oss << right;
                    oss << "(" << z.real() << "," << z.imag() << ")"; break;
                 default: oss << "*****"; break;
                 }
              if (x->commlen())
                 oss << " /" << x->comm() ;
              oss << "\n";
          }
          else {
             if (x->commlen())
                oss << ' ' << x->comm() ;
             oss << "\n";
         }
      }
   }
   os << LogOrigin("FITSReader", "showHDU", WHERE)
      << LogIO::NORMAL << String(oss)
      << LogIO::POST;
     
}

// Read the data in a Primary Group and display the first few groups
#define DOGROUP(Z) void showPrimaryGroup(PrimaryGroup<Z> &x) { \
   LogIO os; \
   int i, j; \
   int ngroup_to_display = 2; \
   int nele_to_display = 6; \
   showHDU(&x); \
   if (x.err() != HeaderDataUnit::OK) { \
       os << LogOrigin("FITSReader", "showPrimaryGroup", WHERE) \
          << LogIO::SEVERE \
          << "Error occured during construction process" \
          << LogIO::POST; \
   } \
   os << LogOrigin("FITSReader", "showPrimaryGroup", WHERE) \
      << LogIO::NORMAL \
      << x.gcount() << " groups total, display first " << nele_to_display \
      << " elements of the first " << ngroup_to_display << " groups\n"; \
   for (i = 0; i < x.gcount(); ++i) { \
       x.read(); \
       if (i < ngroup_to_display) { \
      os << "Group " << i << " parms: " ; \
      for (j = 0; j < x.pcount(); ++j) \
          os << " " << x.parm(j); \
      os << "\n"; \
      os << "Group " << i << " data: " ; \
      for (j = 0; j < 3 * nele_to_display ; j++) \
          os << " " << x(j); \
      os << "... \n"; \
       } \
   } \
   os << LogIO::POST; \
   delete &x; \
}

// Read the data in a Primary Array and display the first few data points
#define DOARRAY(Z) void showPrimaryArray(PrimaryArray<Z> &x) { \
   LogIO os; \
   int i, j, n0, n1; \
   if (x.fitsdatasize()){ \
       int ne = x.fitsdatasize();  \
       x.read( ne ); \
       } \
   showHDU(&x); \
   if (x.err() != HeaderDataUnit::OK) { \
      os << LogOrigin("FITSReader", "showPrimaryArray", WHERE) \
         << LogIO::SEVERE \
         << "Error occured during construction process" \
         << LogIO::POST; \
   } \
   os << LogOrigin("FITSReader", "showPrimaryArray", WHERE) \
     << LogIO::NORMAL; \
   if (x.dims() == 2) { \
       n0 = x.dim(0) > 60 ? 60 : x.dim(0); \
       n1 = x.dim(1) > 60 ? 60 : x.dim(1); \
       for (i = 0; i < n0; ++i) \
       for (j = 0; j < n1; ++j) \
         os << "(" << i << "," << j << ") = " \
            << x(i,j) << "\n"; \
   } \
   os << LogIO::POST; \
   delete &x; \
}

// now actually make the necessary versions of the above
DOGROUP(unsigned char)
DOGROUP(short)
DOGROUP(FitsLong)
DOGROUP(float)
DOGROUP(double)

DOARRAY(unsigned char)
DOARRAY(short)
DOARRAY(FitsLong)
DOARRAY(float)
DOARRAY(double)

#undef DOGROUP
#undef DOARRAY


// Read and display a binary table
void showBinaryTable(BinaryTableExtension &x) {

   LogIO os;

   if (x.err() != 0) {
       os << LogOrigin("FITSReader", "showBinaryTable", WHERE)
          << LogIO::WARN
          << "BT ERROR! " << x.err() << LogIO::POST;
       return;
   }
   showHDU(&x);

   ostringstream oss;
   oss << x.nrows() << " rows, "
       << x.ncols() << " cols, "
       << x.fitsdatasize() << " bytes total\n";
   os << LogOrigin("FITSReader", "showBinaryTable", WHERE)
      << LogIO::NORMAL << String(oss) << LogIO::POST;

   oss.str("");
   oss << "\nTable Data\n";
   int i;
   for (i = 0; i < x.ncols(); ++i) {
       oss << "Col " << i << ": " 
          << x.field(i).nelements() << " "
          << x.field(i).fieldtype() << " "
          << x.ttype(i) << " "
          << x.tunit(i) << "\n";       
   }
   os << LogOrigin("FITSReader", "showBinaryTable", WHERE)
      << LogIO::DEBUGGING << String(oss) << LogIO::POST;


   oss.str("");
   x.read(x.nrows()); 
   char *theheap = 0;
   if (x.pcount()) {
      if (x.notnull(x.theap())) {
         int heapOffset = x.theap() - x.rowsize()*x.nrows();
         // skip to the start of the heap
         char *junk = new char[heapOffset];
         x.ExtensionHeaderDataUnit::read(junk, heapOffset);
      }
      theheap = new char [x.pcount()]; 
      x.ExtensionHeaderDataUnit::read(theheap, x.pcount());
   }
   FITS::ValueType *vatypes = new FITS::ValueType[x.ncols()];
   void **vaptr =  new void *[x.ncols()];
   VADescFitsField *va = new VADescFitsField[x.ncols()];
   for (i = 0; i < x.ncols(); ++i) {
      vaptr[i] = 0;
      if (x.field(i).fieldtype() == FITS::VADESC) {
         int maxsize;
         FITS::parse_vatform(x.tform(i), vatypes[i], maxsize);
         x.bind(i, va[i]); 
         if (vatypes[i] == FITS::NOVALUE) {
            oss << "Error in VA desc format for column " 
               << i << " : " << x.tform(i) << '\n';
         } 
         else {
            switch (vatypes[i]) {
            case FITS::LOGICAL: 
               vaptr[i] = (void *)(new FitsLogical[maxsize]);
               break;
            case FITS::BIT: 
            {
                Int nbytes = maxsize / 8;
                if (maxsize % 8) nbytes++;
                maxsize = nbytes;
            }
            case FITS::BYTE: 
               vaptr[i] = (void *)(new unsigned char[maxsize]);
               break;
            case FITS::SHORT: 
               vaptr[i] = (void *)(new short[maxsize]);
               break;
            case FITS::LONG: 
               vaptr[i] = (void *)(new FitsLong[maxsize]);
               break;
            case FITS::CHAR: 
               vaptr[i] = (void *)(new char[maxsize]);
               break;
            case FITS::FLOAT: 
               vaptr[i] = (void *)(new float[maxsize]);
               break;
            case FITS::DOUBLE:
               vaptr[i] = (void *)(new double[maxsize]);
               break;
            case FITS::COMPLEX:
               vaptr[i] = (void *)(new Complex[maxsize]);
               break;
            case FITS::DCOMPLEX:
               vaptr[i] = (void *)(new DComplex[maxsize]);
               break;
            default: 
               oss << "Impossible VADesc type in column " 
                  << i << " : " << vatypes[i] << '\n';
            break;
            }
         }
      } 
      else {
         vatypes[i] = FITS::NOVALUE;
      }
   }

   int displayLines = 5;
   int displayCols = 20;
   oss << "the first " << displayCols << " cols of the first " << displayLines << " rows:\n"; 
   os << LogOrigin("FITSReader", "showBinaryTable", WHERE)
      << LogIO::NORMAL << String(oss) << LogIO::POST;

   oss.str("");
   for (int n = 0; n < displayLines && n < x.nrows(); ++n) { 
     oss << x.currrow() << ": | ";
     for (i = 0; i < displayCols && i < x.ncols(); ++i) {
       if (i != 0) oss << "| ";
       if (x.field(i).nelements() != 0) 
         oss << x.field(i) << ' ';
       if (x.field(i).fieldtype() == FITS::VADESC) {
         FitsVADesc thisva = va[i]();
         oss << " VA: ";
         if (thisva.num()) {
            switch (vatypes[i]) {
            case FITS::LOGICAL: 
              {
                FitsLogical *vptr = (FitsLogical *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            case FITS::BIT: 
              {
                unsigned char *vptr = (unsigned char *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                Int whichByte = 0;
                unsigned char mask = 0200;
                oss << (vptr[0] & mask);
                for (int k=1;k<thisva.num();++k) {
                  if (k%8 == 0) whichByte++;
                  oss << ", " << (vptr[whichByte] & (mask >> k%8));
                }
              }
              break;
            case FITS::BYTE: 
              {
                unsigned char *vptr = (unsigned char *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << (int)vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << (int)vptr[k];
              }
              break;
            case FITS::SHORT: 
              {
                short *vptr = (short *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            case FITS::LONG: 
              {
                FitsLong *vptr = (FitsLong *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            case FITS::CHAR: 
              {
                char *vptr = (char *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                for (int k=0; k < thisva.num() && vptr[k] != '\0'; ++k)
                  oss << vptr[k];
              }
              break;
            case FITS::FLOAT: 
              {
                float *vptr = (float *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            case FITS::DOUBLE: 
              {
                double *vptr = (double *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            case FITS::COMPLEX: 
              {
                Complex *vptr = (Complex *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            case FITS::DCOMPLEX: 
              {
                DComplex *vptr = (DComplex *)(vaptr[i]);
                FITS::f2l(vptr, (void *)(theheap + thisva.offset()), thisva.num());
                oss << vptr[0];
                for (int k=1;k<thisva.num();++k) 
                  oss << ", " << vptr[k];
              }
              break;
            default:
              break;
            }
         }
       }
     }
     if (i < x.ncols())
        oss << "| ...\n";
     else
        oss << "|\n";
     ++x; 
   }
   os << LogOrigin("FITSReader", "showBinaryTable", WHERE)
      << LogIO::NORMAL
      << String(oss) << LogIO::POST;

   for (i = 0; i < x.ncols(); ++i) {
       if (vaptr[i]) {
          switch (vatypes[i]) {
          case FITS::LOGICAL: delete [] (FitsLogical *)vaptr[i]; break;
          case FITS::BIT: 
          case FITS::BYTE: delete [] (unsigned char *)vaptr[i]; break;
          case FITS::SHORT: delete [] (short *)vaptr[i]; break;
          case FITS::LONG: delete [] (FitsLong *)vaptr[i]; break;
          case FITS::CHAR: delete [] (char *)vaptr[i]; break;
          case FITS::FLOAT: delete [] (float *)vaptr[i]; break;
          case FITS::DOUBLE: delete [] (double *)vaptr[i]; break;
          case FITS::COMPLEX: delete [] (Complex *)vaptr[i]; break;
          case FITS::DCOMPLEX: delete [] (DComplex *)vaptr[i]; break;
          default: break;
          }
       }
   }
   delete [] va;
   delete [] vatypes;
   delete [] vaptr;
   delete [] theheap;
   delete &x;
}


// Read and display values from a FITS file
void FITSReader::listFits(const char* fitsfile) {

   HeaderDataUnit *h;
   PrimaryArray<unsigned char> *paB;
   PrimaryArray<short> *paS;
   PrimaryArray<FitsLong> *paL;
   PrimaryArray<float> *paF;
   PrimaryArray<double> *paD;
   PrimaryGroup<unsigned char> *pgB;
   PrimaryGroup<short> *pgS;
   PrimaryGroup<FitsLong> *pgL;
   PrimaryGroup<float> *pgF;
   PrimaryGroup<double> *pgD;
   BinaryTableExtension *bt;
   AsciiTableExtension *at;


   LogIO os;
   os << LogOrigin("FITSReader", "listFits", WHERE)
      << LogIO::NORMAL
      << "read fitsfile=" << fitsfile << LogIO::POST;

   FitsInput fin(fitsfile,FITS::Disk);
   os << LogOrigin("FITSReader", "listFits", WHERE)
      << LogIO::NORMAL5 << "FitsInput object created ok."
      << LogIO::POST;
   
   if (fin.err() == FitsIO::IOERR) {
       os << LogOrigin("FITSReader", "listFits", WHERE)
          << LogIO::WARN << "Error opening FITS input."
          << LogIO::POST;
   } else if (fin.err()) {
       os << LogOrigin("FITSReader", "listFits", WHERE)
          << LogIO::WARN 
          << "Error reading initial record -- exiting."
          << LogIO::POST;
   }
   const int NMAXERRS = 5;

   int nerrs;
   for(nerrs = 0;
      nerrs < NMAXERRS && fin.rectype() != FITS::EndOfFile; ) {
     if (fin.rectype() == FITS::HDURecord) {
       switch (fin.hdutype()) {
         case FITS::PrimaryArrayHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Primary Array HDU ------>>>" << LogIO::POST;
            os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            switch (fin.datatype()) {
               case FITS::BYTE:
                  paB = new PrimaryArray<unsigned char>(fin);
                  showPrimaryArray(*paB);
                  break;
               case FITS::SHORT:
                  paS = new PrimaryArray<short>(fin);
                  showPrimaryArray(*paS);
                  break;
               case FITS::LONG:
                  paL = new PrimaryArray<FitsLong>(fin);
                  showPrimaryArray(*paL);
                  break;
               case FITS::FLOAT:
                  paF = new PrimaryArray<float>(fin);
                  showPrimaryArray(*paF);
                  break;
              case FITS::DOUBLE:
                  paD = new PrimaryArray<double>(fin);
                  showPrimaryArray(*paD);
                  break;
               default:
                  break;
            }
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Primary Array HDU" << LogIO::POST;
            break;
         case FITS::PrimaryGroupHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Primary Group HDU ------>>>" << LogIO::POST;
            os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            switch (fin.datatype()) {
               case FITS::BYTE:
                  pgB = new PrimaryGroup<unsigned char>(fin);
                  showPrimaryGroup(*pgB);
                  break;
               case FITS::SHORT:
                  pgS = new PrimaryGroup<short>(fin);
                  showPrimaryGroup(*pgS);
                  break;
               case FITS::LONG:
                  pgL = new PrimaryGroup<FitsLong>(fin);
                  showPrimaryGroup(*pgL);
                  break;
               case FITS::FLOAT:
                  pgF = new PrimaryGroup<float>(fin);
                  showPrimaryGroup(*pgF);
                  break;
               case FITS::DOUBLE:
                  pgD = new PrimaryGroup<double>(fin);
                  showPrimaryGroup(*pgD);
                  break;
               default:
                  break;
            }
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Primary Group HDU" << LogIO::POST;
            break;
         case FITS::AsciiTableHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Ascii Table HDU ------>>>" << LogIO::POST;
            os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            at = new AsciiTableExtension(fin);
            showBinaryTable(*at);
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Ascii Table HDU" << LogIO::POST;
            break;
         case FITS::BinaryTableHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Binary Table HDU ------>>>" << LogIO::POST;
            os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            bt = new BinaryTableExtension(fin);
            showBinaryTable(*bt);
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Binary Table HDU" << LogIO::POST;
            break;
         case FITS::ImageExtensionHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Image Extension HDU ------>>>" << LogIO::POST;
            os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            switch (fin.datatype()) {
               case FITS::BYTE:
                  paB = new ImageExtension<unsigned char>(fin);
                  showPrimaryArray(*paB);
                  break;
               case FITS::SHORT:
                  paS = new ImageExtension<short>(fin);
                  showPrimaryArray(*paS);
                  break;
               case FITS::LONG:
                  paL = new ImageExtension<FitsLong>(fin);
                  showPrimaryArray(*paL);
                  break;
               case FITS::FLOAT:
                  paF = new ImageExtension<float>(fin);
                  showPrimaryArray(*paF);
                  break;
               case FITS::DOUBLE:
                  paD = new ImageExtension<double>(fin);
                  showPrimaryArray(*paD);
                  break;
               default:
                  break;
            }
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Image Extension HDU" << LogIO::POST;
            break;
         case FITS::PrimaryTableHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Primary Table HDU ------>>>" << LogIO::POST;
            os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            switch (fin.datatype()) {
               case FITS::BYTE:
                  paB = new PrimaryTable<unsigned char>(fin);
                  showPrimaryArray(*paB);
                  break;
               case FITS::SHORT:
                  paS = new PrimaryTable<short>(fin);
                  showPrimaryArray(*paS);
                  break;
               case FITS::LONG:
                  paL = new PrimaryTable<FitsLong>(fin);
                  showPrimaryArray(*paL);
                  break;
               case FITS::FLOAT:
                  paF = new PrimaryTable<float>(fin);
                  showPrimaryArray(*paF);
                  break;
               case FITS::DOUBLE:
                  paD = new PrimaryTable<double>(fin);
                  showPrimaryArray(*paD);
                  break;
               default:
                  break;
            }
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Primary Table HDU" << LogIO::POST;
            break;
         case FITS::UnknownExtensionHDU:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Unknown Extension HDU ------>>>" << LogIO::POST;
            //os << LogIO::DEBUGGING << "Data type is " << fin.datatype() << LogIO::POST;
            h = new ExtensionHeaderDataUnit(fin);
            h->skip();
            delete h;
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "<<<------ Unknown Extension HDU" << LogIO::POST;
            break;
         default:
            os << LogOrigin("FITSReader", "listFits", WHERE)
               << LogIO::NORMAL << "Unknown Extension HDU ------>>>" << LogIO::POST;
            os << LogIO::NORMAL << "<<<------ Unknown Extension HDU" << LogIO::POST;
            break;
      }
    }else if (fin.rectype() == FITS::BadBeginningRecord ||
             fin.rectype() == FITS::UnrecognizableRecord) {
       os << LogOrigin("FITSReader", "listFits", WHERE)
          << LogIO::WARN 
          << "Bad Record encountered"
          << LogIO::POST;
    }else if (fin.rectype() == FITS::SpecialRecord) {
       os << LogOrigin("FITSReader", "listFits", WHERE)
          << LogIO::WARN 
          << "Special Record encountered"
          << LogIO::POST;
    }
    if (fin.err())
       ++nerrs;
   }

   if (nerrs == NMAXERRS)
       os << LogOrigin("FITSReader", "listFits", WHERE)
          << LogIO::SEVERE 
          << "Too many errors.  Processing terminated."
          << LogIO::POST;
   else
       os << LogOrigin("FITSReader", "listFits", WHERE)
          << LogIO::NORMAL 
          << "finish processing of Header-Data Units."
          << LogIO::POST;
   return ;

}


} 

