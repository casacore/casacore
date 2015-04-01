//# ArrayIO.cc: text output and binary IO for an array of any dimensionality.
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001,2002,2003
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

#ifndef CASA_ARRAYIO_TCC
#define CASA_ARRAYIO_TCC

//# Includes
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/AipsIOCarray.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>
#include <casacore/casa/sstream.h>           // needed for internal IO

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Take care that a uChar is printed numerically, not as a letter.
inline void ArrayIO_printValue (ostream& s, const uChar& v)
  { s << Int(v); }
template<class T>
inline void ArrayIO_printValue (ostream& s, const T& v)
  { s << v; }


template<class T>
ostream &operator<<(ostream &s, const Array<T> &a)
{
    // Print any "header" information
    if (a.ndim() > 2) {
	s << "Ndim=" << a.ndim() << " ";
    }
    if (a.ndim() > 1) {
	s << "Axis Lengths: " << a.shape() << " ";
    }

    if (a.nelements() == 0) {
      s << "[]";
      return s;
    }

    // Then print the values -
    if (a.ndim() == 1) {
	// Vector
	IPosition ipos(1);
	s << "[";
	Int64 iend = a.shape()(0) - 1;
	for (Int64 i=0; i < iend; i++) {
	    ipos(0) = i;
	    ArrayIO_printValue (s, a(ipos));
	    s << ", ";
	}
	ipos(0) = iend;
	ArrayIO_printValue (s, a(ipos));
	s << "]";
    } else if (a.ndim() == 2) {
	// Matrix
	s << " (NB: Matrix in Row/Column order)" << endl;
	IPosition index(2);
	Int64 row_end = a.shape()(0) - 1;
	Int64 col_end = a.shape()(1) - 1;
	for (Int64 i=0; i <= row_end; i++) {
	    index(0) = i;
	    if (i == 0) {
		s << "[";
	    } else {
		s << " ";
	    }
	    for (Int64 j=0; j <= col_end; j++) {
		index(1) = j;
		ArrayIO_printValue (s, a(index));
		if (j != col_end) {
		    s << ", ";
		}
	    }
	    if (i != row_end) {
		s << endl;
	    } else {
		s << "]" << endl;
	    }
	}
    } else {
	// Any dimension - print by vectors, preceed each vector by IPosition
	// of start.
	s << endl;
	IPosition ashape = a.shape();
	Int andim = a.ndim();
	ArrayPositionIterator ai(ashape, 1);
	Int i;
	IPosition index(andim);
	// Print vector by vector
	while(! ai.pastEnd()) {
	    index = ai.pos();
	    s << index;
	    s << "[";
	    for(i=0; i < ashape(0); i++) {
		index(0) = i;
		if (i > 0) s << ", ";
		ArrayIO_printValue (s, a(index));
	    }
	    s << "]\n";
	    ai.next();
	}
    }
    return s;
}

template<class T>
LogIO &operator<<(LogIO &os, const Array<T> &a)
{
    os.output() << a;
    return os;
}

template<class T>
AipsIO &operator<<(AipsIO &ios, const Array<T> &a)
{
    putArray (ios, a, "Array");
    return ios;
}

template<class T>
void putArray (AipsIO &ios, const Array<T> &a, const Char* name)
{
    if (a.size() * sizeof(T) > 2147483647) {
      throw AipsError("AipsIO putArray too large (exceeds 2**31 bytes)");
    }
    ios.putstart(name, Array<T>::arrayVersion());
    // Write out dimensionality
    ios << a.ndim();
    // Write out length
    for (uInt i=0; i < a.ndim(); i++) {
      ios << uInt(a.shape()(i));
    }
    // Now write out the data
    Bool deleteIt;
    const T *storage =  a.getStorage(deleteIt);
    putAipsIO (ios, a.nelements(), storage);
    a.freeStorage(storage, deleteIt);
    ios.putend();
}

// <thrown>
//     <item> ArrayError
// </thrown>
template<class T>
AipsIO &operator>>(AipsIO &ios, Array<T> &a)
{
    // Makes the argument unique, i.e. existing refs will be lost. At the moment
    // this is relatively inefficient as it makes a temporary copy.
    a.unique();
    
    // On 20-Nov-2000 use of the home-brew rtti was removed.
    // It meant that a name 'Array<Int>' is now replaced by 'Array'.
    // In order to recognize those old names, we must do something special.
    String type = ios.getNextType();
    Int version;
    if (type.length() > 6  &&  type.index("Array<") == 0) {
      version = ios.getstart(type);
    } else {
      version = ios.getstart("Array");
    }
    Int ndim;
    ios >> ndim;
    IPosition shape(ndim);
    // Older versions contain an origin (which we discard).
    if (version < 3) {
	Int orig;
	for (Int i=0; i < ndim; i++) {
	    ios >> orig;
	}
    }
    uInt v;
    for (Int i=0; i < ndim; i++) {
      ios >> v;
      shape(i) = v;
    }
    a.resize(shape);                // hopefully a no-op if unchanged

    // Now read in the data.

    Bool deleteIt;
    T *storage = a.getStorage(deleteIt);

    uInt nwritten;
    ios >> nwritten;
    if (nwritten != a.nelements())
	throw(ArrayError("AipsIO &operator>>(AipsIO, Array<T>"
			  " - nelements() differs from number in file"));
    getAipsIO (ios, nwritten, storage);
    a.putStorage(storage, deleteIt);
    ios.getend();
    return ios;
}




// These functions allow the user to read /write raw binary data files
// created with the Array class to / from disk, so that they can be
// viewed, for example, with SAOimage.
//
// They should eventually be replaced with something more sophisticated.

template <class T>
void write_array (const Array<T>& the_array, const String& fileName)
{
    size_t nbytes = the_array.nelements() * sizeof(T);
    ofstream outfile(fileName.chars(), ios::out);
    if(!outfile) {
	throw (ArrayError
	       ("write_array error: could not open file " + fileName));
    }
    Bool delete_storage;
    const T *storage = the_array.getStorage (delete_storage);
    outfile.write ((char*)storage, nbytes);
    outfile.close();
    the_array.freeStorage (storage, delete_storage);
}


template <class T>
void read_array(Array<T>& the_array, const String& fileName)
{
    size_t nbytes = the_array.nelements() * sizeof(T);
    ifstream infile(fileName.chars(), ios::in);
    if(!infile) {
	throw (ArrayError
	       ("read_array error: could not open file " + fileName));
    }
    Bool delete_storage;
    T *storage = the_array.getStorage (delete_storage);
    infile.read ((char*)storage, nbytes);
    infile.close();
    the_array.putStorage (storage, delete_storage);
}



template <class T>
void readAsciiMatrix (Matrix<T>& mat, const Char* filein)
{
    const Int      bufSize = 1024, numberSize = 50;
          char     buf[bufSize], buf2[numberSize];
          uInt     blockSize = 100;
          Block<T> temp(blockSize);

    ifstream iFile;
    iFile.open (filein, ios::in);
    if (! iFile) {
	throw (ArrayError ("readAsciiFile: cannot open " + String(filein)));
    }

    size_t rows = 0, cols = 0, saveCols = 0;
    uInt havePoint = 0;

    while (iFile.getline(buf, bufSize)) {
	Int blankLine = 1;
	for (Int j1=0;j1<bufSize;j1++) {
	    if (buf[j1] == '\0')
		break;
	    if (buf[j1] != ' ') {
		blankLine = 0;
		break;
	    }
	}

	if (! blankLine) { 
	    if (havePoint > (blockSize - saveCols - 10)) {
		blockSize *= 2;
		temp.resize(blockSize);
	    }

	    rows += 1; cols = 0;
	    Int ch = 0, startedNew = 0;
	    for (Int i2=0; i2<bufSize; i2++) {
		if (buf[i2] == '\0' || buf[i2] == ' ') {
		    if (ch > 0) {
			buf2[ch] = ' ';
			// istringstream(buf2,sizeof(buf2)) >>  temp[havePoint];
			istringstream(buf2) >>  temp[havePoint];
			havePoint += 1;
			ch = 0;
		    }
		    startedNew = 0;
		    if (buf[i2] == '\0')
			break;
		}
		
		if (buf[i2] != ' ' && startedNew == 0) {
		    cols += 1;
		    startedNew = 1;
		}

		if (startedNew)
		    buf2[ch++] = buf[i2];
	    }

	    if (rows == 1)
		saveCols = cols;
	    else if (cols != saveCols) {
		cout << "Array is not regular.  Number of elements was "
		     << saveCols << " at row 1"
		     << " but is " << cols << " at row " << rows << endl;
		exit (1);
	    }
	}
    }
    iFile.close();

    mat.resize(rows, cols);
    size_t k3 = 0;
    for (size_t i3=0;i3<rows;i3++)
	for (size_t j3=0;j3<cols;j3++)
	    mat(i3,j3) = temp[k3++];

}


template <class T>
void writeAsciiMatrix (const Matrix<T>& mat, const Char* fileout)
{
    ofstream oFile;
    oFile.precision(12);
    oFile.open (fileout, ios::out);
    if (! oFile) {
	throw (ArrayError ("writeAsciiFile: cannot open " + String(fileout)));
    }
    for (size_t i1=0;i1<mat.nrow();i1++) {
      for (size_t j1=0;j1<mat.ncolumn();j1++) {
	    oFile << mat(i1,j1) << "  ";
	}
	oFile << endl; 
    }    
}


template <class T>
void readAsciiVector (Vector<T>& vect, const Char* filein) 
{
    const Int      bufSize = 1024, numberSize = 50;
          char     buf[bufSize], buf2[numberSize];
          uInt     blockSize = 100;
          Block<T> temp(blockSize);

    ifstream iFile;
    iFile.open (filein, ios::in);
    if (! iFile) {
	throw (ArrayError ("readAsciiFile: cannot open " + String(filein)));
    }

    size_t havePoint = 0;

    while (iFile.getline(buf, bufSize)) {
	Int blankLine = 1;
	for (Int j1=0;j1<bufSize;j1++) {
	    if (buf[j1] == '\0')
		break;
	    if (buf[j1] != ' ') {
		blankLine = 0;
		break;
	    }
	}

	if (! blankLine) { 
	    Int ch = 0, startedNew = 0;
	    for (Int i2=0; i2<bufSize; i2++) {
		if (buf[i2] == '\0' || buf[i2] == ' ') {
		    if (ch > 0) {
			buf2[ch] = ' ';
			if (havePoint > (blockSize - 2)) {
			    blockSize *= 2;
			    temp.resize(blockSize);
			}
			istringstream(buf2) >> temp[havePoint];
			havePoint +=1;
			if (buf[i2] == ' ')
			    ch = 0;
		    }
		    if (buf[i2] == '\0')
			break;
		    else
			startedNew = 0;
		}

		if (buf[i2] != ' ' && startedNew == 0)
		    startedNew = 1;

		if (startedNew)
		    buf2[ch++] = buf[i2];
	    }
	} 
    }
    iFile.close();

    vect.resize(havePoint);
    size_t k3 = 0;
    for (size_t i3=0;i3<havePoint;i3++)
	vect(i3) = temp[k3++];

}



template <class T>
void writeAsciiVector (const Vector<T>& vect, const Char* fileout)
{
    size_t rows = vect.size();
    ofstream oFile;
    oFile.precision(12);
    oFile.open (fileout, ios::out);
    if (! oFile) {
	throw (ArrayError ("writeAsciiFile: cannot open " + String(fileout)));
    }
    for (size_t i1=0;i1<rows;i1++) {
	oFile << vect(i1) << "  ";
    }
    oFile << endl; 
}

template <class T>
istream &operator >> (istream &s, Array<T> &x) {
  if (!read(s, x, 0, False)) {
    s.clear(ios::failbit | s.rdstate());
  }
  return s;
}

template <class T>
Bool read(istream &s, Array<T> &x,
	  const IPosition *ip, Bool it) {
  ///  Array<T> *to; PCAST(to, Array<T>, &x);
  ///  if (to) {
  // If an empty array, it can get any dimension.
  if (x.ndim() == 0) {
    Block<T> tmp;
    Bool tr;
    IPosition p;
    if (!readArrayBlock(s, tr, p, tmp, ip, it)) return False;
    x.resize(p);
    uInt iptr = p.nelements() - 1;
    IPosition iter(p); iter = Int(0);
    for (size_t i=0; i < x.nelements(); i++) {
      x(iter) = tmp[i];
      if (!tr) {
	for (Int j=iptr; j >=0; j--) {
	  iter(j) += 1;
	  if (iter(j) < p(j)) break;
	  iter(j) = 0;
	}
      } else {
	for (uInt j=0; j <=iptr; j++) {
	  iter(j) += 1;
	  if (iter(j) < p(j)) break;
	  iter(j) = 0;
	}
      }
    }
  } else {
    // Otherwise try if we can resize.
    // This will always be possible for an Array,
    // but e.g. not for Vector if the Array is not 1D.
    Array<T> tx;
    if (!read(s, tx, ip, it)) return False;
    try {
      x.resize (tx.shape());
      x = tx;
    } catch (AipsError) {
      IPosition first;
      IPosition last;
      if (x.ndim() >= tx.ndim()) {
	first = tx.shape();
	last = IPosition(x.ndim() - tx.ndim()); last = Int(1);
      } else {
	first = tx.shape().getFirst(x.ndim() - 1);
	last = IPosition(1, tx.shape().
			 getLast(tx.ndim() - x.ndim() + 1).product());
      }
      IPosition tot(x.ndim());
      tot.setFirst(first);
      tot.setLast(last);
      x.resize(tot);
      IPosition p(x.shape());
      uInt iptr = p.nelements() - 1;
      IPosition iter(p);
      iter = Int(0);
      Bool deleteIt;
      const T *tmp = tx.getStorage(deleteIt);
      for (size_t i=0; i < tx.nelements(); i++) {
	x(iter) = tmp[i];
	for (uInt j=0; j<=iptr; j++) {
	  iter(j) += 1;
	  if (iter(j) < p(j)) break;
	  iter(j) = 0;
	}
      }
      tx.freeStorage(tmp, deleteIt);
    }
  }
  return True;
}

template <class T>
Bool readArrayBlock(istream &s, Bool &trans,
	       IPosition &p,
	       Block<T> &x, const IPosition *ip, Bool it) {

  if (!s.good()) {
    s.clear(ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
    return False;
  }
  Bool how = True;
  T r;
  Char ch;
  size_t cnt = 0;
  p.resize(0);
  if (ip == 0) {
    p = IPosition(0);
  } else {
    p = *ip;
  }
  trans = it;
  s >> ws;
  s.get(ch);
  if (ch == '{') {
    s >> ws;
    s.get(ch);
    if (ch == 't' || ch == 'T') {
      trans = (!trans);
      s >> ws;
      s.get(ch);
    }
    if (ch != '}') {
      s.putback(ch);
      Bool lpt;
      IPosition lpp, lpq;
      Block<uInt> lpx;
      if (!readArrayBlock(s, lpt, lpp, lpx, 0, False) ||
	  lpp.nelements() != 1) {
	how = False;
      } else {
	lpq.resize(lpp(0));
	for (Int i=0; i<lpp(0); i++) {
	  lpq(i) = lpx[i];
	}
	if (p.nelements() != 0 && p.product() != lpq.product()) {
	  how = False;
	} else if (p.nelements() == 0) {
	  p = lpq;
	}
      }
    } else {
      s.putback(ch);
    }
    if (how) {
      s >> ws;
      s.get(ch);
      if (ch != '}') {
	how = False;
      } else {
	s >> ws;
	s.get(ch);
      }
    }
  }		// end start shape

  // The following is done to circumvent the problem arising from the fact that
  // the output of a String array is given as [ abc, def], but the String >>
  // will read all characters between blanks. Proper String I/O handling
  // would solve this. Lines with /// were added/deleted
  if (how && ch != '[') {
    s.putback(ch);
    s >> r;
    if (!s.good()) {
      s.clear(ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
      how = False;
    } else {
      if (x.nelements() <= cnt) {
	x.resize(2*x.nelements() + 1);
      }
      x[cnt] = r;
      cnt++;
    }
  } else {
    String st;	///
    String sts;	///
    uInt chstr = Register(static_cast<String *>(0));	///
    while (how) {
      s >> ws;
      s.get(ch);
      if (ch == ',') {
	s >> ws;
      } else if (ch == ']' || (Int)ch == EOF) {
	break;
      } else {
	s.putback(ch);
      }
      if (chstr == Register(&r)) {	/// all of this extra
	s >> st;	/// Read string
	Int ix = st.index(Regex("[],]"));	/// See if any present
	if (ix >= 0) {	/// if yes
	  while ((ix = st.index(',')) >= 0) {
	    sts = st.before(ix);
	    istringstream ins(sts.chars()); /// Necessary for template
	    ins >> r;				/// expansion
	    st = st.after(ix);
	    if (x.nelements() <= cnt) {
	      x.resize(2*x.nelements() + 1);
	    }
	    x[cnt] = r;
	    cnt++;
	  }
	  if ((ix = st.index(']')) >= 0) {
	    sts = st.before(ix);
	    istringstream ins(sts.chars()); /// Necessary for template
	    ins >> r;				/// expansion
	    st = st.from(ix);
	    if (x.nelements() <= cnt) {
	      x.resize(2*x.nelements() + 1);
	    }
	    x[cnt] = r;
	    cnt++;
	    for (Int i1=st.length()-1; i1>=0; i1--) {	/// set back
	      s.putback(st[i1]);	///
	    }			///
	    break;
	  }	  
	  for (Int i1=st.length()-1; i1>=0; i1--) {	/// set back
	    s.putback(st[i1]);	///
	  }			///
	} else {
	  istringstream ins(st.chars()); /// Necessary for template
	  ins >> r;				/// expansion
	  if (x.nelements() <= cnt) {
	    x.resize(2*x.nelements() + 1);
	  }
	  x[cnt] = r;
	  cnt++;
	}
	if (!s.good()) {
	  s.clear(ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
	  how = False;
	}
      } else {
	s >> r;
	if (!s.good()) {
	  s.clear(ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
	  how = False;
	} else {
	  if (x.nelements() <= cnt) {
	    x.resize(2*x.nelements() + 1);
	  }
	  x[cnt] = r;
	  cnt++;
	}
      }
    }
  }
  if (how) {
    if (p.nelements() == 0) {
      p = IPosition(1, cnt);
    } else if (Int64(cnt) != p.product()) {
      how = False;
    }
  }
  if (!how) {
    s.clear(ios::failbit);
    p.resize(0);
  }
  return how;
}


} //# NAMESPACE CASACORE - END


#endif
