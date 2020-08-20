#ifndef CASACORE_ARRAYSTR_TCC
#define CASACORE_ARRAYSTR_TCC

#include "Array.h"
#include "ArrayStr.h"
#include "ArrayPosIter.h"

#include <string>
#include <fstream>
#include <istream>
#include <stdexcept>

namespace casacore {
  
template<typename T, typename Alloc>
std::string to_string(const Array<T, Alloc> array)
{
  std::ostringstream str;
  str << array;
  return str.str();
}

// Take care that a uChar is printed numerically, not as a letter.
inline void ArrayIO_printValue (std::ostream& s, const unsigned char& v)
  { s << int(v); }
template<typename T>
inline void ArrayIO_printValue (std::ostream& s, const T& v)
  { s << v; }

template<typename T, typename Alloc>
std::ostream &operator<<(std::ostream &s, const Array<T, Alloc> &a)
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
    long long iend = a.shape()(0) - 1;
    for (long long i=0; i < iend; i++) {
      ipos(0) = i;
      casacore::ArrayIO_printValue (s, a(ipos));
      s << ", ";
    }
    ipos(0) = iend;
    casacore::ArrayIO_printValue (s, a(ipos));
    s << "]";
  } else if (a.ndim() == 2) {
    // Matrix
    s << " (NB: Matrix in Row/Column order)\n";
    IPosition index(2);
    long long row_end = a.shape()(0) - 1;
    long long col_end = a.shape()(1) - 1;
    for (long long i=0; i <= row_end; i++) {
      index(0) = i;
      if (i == 0) {
        s << "[";
      } else {
        s << " ";
      }
      for (long long j=0; j <= col_end; j++) {
        index(1) = j;
        casacore::ArrayIO_printValue (s, a(index));
        if (j != col_end) {
          s << ", ";
        }
      }
      if (i != row_end) {
        s << '\n';
      } else {
        s << "]\n";
      }
    }
  } else {
    // Any dimension - print by vectors, preceed each vector by IPosition
    // of start.
    s << '\n';
    IPosition ashape = a.shape();
    int andim = a.ndim();
    ArrayPositionIterator ai(ashape, 1);
    int i;
    IPosition index(andim);
    // Print vector by vector
    while(! ai.pastEnd()) {
      index = ai.pos();
      s << index;
      s << "[";
      for(i=0; i < ashape(0); i++) {
        index(0) = i;
        if (i > 0) s << ", ";
        casacore::ArrayIO_printValue (s, a(index));
      }
      s << "]\n";
      ai.next();
    }
  }
  return s;
}

// These functions allow the user to read /write raw binary data files
// created with the Array class to / from disk, so that they can be
// viewed, for example, with SAOimage.
//
// They should eventually be replaced with something more sophisticated.

template <typename T, typename Alloc>
void write_array (const Array<T, Alloc>& the_array, const std::string& fileName)
{
  size_t nbytes = the_array.nelements() * sizeof(T);
  std::ofstream outfile(fileName, std::ios::out);
  if(!outfile) {
    throw (ArrayError
    ("write_array error: could not open file " + fileName));
  }
  std::vector<T> storage = the_array.tovector ();
  outfile.write ((char*)storage.data(), nbytes);
  outfile.close();
}

template <typename T, typename Alloc>
void read_array(Array<T, Alloc>& the_array, const std::string& fileName)
{
  size_t nbytes = the_array.nelements() * sizeof(T);
  std::ifstream infile(fileName, std::ios::in);
  if(!infile) {
    throw (ArrayError
    ("read_array error: could not open file " + fileName));
  }
  bool delete_storage;
  T *storage = the_array.getStorage (delete_storage);
  infile.read ((char*)storage, nbytes);
  infile.close();
  the_array.putStorage (storage, delete_storage);
}

template <typename T, typename Alloc>
void readAsciiMatrix (Matrix<T, Alloc>& mat, const char* filein)
{
  const size_t bufSize = 1024;
  char buf[bufSize];
  std::fill_n(buf, bufSize, 0);
  
  const size_t numberSize = 50;
  char buf2[numberSize];
  std::fill_n(buf2, numberSize, 0);
  
  size_t     blockSize = 100;
  std::vector<T> temp(blockSize);
  
  std::ifstream iFile;
  iFile.open (filein, std::ios::in);
  if (! iFile) {
    throw (ArrayError ("readAsciiFile: cannot open " + std::string(filein)));
  }
  
  size_t rows = 0, cols = 0, saveCols = 0;
  size_t havePoint = 0;
  
  while (iFile.getline(buf, bufSize)) {
    size_t blankLine = 1;
    for (size_t j1=0;j1<bufSize;j1++) {
      if (buf[j1] == '\0')
        break;
      if (buf[j1] != ' ') {
        blankLine = 0;
        break;
      }
    }
    
    if (! blankLine) { 
      if (int(havePoint) > (int(blockSize) - int(saveCols) - 10)) {
        blockSize *= 2;
        temp.resize(blockSize);
      }
      
      rows += 1; cols = 0;
      size_t ch = 0;
      bool startedNew = false;
      for (size_t i2=0; i2<bufSize; i2++) {
        if (buf[i2] == '\0' || buf[i2] == ' ') {
          if (ch > 0) {
            buf2[ch] = ' ';
            // istringstream(buf2,sizeof(buf2)) >>  temp[havePoint];
            std::istringstream(buf2) >>  temp[havePoint];
            havePoint += 1;
            ch = 0;
          }
          startedNew = false;
          if (buf[i2] == '\0')
            break;
        }
        
        if (buf[i2] != ' ' && !startedNew) {
          cols += 1;
          startedNew = true;
        }
        
        if (startedNew)
          buf2[ch++] = buf[i2];
      }
      
      if (rows == 1)
        saveCols = cols;
      else if (cols != saveCols) {
        throw ArrayError("Array is not regular.  Number of elements was "
        + std::to_string(saveCols) + " at row 1" +
        " but is " + std::to_string(cols) + " at row " + std::to_string(rows));
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


template <typename T, typename Alloc>
void writeAsciiMatrix (const Matrix<T, Alloc>& mat, const char* fileout)
{
  std::ofstream oFile;
  oFile.precision(12);
  oFile.open (fileout, std::ios::out);
  if (! oFile) {
    throw (ArrayError ("writeAsciiFile: cannot open " + std::string(fileout)));
  }
  for (size_t i1=0;i1<mat.nrow();i1++) {
    for (size_t j1=0;j1<mat.ncolumn();j1++) {
      oFile << mat(i1,j1) << "  ";
    }
    oFile << '\n'; 
  }    
}


template <typename T, typename Alloc>
void readAsciiVector (Vector<T, Alloc>& vect, const char* filein) 
{
  const size_t bufSize = 1024;
  char buf[bufSize];
  std::fill_n(buf, bufSize, 0);
  
  const size_t numberSize = 50;
  char buf2[numberSize];
  std::fill_n(buf2, numberSize, 0);
  
  size_t     blockSize = 100;
  std::vector<T> temp(blockSize);
  
  std::ifstream iFile;
  iFile.open (filein, std::ios::in);
  if (! iFile) {
    throw (ArrayError ("readAsciiFile: cannot open " + std::string(filein)));
  }
  
  size_t havePoint = 0;
  
  while (iFile.getline(buf, bufSize)) {
    size_t blankLine = 1;
    for (size_t j1=0;j1<bufSize;j1++) {
      if (buf[j1] == '\0')
        break;
      if (buf[j1] != ' ') {
        blankLine = 0;
        break;
      }
    }
    
    if (! blankLine) { 
      size_t ch = 0;
      bool startedNew = false;
      for (size_t i2=0; i2<bufSize; i2++) {
        if (buf[i2] == '\0' || buf[i2] == ' ') {
          if (ch > 0) {
            buf2[ch] = ' ';
            if (int(havePoint) > (int(blockSize) - 2)) {
              blockSize *= 2;
              temp.resize(blockSize);
            }
            std::istringstream(buf2) >> temp[havePoint];
            havePoint +=1;
            if (buf[i2] == ' ')
              ch = 0;
          }
          if (buf[i2] == '\0')
            break;
          else
            startedNew = false;
        }
        
        if (buf[i2] != ' ' && !startedNew)
          startedNew = true;
        
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

template <typename T, typename Alloc>
void writeAsciiVector (const Vector<T, Alloc>& vect, const char* fileout)
{
  size_t rows = vect.size();
  std::ofstream oFile;
  oFile.precision(12);
  oFile.open (fileout, std::ios::out);
  if (! oFile) {
    throw (ArrayError ("writeAsciiFile: cannot open " + std::string(fileout)));
  }
  for (size_t i1=0;i1<rows;i1++) {
    oFile << vect(i1) << "  ";
  }
  oFile << '\n'; 
}

template <typename T, typename Alloc>
std::istream &operator >> (std::istream &s, Array<T, Alloc> &x) {
  if (!read(s, x, 0, false)) {
    s.clear(std::ios::failbit | s.rdstate());
  }
  return s;
}

template <typename T, typename Alloc>
bool read(std::istream &s, Array<T, Alloc> &x,
	  const IPosition *ip, bool it) {
  ///  Array<T, Alloc> *to; PCAST(to, Array<T, Alloc>, &x);
  ///  if (to) {
  // If an empty array, it can get any dimension.
  if (x.ndim() == 0) {
    std::vector<T> tmp;
    bool tr;
    IPosition p;
    if (!readArrayBlock(s, tr, p, tmp, ip, it)) return false;
    x.resize(p);
    size_t iptr = p.nelements() - 1;
    IPosition iter(p); iter = int(0);
    for (size_t i=0; i < x.nelements(); i++) {
      x(iter) = tmp[i];
      if (!tr) {
	for (int j=iptr; j >=0; j--) {
	  iter(j) += 1;
	  if (iter(j) < p(j)) break;
	  iter(j) = 0;
	}
      } else {
	for (size_t j=0; j <=iptr; j++) {
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
    Array<T, Alloc> tx;
    if (!read(s, tx, ip, it)) return false;
    // TODO this can be done without trial and error:
    // reading a matrix in always throws and catches an exception
    // in the current implementation, which should not be the
    // normal flow.
    try {
      x.resize (tx.shape());
      x.assign_conforming(tx);
    } catch (std::runtime_error&) {
      IPosition first;
      IPosition last;
      if (x.ndim() >= tx.ndim()) {
	first = tx.shape();
	last = IPosition(x.ndim() - tx.ndim()); last = int(1);
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
      size_t iptr = p.nelements() - 1;
      IPosition iter(p);
      iter = int(0);
      bool deleteIt;
      const T *tmp = tx.getStorage(deleteIt);
      for (size_t i=0; i < tx.nelements(); i++) {
	x(iter) = tmp[i];
	for (size_t j=0; j<=iptr; j++) {
	  iter(j) += 1;
	  if (iter(j) < p(j)) break;
	  iter(j) = 0;
	}
      }
      tx.freeStorage(tmp, deleteIt);
    }
  }
  return true;
}

template <typename T, typename Alloc>
bool readArrayBlock(std::istream &s, bool &trans,
                    IPosition &p,
                    std::vector<T, Alloc> &x, const IPosition *ip, bool it) {
  if (!s.good()) {
    s.clear(std::ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
    return false;
  }
  bool how = true;
  T r;
  char ch;
  size_t cnt = 0;
  p.resize(0);
  if (ip == 0) {
    p = IPosition(0);
  } else {
    p = *ip;
  }
  trans = it;
  s >> std::ws;
  s.get(ch);
  if (ch == '{') {
    s >> std::ws;
    s.get(ch);
    if (ch == 't' || ch == 'T') {
      trans = (!trans);
      s >> std::ws;
      s.get(ch);
    }
    if (ch != '}') {
      s.putback(ch);
      bool lpt;
      IPosition lpp, lpq;
      std::vector<size_t> lpx;
      if (!readArrayBlock(s, lpt, lpp, lpx, 0, false) ||
        lpp.nelements() != 1) {
        how = false;
        } else {
          lpq.resize(lpp(0));
          for (int i=0; i<lpp(0); i++) {
            lpq(i) = lpx[i];
          }
          if (p.nelements() != 0 && p.product() != lpq.product()) {
            how = false;
          } else if (p.nelements() == 0) {
            p = lpq;
          }
        }
    } else {
      s.putback(ch);
    }
    if (how) {
      s >> std::ws;
      s.get(ch);
      if (ch != '}') {
        how = false;
      } else {
        s >> std::ws;
        s.get(ch);
      }
    }
  }		// end start shape
  
  // The following is done to circumvent the problem arising from the fact that
  // the output of a string array is given as [ abc, def], but the string >>
  // will read all characters between blanks. Proper string I/O handling
  // would solve this. Lines with /// were added/deleted
  if (how && ch != '[') {
    s.putback(ch);
    s >> r;
    if (!s.good()) {
      s.clear(std::ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
      how = false;
    } else {
      if (x.size() <= cnt) {
        x.resize(2*x.size() + 1);
      }
      x[cnt] = r;
      cnt++;
    }
  } else {
    std::string st;	///
    std::string sts;	///
    while (how) {
      s >> std::ws;
      s.get(ch);
      if (ch == ',') {
        s >> std::ws;
      } else if (ch == ']' || (int)ch == EOF) {
        break;
      } else {
        s.putback(ch);
      }
      if (std::is_base_of<std::string, T>::value) {	/// all of this extra
        s >> st;	/// Read string
        bool hasEither = st.find_first_of(",]")!=std::string::npos;
        if (hasEither) {
          size_t ix;
          while ((ix = st.find(',')) != std::string::npos) {
            sts = st.substr(0, ix);
            std::istringstream ins(sts);
            ins >> r;
            st = st.substr(ix+1);
            if (x.size() <= cnt) {
              x.resize(2*x.size() + 1);
            }
            x[cnt] = r;
            cnt++;
          }
          if ((ix = st.find(']')) != std::string::npos) {
            sts = st.substr(0, ix);
            std::istringstream ins(sts);
            ins >> r;
            st = st.substr(ix);
            if (x.size() <= cnt) {
              x.resize(2*x.size() + 1);
            }
            x[cnt] = r;
            cnt++;
            for (int i1=st.length()-1; i1>=0; i1--) {	/// set back
              s.putback(st[i1]);	///
            }			///
            break;
          }	  
          for (int i1=st.length()-1; i1>=0; i1--) {	/// set back
            s.putback(st[i1]);	///
          }			///
        } else {
          std::istringstream ins(st); /// Necessary for template
          ins >> r;				/// expansion
          if (x.size() <= cnt) {
            x.resize(2*x.size() + 1);
          }
          x[cnt] = r;
          cnt++;
        }
        if (!s.good()) {
          s.clear(std::ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
          how = false;
        }
      } else {
        s >> r;
        if (!s.good()) {
          s.clear(std::ios::failbit|s.rdstate()); // Redundant if using GNU iostreams
          how = false;
        } else {
          if (x.size() <= cnt) {
            x.resize(2*x.size() + 1);
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
    } else if ((long long)(cnt) != p.product()) {
      how = false;
    }
  }
  if (!how) {
    s.clear(std::ios::failbit);
    p.resize(0);
  }
  return how;
}
                    
}

#endif
