//# Sort.tcc: Sort objects on one or more keys
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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

#ifndef CASA_SORT_TCC
#define CASA_SORT_TCC

//# Includes
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/SortError.h>
#include <casacore/casa/Arrays/ArrayMath.h>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template<typename T>
  T Sort::doSort (Vector<T>& indexVector, T nrrec, int opt,
                  bool doTryGenSort) const
  {
    if (nrrec == 0) {
      return nrrec;
    }
    //# Try if we can use the faster GenSort when we have one key only.
    if (doTryGenSort  &&  nrkey_p == 1) {
      uint32_t n = keys_p[0]->tryGenSort (indexVector, nrrec, opt);
      if (n > 0) {
        return n;
      }
    }
    indexVector.resize (nrrec);
    indgen (indexVector);
    // Pass the sort function a C-array of indices, because indexing
    // in there is (much) faster than in a vector.
    bool del;
    T* inx = indexVector.getStorage (del);
    // Choose the sort required.
    int nodup = opt & NoDuplicates;
    int type  = opt - nodup;
    // Determine default sort to use.
    int nthr = 1;
#ifdef _OPENMP
    nthr = omp_get_max_threads();
    // Do not use more threads than there are values.
    if (uint32_t(nthr) > nrrec) nthr = nrrec;
#endif
    if (type == DefaultSort) {
      type = (nrrec<1000 || nthr==1  ?  QuickSort : ParSort);
    }
    T n = 0;
    switch (type) {
    case QuickSort:
      if (nodup) {
        n = quickSortNoDup (nrrec, inx);
      }else{
        n = quickSort (nrrec, inx);
      }
      break;
    case HeapSort:
      if (nodup) {
        n = heapSortNoDup (nrrec, inx);
      }else{
        n = heapSort (nrrec,inx);
      }
      break;
    case InsSort:
      if (nodup) {
        n = insSortNoDup (nrrec, inx);
      }else{
        n = insSort (nrrec, inx);
      }
      break;
    case ParSort:
      n = parSort (nthr, nrrec, inx);
      if (nodup) {
        n = insSortNoDup (nrrec, inx);
      }
      break;
    default:
      throw SortInvOpt();
    }
    indexVector.putStorage (inx, del);
    // If n < nrrec, some duplicates have been deleted.
    // This means we have to resize the Vector.
    if (n < nrrec) {
      indexVector.resize (n, true);
    }
    return n;
  }

  template<typename T>
  T Sort::doUnique (Vector<T>& uniqueVector, T nrrec) const
  {
    Vector<T> indexVector(nrrec);
    indgen (indexVector);
    return doUnique (uniqueVector, indexVector);
  }

  template<typename T>
  T Sort::doUnique (Vector<T>& uniqueVector, const Vector<T>& indexVector) const
  {
    Vector<size_t> changeKey;
    return doUnique (uniqueVector, changeKey, indexVector);
  }

  template<typename T>
  T Sort::doUnique (Vector<T>& uniqueVector,
                    Vector<size_t>& changeKey, 
                    const Vector<T>& indexVector) const
  {
    T nrrec = indexVector.nelements();
    uniqueVector.resize (nrrec);
    changeKey.resize (nrrec);
    if (nrrec == 0) {
      return 0;
    }
    // Pass the sort function a C-array of indices, because indexing
    // in there is (much) faster than in a vector.
    bool delInx, delUniq, delChange;
    const T* inx = indexVector.getStorage (delInx);
    T* uniq = uniqueVector.getStorage (delUniq);
    size_t* change = changeKey.getStorage (delChange);
    uniq[0] = 0;
    T nruniq = 1;
    size_t idxComp;
    for (T i=1; i<nrrec; i++) {
      int32_t cmp = compareChangeIdx (inx[i-1], inx[i], idxComp);
      if (cmp != 1  &&  cmp != -1) {
        change[nruniq-1] = idxComp;
        uniq[nruniq++] = i;
      }
    }
    indexVector.freeStorage (inx, delInx);
    uniqueVector.putStorage (uniq, delUniq);
    changeKey.putStorage (change, delChange);
    if (nruniq < nrrec) {
      uniqueVector.resize (nruniq, true);
      changeKey.resize (nruniq, true);
    }
    return nruniq;
  }

  template<typename T>
  T Sort::parSort (int nthr, T nrrec, T* inx) const
  {
    Block<T> index(nrrec+1);
    Block<T> tinx(nthr+1);
    Block<T> np(nthr);
    // Determine ordered parts in the array.
    // It is done in parallel, whereafter the parts are combined.
    int step = nrrec/nthr;
    for (int i=0; i<nthr; ++i) tinx[i] = i*step;
    tinx[nthr] = nrrec;
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (int i=0; i<nthr; ++i) {
      int nparts = 1;
      index[tinx[i]] = tinx[i];
      for (T j=tinx[i]+1; j<tinx[i+1]; ++j) {
        if (compare (inx[j-1], inx[j]) <= 0) {
          index[tinx[i]+nparts] = j;    // out of order, thus new part
          nparts++;
        }
      }
      np[i] = nparts;
    }
    // Make index parts consecutive by shifting to the left.
    // See if last and next part can be combined.
    T nparts = np[0];
    for (int i=1; i<nthr; ++i) {
      if (compare (tinx[i]-1, tinx[i]) <= 0) {
        index[nparts++] = index[tinx[i]];
      }
      if (nparts == tinx[i]+1) {
        nparts += np[i]-1;
      } else {
        for (T j=1; j<np[i]; ++j) {
          index[nparts++] = index[tinx[i]+j];
        }
      }
    }
    index[nparts] = nrrec;
    //cout<<"nparts="<<nparts<<endl;
    // Merge the array parts. Each part is ordered.
    if (nparts < nrrec) {
      Block<T> inxtmp(nrrec);
      merge (inx, inxtmp.storage(), nrrec, index.storage(), nparts);
    } else {
      // Each part has length 1, so the array is in reversed order.
      for (T i=0; i<nrrec; ++i) inx[i] = nrrec-1-i;
    }
    return nrrec;
  }  

  template<typename T>
  void Sort::merge (T* inx, T* tmp, T nrrec, T* index,
                    T nparts) const
  {
    T* a = inx;
    T* b = tmp;
    int np = nparts;
    // If the nr of parts is odd, the last part is not merged. To avoid having
    // to copy it to the other array, a pointer 'last' is kept.
    // Note that merging the previous part with the last part works fine, even
    // if the last part is in the same buffer.
    T* last = inx + index[np-1];
    while (np > 1) {
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
      for (int i=0; i<np; i+=2) {
        if (i < np-1) {
          // Merge 2 subsequent parts of the array.
          T* f1 = a+index[i];
          T* f2 = a+index[i+1];
          T* to = b+index[i];
          T na = index[i+1]-index[i];
          T nb = index[i+2]-index[i+1];
          if (i == np-2) {
            //cout<<"swap last np=" <<np<<endl;
            f2 = last;
            last = to;
          }
          T ia=0, ib=0, k=0;
          while (ia < na && ib < nb) {
            if (compare(f1[ia], f2[ib]) > 0) {
              to[k] = f1[ia++];
            } else {
              to[k] = f2[ib++];
            }
            k++;
          }
          if (ia < na) {
            for (T p=ia; p<na; p++,k++) to[k] = f1[p];
          } else {
            for (T p=ib; p<nb; p++,k++) to[k] = f2[p];
          }
        }
      }
      // Collapse the index.
      int k=0;
      for (int i=0; i<np; i+=2) index[k++] = index[i];
      index[k] = nrrec;
      np = k;
      // Swap the index target and destination.
      T* c = a;
      a = b;
      b = c;
    }
    // If final result happens to be in incorrect array, copy it over.
    if (a != inx) {
      objcopy (inx, a, nrrec);
    }
  }

  template<typename T>
  T Sort::insSort (T nrrec, T* inx) const
  {
    for (T i=1; i<nrrec; i++) {
      int64_t j = i;
      T cur = inx[i];
      while (--j>=0  &&  compare(inx[j], cur) <= 0) {
        inx[j+1] = inx[j];
      }
      inx[j+1] = cur;
    }
    return nrrec;
  }

  template<typename T>
  T Sort::insSortNoDup (T nrrec, T* inx) const
  {
    if (nrrec < 2) {
      return nrrec;                             // nothing to sort
    }
    T nr = 1;
    int cmp = 0;
    for (T i=1; i<nrrec; i++) {
      int64_t j = nr;
      T cur = inx[i];
      // Continue as long as key is out of order.
      while (--j>=0  &&  (cmp = compare (inx[j], cur)) == 0) {
      }
      if (j<0 || cmp==2) {                   // no equal key
        for (int64_t k=nr-1; k>j; k--) {
          inx[k+1] = inx[k];                 // now shift to right
        }
        inx[j+1] = cur;                      // insert in right place
        nr++;
      }
    }
    return nr;
  }


  template<typename T>
  T Sort::quickSort (T nrrec, T* inx) const
  {
    // Use the quicksort algorithm and improvements as described
    // in "Algorithms in C" by R. Sedgewick.
    // Small subsets are not sorted with qksort anymore, but
    // thereafter with insertion sort.
    qkSort (nrrec, inx);
    return insSort (nrrec, inx);
  }

  template<typename T>
  T Sort::quickSortNoDup (T nrrec, T* inx) const
  {
    qkSort (nrrec, inx);
    return insSortNoDup (nrrec, inx);
  }


  template<typename T>
  void Sort::qkSort (T nr, T* inx) const
  {
    // If the nr of elements to be sorted is less than N, it is
    // better not to use quicksort anymore (according to Sedgewick).
    // Take N=15, because that seems to work best after testing
    // N=5, 10, 15 and 20.
    if (nr <= 15) {
      return;
    }
    // According to Sedgewick it is best to use a random partition element
    // to avoid degenerated cases (if the data is already in order for example)
    // rand is not a particularly good random number generator, but good
    // enough for this purpose.
    // Put this element at the beginning of the array.
    T p = rand() % nr;
    swap (T(0), p, inx);
    // Now shift all elements < partition-element to the left.
    // If an element is equal, shift every other element to avoid
    // degeneration. This trick is described by Jon Bentley in
    // UNIX Review, October 1992.
    // We do not have equal elements anymore (because of the stability
    // property introduced on 13-Feb-1995).
    T j = 0;
    for (T i=1; i<nr; i++) {
      if (compare (inx[0], inx[i]) <= 0) {
        swap (i, ++j, inx);
      }
    }
    swap (T(0), j, inx);
    qkSort (j, inx);
    qkSort (nr-j-1, inx+j+1);
  }


  template<typename T>
  T Sort::heapSort (T nrrec, T* inx) const
  {
    // Use the heapsort algorithm described by Jon Bentley in
    // UNIX Review, August 1992.
    T j;
    inx--;
    for (j=nrrec/2; j>=1; j--) {
      siftDown (j, nrrec, inx);
    }
    for (j=nrrec; j>=2; j--) {
      swap (T(1), j, inx);
      siftDown (T(1), j-1, inx);
    }
    return nrrec;
  }

  template<typename T>
  T Sort::heapSortNoDup (T nrrec, T* inx) const
  {
    heapSort (nrrec, inx);
    return insSortNoDup (nrrec, inx);
  }

  template<typename T>
  void Sort::siftDown (T low, T up, T* inx) const
  {
    T sav = inx[low];
    T c;
    T i;
    for (i=low; (c=2*i)<=up; i=c) {
      if (c < up  &&  compare(inx[c+1], inx[c]) <= 0) {
        c++;
      }
      inx[i] = inx[c];
    }
    inx[i] = sav;
    for ( ; (c=i/2)>=low; i=c) {
      if (compare (inx[i], inx[c]) > 0) {
        break;
      }
      swap (c, i, inx);
    }
  }


  // Note that the block of SortKeys is defined as void*, to achieve
  // that only 1 type of Block<pointer> is needed.
  // Casting is perfectly save.
  // The comparison functions return:
  //   -1   when obj1 < obj2
  //    0   when obj1 = obj2
  //    1   when obj1 > obj2
  // compare returns:
  //    2   when data[i1],data[i2] is in correct order
  //        (thus data[i1] < data[i2] for ascending sort)
  //    1   when data is equal and indices are in order
  //    0   when data is out of order
  //   -1   when data is equal and indices are out of order
  template<typename T>
  int Sort::compare (T i1, T i2) const
  {
    size_t idxComp;
    return compareChangeIdx(i1, i2, idxComp);
  }

  // This is a similar function to compare() but it also gives back which is the
  // first comparison function that doesn't match.
  // idxComp gives the comparison function index. In case the function returns
  // 1 or -1 idxComp is not modified.
  template<typename T>
  int Sort::compareChangeIdx(T i1, T i2, size_t& idxComp) const
  {
    int seq;
    SortKey* skp;
    for (size_t i=0; i<nrkey_p; i++) {
      skp = keys_p[i];
      seq = skp->cmpObj_p->comp ((char*)skp->data_p + i1*skp->incr_p,
                                 (char*)skp->data_p + i2*skp->incr_p);
      if (seq == skp->order_p)
      {
        idxComp = i;
        return 2;                       // in order
      }
      if (seq != 0) {
        idxComp = i;
        return 0;                       // out-of-order
      }
    }
    // Equal keys, so return i1<i2 to maintain stability.
    if (i1<i2) {
      if (order_p == 1) return -1;          // desc, thus out-of-order
      return 1;                             // equal keys; in order
    }
    if (order_p == 1) return 1;
    return -1;                                // equal keys; out-of-order
  }


} //# NAMESPACE CASACORE - END

#endif
