//# ISMBucket.cc: A bucket in the Incremental Storage Manager
//# Copyright (C) 1996,1997,1999,2001,2002
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

//# Includes
#include <casacore/tables/DataMan/ISMBucket.h>
#include <casacore/tables/DataMan/ISMBase.h>
#include <casacore/tables/DataMan/ISMColumn.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ISMBucket::ISMBucket (ISMBase* parent, const char* bucketStorage)
: stmanPtr_p (parent),
  uIntSize_p (parent->uIntSize()),
  dataLeng_p (0),
  indexLeng_p(0),
  rowIndex_p (parent->ncolumn(), static_cast<Block<uInt>*>(0)),
  offIndex_p (parent->ncolumn(), static_cast<Block<uInt>*>(0)),
  indexUsed_p(parent->ncolumn(), (uInt)0)
{
    uInt nrcol = stmanPtr_p->ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	rowIndex_p[i] = new Block<uInt>;
	offIndex_p[i] = new Block<uInt>;
    }
    // Get the initial index length.
    // This consists of the offset at the beginning of the bucket
    // and #entries for each column.
    indexLeng_p = uIntSize_p + nrcol * uIntSize_p;
    // Allocate a buffer for the data.
    data_p = new char[stmanPtr_p->bucketSize()];
    AlwaysAssert (data_p != 0, AipsError);
    // Read the row index for all columns (for an existing bucket).
    if (bucketStorage != 0) {
	read (bucketStorage);
    }
}

ISMBucket::~ISMBucket()
{
    uInt nrcol = stmanPtr_p->ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	delete rowIndex_p[i];
	delete offIndex_p[i];
    }
    delete [] data_p;
}

void ISMBucket::copy (const ISMBucket& that)
{
    dataLeng_p  = that.dataLeng_p;
    indexLeng_p = that.indexLeng_p;
    indexUsed_p = that.indexUsed_p;
    uInt nrcol = stmanPtr_p->ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	uInt nused = indexUsed_p[i];
	rowIndex_p[i]->resize (nused);
	offIndex_p[i]->resize (nused);
	for (uInt j=0; j<nused; j++) {
	    (*(rowIndex_p[i]))[j] = (*(that.rowIndex_p[i]))[j];
	    (*(offIndex_p[i]))[j] = (*(that.offIndex_p[i]))[j];
	}
    }
    memcpy (data_p, that.data_p, dataLeng_p);
}


uInt& ISMBucket::getOffset (uInt colnr, uInt rownr)
{
    Bool found;
    uInt inx = binarySearchBrackets (found, *(rowIndex_p[colnr]),
				     rownr, indexUsed_p[colnr]);
    // If no exact match, start of interval is previous index.
    if (!found) {
	inx--;
    }
    return (*(offIndex_p[colnr]))[inx];
}

uInt ISMBucket::getInterval (uInt colnr, uInt rownr, uInt bucketNrrow,
			     uInt& start, uInt& end, uInt& offset) const
{
    Block<uInt>& rowIndex = *(rowIndex_p[colnr]);
    Bool found;
    uInt inx = binarySearchBrackets (found, rowIndex,
				     rownr, indexUsed_p[colnr]);
    uInt index = inx;
    // If no exact match, start of interval is previous index.
    if (!found) {
	inx--;
    }
    offset = (*(offIndex_p[colnr]))[inx];
    start = rowIndex[inx];
    // End of interval is start of next interval, but it is the last
    // row in the bucket if it is the last interval.
    inx++;
    if (inx == indexUsed_p[colnr]) {
	end = bucketNrrow;
    }else{
	end = rowIndex[inx];
    }
    end--;
    return index;
}


Bool ISMBucket::canReplaceData (uInt newLeng, uInt oldLeng) const
{
    if (dataLeng_p + newLeng - oldLeng + indexLeng_p <=
	                                         stmanPtr_p->bucketSize()) {
	return True;
    }
    return False;
}
void ISMBucket::replaceData (uInt& offset, const char* data, uInt newLeng,
			     uInt oldLeng)
{
#ifdef AIPS_TRACE
    cout << "  replace at offset "<<offset<< ": oldleng="<<oldLeng<<", new="<<newLeng<<endl;
#endif
    AlwaysAssert (dataLeng_p + newLeng - oldLeng + indexLeng_p
		  <= stmanPtr_p->bucketSize(), AipsError);
    if (oldLeng == newLeng) {
	memcpy (data_p+offset, data, newLeng);
    }else{
	removeData (offset, oldLeng);
	offset = insertData (data, newLeng);
#ifdef AIPS_TRACE
    cout << "       new offset = "<<offset<<endl;
#endif
    }
}


Bool ISMBucket::canAddData (uInt leng) const
{
    if (dataLeng_p + leng + indexLeng_p + 2*uIntSize_p <=
	                                         stmanPtr_p->bucketSize()) {
	return True;
    }
    return False;
}

void ISMBucket::addData (uInt colnr, uInt rownr, uInt index,
			 const char* data, uInt leng)
{
#ifdef AIPS_TRACE
    cout << "  add at index "<< index<<endl;
#endif
    Block<uInt>& rowIndex = *(rowIndex_p[colnr]);
    Block<uInt>& offIndex = *(offIndex_p[colnr]);
    uInt nrused = indexUsed_p[colnr];
    DebugAssert ((index == 0  ||  rowIndex[index-1] < rownr)  &&
		 (index <= nrused)  &&
		 (index == nrused  ||  rowIndex[index] >= rownr), AipsError);
    // Extend blocks if needed.
    if (offIndex.nelements() <= nrused) {
	rowIndex.resize (nrused + 32);
	offIndex.resize (nrused + 32);
    }
    // Increment row if the same row is being added.
    if (index < nrused  &&  rownr == rowIndex[index]) {
	rowIndex[index]++;
    }
    // Shift to the right.
    for (uInt i=nrused; i>index; i--) {
	rowIndex[i] = rowIndex[i-1];
	offIndex[i] = offIndex[i-1];
    }
    // Insert the new row number.
    indexLeng_p += 2*uIntSize_p;
    indexUsed_p[colnr]++;
    rowIndex[index] = rownr;
    offIndex[index] = insertData (data, leng);
}


uInt ISMBucket::getLength (uInt fixedLength, const char* data) const
{
    if (fixedLength != 0) {
	return fixedLength;
    }
    // Get the data item length if it is variable.
    uInt leng;
    Conversion::ValueFunction* readuInt =
	                  ISMColumn::getReaduInt (stmanPtr_p->asBigEndian());
    readuInt (&leng, data, 1);
    return leng;
}


void ISMBucket::shiftLeft (uInt index, uInt nr, Block<uInt>& rowIndex,
			   Block<uInt>& offIndex, uInt& nused, uInt leng)
{
#ifdef AIPS_TRACE
    cout<<"   shift left "<<nr<<" elements"<<endl;
#endif
    // First remove the data items.
    for (uInt i=0; i<nr; i++) {
	removeData (offIndex[index+i], leng);
    }
    // Now shift row numbers and offsets to the left.
    // Decrement the index length.
    if (nused > index + nr) {
      objmove (&rowIndex[index], &rowIndex[index+nr], nused - index - nr);
      objmove (&offIndex[index], &offIndex[index+nr], nused - index - nr);
    }
    indexLeng_p -= 2 * nr * uIntSize_p;
    nused -= nr;
}

void ISMBucket::removeData (uInt offset, uInt leng)
{
    // Get the data item length if it is variable.
    leng = getLength (leng, data_p + offset);
    // Remove the data and decrease the length.
    dataLeng_p -= leng;
#ifdef AIPS_TRACE
    cout<<"    removed " <<leng<<" bytes at " << offset << endl;
#endif
    // The real remove is only necesarry if not at the end of the bucket.
    if (dataLeng_p > offset) {
	memmove (data_p + offset, data_p + offset + leng, dataLeng_p - offset);
	// Decrement the offset of all other items following this one.
	uInt nrcol = offIndex_p.nelements();
	for (uInt i=0; i<nrcol; i++) {
	    Block<uInt>& offIndex = *(offIndex_p[i]);
	    for (uInt j=0; j<indexUsed_p[i]; j++) {
		if (offIndex[j] > offset) {
		    offIndex[j] -= leng;
		}
	    }
	}
    }
}

uInt ISMBucket::insertData (const char* data, uInt leng)
{
    AlwaysAssert (dataLeng_p + leng + indexLeng_p <= stmanPtr_p->bucketSize(),
		  AipsError);
    memcpy (data_p + dataLeng_p, data, leng);
    uInt offset = dataLeng_p;
    dataLeng_p += leng;
#ifdef AIPS_TRACE
    cout<<"  inserted "<<leng<<" bytes at "<<offset << endl;
#endif
    return offset;
}


char* ISMBucket::readCallBack (void* owner, const char* bucketStorage)
{
    ISMBucket* bucket = new ISMBucket ((ISMBase*)owner, bucketStorage);
    AlwaysAssert (bucket != 0, AipsError);
    return (char*)bucket;
}
void ISMBucket::writeCallBack (void*, char* bucketStorage, const char* local)
{
    ((ISMBucket*)local)->write (bucketStorage);
}
void ISMBucket::deleteCallBack (void*, char* bucket)
{
    delete (ISMBucket*)bucket;
}
char* ISMBucket::initCallBack (void* owner)
{
    ISMBucket* bucket = new ISMBucket ((ISMBase*)owner, 0);
    AlwaysAssert (bucket != 0, AipsError);
    return (char*)bucket;
}

void ISMBucket::write (char* bucketStorage) const
{
    uInt nrcol = stmanPtr_p->ncolumn();
    Conversion::ValueFunction* writeuInt =
	                  ISMColumn::getWriteuInt (stmanPtr_p->asBigEndian());
    // The index will be written just after the data.
    uInt offset = dataLeng_p + uIntSize_p;
    writeuInt (bucketStorage, &offset, 1);
    // Copy the data.
    memcpy (bucketStorage + uIntSize_p, data_p, dataLeng_p);
    // Write the index.
    for (uInt i=0; i<nrcol; i++) {
	offset += writeuInt (bucketStorage+offset, &(indexUsed_p[i]), 1);
	uInt nr = indexUsed_p[i];
	offset += writeuInt (bucketStorage+offset,
			     rowIndex_p[i]->storage(), nr);
	offset += writeuInt (bucketStorage+offset,
			     offIndex_p[i]->storage(), nr);
    }
    // Do an extra validity check.
    AlwaysAssert (dataLeng_p + indexLeng_p == offset  &&
		  offset <= stmanPtr_p->bucketSize(), AipsError);
}

void ISMBucket::read (const char* bucketStorage)
{
    uInt nrcol = stmanPtr_p->ncolumn();
    Conversion::ValueFunction* readuInt =
	                  ISMColumn::getReaduInt (stmanPtr_p->asBigEndian());
    // Get the offset of the index.
    uInt offset;
    readuInt (&offset, bucketStorage, 1);
    // Copy the data, which are just before the index.
    dataLeng_p = offset - uIntSize_p;
    memcpy (data_p, bucketStorage + uIntSize_p, dataLeng_p);
    // Read the index.
    for (uInt i=0; i<nrcol; i++) {
	offset += readuInt (&(indexUsed_p[i]), bucketStorage+offset, 1);
	uInt nr = indexUsed_p[i];
	rowIndex_p[i]->resize (nr);
	offIndex_p[i]->resize (nr);
	offset += readuInt (rowIndex_p[i]->storage(),
			    bucketStorage+offset, nr);
	offset += readuInt (offIndex_p[i]->storage(),
			    bucketStorage+offset, nr);
    }
    // Calculate length of index (in external format).
    indexLeng_p = offset - dataLeng_p;
}


Bool ISMBucket::simpleSplit (ISMBucket* left, ISMBucket* right,
			     Block<Bool>& duplicated,
			     uInt& splitRownr, uInt rownr)
{
    // Determine the last rownr in the bucket.
    uInt i, row;
    uInt lastRow = 0;
    uInt nrcol = stmanPtr_p->ncolumn();
    for (i=0; i<nrcol; i++) {
	row = (*(rowIndex_p[i]))[indexUsed_p[i]-1];
        if (row > lastRow) {
	    lastRow = row;
	}
    }
    // Don't do a simple split if the row is not the last row in the bucket.
    if (rownr < lastRow) {
	return False;
    }
    // The last values of the bucket are the starting values of the
    // right one, so copy them.
    // The left bucket is this bucket.
    // Remove the last value from the left if the rownr is in the bucket. 
    left->copy (*this);
    for (i=0; i<nrcol; i++) {
	uInt index = indexUsed_p[i] - 1;
	row = (*(rowIndex_p[i]))[index];
	copyData (*right, i, 0, index, 0);
	duplicated[i] = True;
	if (row == rownr) {
	    left->shiftLeft (index, 1,
			     left->rowIndex(i), left->offIndex(i),
			     left->indexUsed(i),
			     stmanPtr_p->getColumn(i).getFixedLength());
	    duplicated[i] = False;
	}
    }
    splitRownr = rownr;
#ifdef AIPS_TRACE
    cout << "Simple split ";
    cout << "Original" << endl;
    show (cout);
    cout << "Left" << endl;
    left->show (cout);
    cout << "Right" << endl;
    right->show (cout);
#endif
    return True;
}

uInt ISMBucket::split (ISMBucket*& left, ISMBucket*& right,
		       Block<Bool>& duplicated,
		       uInt bucketStartRow, uInt bucketNrrow,
		       uInt colnr, uInt rownr, uInt lengToAdd)
{
    uInt nrcol = stmanPtr_p->ncolumn();
    duplicated.resize (nrcol);
    left  = new ISMBucket (stmanPtr_p, 0);
    right = new ISMBucket (stmanPtr_p, 0);
    uInt splitRownr;
    // Try a simple split if the current bucket is the last one.
    // (Then we usually add to the end of the file).
    if (bucketStartRow + bucketNrrow >= stmanPtr_p->nrow()) {
	if (simpleSplit (left, right, duplicated, splitRownr, rownr)) {
	    return splitRownr;
	}
    }
    // Count the number of values in all columns.
    uInt i, j;
    uInt nr = 0;
    for (i=0; i<nrcol; i++) {
	nr += indexUsed_p[i];
    }
    // Create a block containing the row numbers of all
    // values in all columns. Include the new item.
    Block<uInt> rows(nr + 1);
    rows[0] = rownr;               // new item
    nr = 1;
    for (i=0; i<nrcol; i++) {
	for (j=0; j<indexUsed_p[i]; j++) {
	    rows[nr++] = (*rowIndex_p[i])[j];
	}
    }
    // Sort it (uniquely) to get all row numbers with a value.
    uInt nruniq = GenSort<uInt>::sort (rows, rows.nelements(), 
				       Sort::Ascending, Sort::NoDuplicates);
    // If the bucket contains values of only one row, a simple split
    // can be done (and should succeed).
    if (nruniq == 1) {
	Bool split = simpleSplit (left, right, duplicated, splitRownr, rownr);
	AlwaysAssert (split, AipsError);
	return splitRownr;
    }
    // Now get the length of all data items in the rows.
    // Also determine the index of the row to be added.
    Matrix<uInt> itemLeng(nrcol, nruniq);
    itemLeng = 0;
    Block<uInt> cursor(nrcol, uInt(0));
    uInt index = 0;
    for (j=0; j<nruniq; j++) {
	for (i=0; i<nrcol; i++) {
	    if (cursor[i] < indexUsed_p[i]
	    &&  (*rowIndex_p[i])[cursor[i]] == rows[j]) {
		uInt leng = getLength (
		                  stmanPtr_p->getColumn(i).getFixedLength(),
		                  data_p + (*offIndex_p[i])[cursor[i]]);
		itemLeng(i,j) = 2*uIntSize_p + leng;
		cursor[i]++;
	    }
	}
	if (rownr == rows[j]) {
	    index = j;
	}
    }
    // Insert the length of the new item.
    // If it is a new item, add the index length too.
    if (itemLeng(colnr, index) == 0) {
	itemLeng(colnr, index) = lengToAdd + 2*uIntSize_p;
    }else{
	itemLeng(colnr, index) += lengToAdd;
    }
    // Now determine the length of all items in each row.
    // Determine the cumulative and total size.
    Block<uInt> size(nrcol, uInt(0));
    Block<uInt> rowLeng(nruniq, uInt(0));
    Block<uInt> cumLeng(nruniq);
    uInt totLeng = 0;
    for (j=0; j<nruniq; j++) {
	for (i=0; i<nrcol; i++) {
	    if (itemLeng(i,j) != 0) {
		size[i]  = itemLeng(i,j);
		totLeng += itemLeng(i,j);
	    }
	    rowLeng[j] += size[i];
	}
	cumLeng[j] = totLeng;
    }
    // Get the index where splitting results in two parts with
    // almost equal length.
    index = getSplit (totLeng, rowLeng, cumLeng);
    // Now copy values until the split index.
    // Maintain a cursor block to keep track of the row processed for
    // each column. A row has to be copied completely, because a row
    // cannot be split over multiple buckets.
    cursor = 0;
    uInt row;
    for (j=0; j<index; j++) {
	row = rows[j];
	for (i=0; i<nrcol; i++) {
	    if (cursor[i] < indexUsed_p[i]
            &&  (*rowIndex_p[i])[cursor[i]] == row) {
		copyData (*left, i, row, cursor[i], cursor[i]);
		cursor[i]++;
	    }
	}
    }
    // Copy the rest to the right bucket.
    // Start with filling in the start values for that block.
    // Take from this index if the row number matches, otherwise
    // from the previous index. Fill the duplicate switch.
    splitRownr = rows[index];
    for (i=0; i<nrcol; i++) {
	if (cursor[i] < indexUsed_p[i]
        &&  (*rowIndex_p[i])[cursor[i]] == splitRownr) {
	    copyData (*right, i, 0, cursor[i], 0);
	    cursor[i]++;
	    duplicated[i] = False;
	}else{
	    copyData (*right, i, 0, cursor[i]-1, 0);
	    duplicated[i] = True;
	}
    }
    // Now copy the rest of the values.
    Block<uInt> toCursor(nrcol, 1);
    index++;
    while (index < nruniq) {
	row = rows[index];
	for (i=0; i<nrcol; i++) {
	    if (cursor[i] < indexUsed_p[i]
            &&  (*rowIndex_p[i])[cursor[i]] == row) {
		copyData (*right, i, row - splitRownr,
			  cursor[i], toCursor[i]);
		cursor[i]++;
		toCursor[i]++;
	    }
	}
	index++;
    }
#ifdef AIPS_TRACE
    cout << "Original" << endl;
    show (cout);
    cout << "Left" << endl;
    left->show (cout);
    cout << "Right" << endl;
    right->show (cout);
#endif
    return splitRownr;
}

uInt ISMBucket::getSplit (uInt totLeng, const Block<uInt>& rowLeng,
			  const Block<uInt>& cumLeng)
{
    // If there are only 2 elements, we can only split in the middle.
    uInt nr = rowLeng.nelements();
    if (nr <= 2) {
	return 1;
    }
    // Determine the index where left and right have about the same size.
    // totLeng = length of all values. This includes the starting values.
    // rowLeng = length of all values in a row. This gives the length
    //           of the starting values if the bucket starts at that row.
    // cumLeng = length of all values till the row with index i.
    //           cumLeng[0] = length of the starting values in first row.
    // If i is the index where the bucket is split, then:
    //   length of left  bucket = cumLeng[i-1].
    //   length of right bucket = rowLeng[i] + totleng - cumLeng[i]
    // Loop until left size exceeds right size or until we get at the
    // rightmost index.
    uInt i=1;
    uInt diff = 0;
    while (cumLeng[i-1]  <  rowLeng[i] + totLeng - cumLeng[i]  &&  i<nr-1) {
	diff = rowLeng[i] + totLeng - cumLeng[i] - cumLeng[i-1];
	i++;
    }
    // Now look if the current index results in a greater difference
    // between left and right. If so, split at previous index.
    if (diff > 0) {
	if (cumLeng[i-1] + cumLeng[i] - rowLeng[i] - totLeng  >  diff) {
	    i--;
	}
    }
    return i;
}


uInt ISMBucket::copyData (ISMBucket& other, uInt colnr, uInt toRownr,
			  uInt fromIndex, uInt toIndex) const
{
    // Determine the length of the data item.
    // If variable, read it from the data.
    char* data = data_p + (*offIndex_p[colnr])[fromIndex];
    uInt leng = getLength (stmanPtr_p->getColumn(colnr).getFixedLength(),
			   data);
    other.addData (colnr, toRownr, toIndex, data, leng);
    return leng;
}


void ISMBucket::show (ostream& os) const
{
    uInt nrcol = stmanPtr_p->ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	cout << "  rows: ";
	showBlock (os, *(rowIndex_p[i]), indexUsed_p[i]);
	cout << endl;
	cout << "  offs: ";
	showBlock (os, *(offIndex_p[i]), indexUsed_p[i]);
	cout << endl;
    }
}

Bool ISMBucket::check (uInt &offendingCol, uInt &offendingIndex,
                       uInt &offendingRow, uInt &offendingPrevRow) const
{
  uInt ncols = stmanPtr_p->ncolumn();
  for (uInt col_i=0; col_i<ncols; ++col_i) {
    for (uInt it=1; it<indexUsed_p[col_i]; ++it) {
      if ( (*(rowIndex_p[col_i]))[it] <= (*(rowIndex_p[col_i]))[it-1] ) {
        offendingCol = col_i;
        offendingIndex = it;
        offendingRow = (*(rowIndex_p[col_i]))[it];
        offendingPrevRow = (*(rowIndex_p[col_i]))[it-1];
        return False;
      }
    }
  }
  return True;
}

} //# NAMESPACE CASACORE - END

