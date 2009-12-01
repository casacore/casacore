//# ArrayOpsDiffShapes.tcc: Operations for 2 Arrays with possibly different shapes.
//# Copyright (C) 2009
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
//#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/IPosition.h>
//#include <casa/Arrays/Slice.h>
#include <casa/Arrays/ArrayError.h>
//#include <casa/BasicSL/String.h>

namespace casa {

template<typename T>
LogicalArray reformedMask(const Array<T>& data, const T truthvalue,
			  const IPosition& desiredform)
{
  if(data.shape().nelements() == desiredform.nelements()
     && data.shape() == desiredform){
    return (data == truthvalue);
  }
  else if(static_cast<Int>(data.nelements()) == desiredform.product()){
    return (data == truthvalue).reform(desiredform);
  }
  else{
    if(rightExpandableToLeft(data.shape(), desiredform)){
      uInt n_data_dim = data.shape().nelements();
      uInt n_desired_dim = desiredform.nelements();

      // Create an array with desiredform's shape,
      LogicalArray collapseddata(desiredform);
      ReadOnlyArrayIterator<T> data_cursor(data,
					   IPosition::otherAxes(n_data_dim,
					      IPosition::makeAxisPath(n_desired_dim)));
      IPosition collapsedPos;
	
      // Go through each position in the new array,
      for(ArrayPositionIterator positer(desiredform, 0);
	  !positer.pastEnd(); positer.next()){
	collapsedPos = positer.pos();
	  
	data_cursor.set(collapsedPos);
	collapseddata(collapsedPos) = anyEQ(data_cursor.array(), truthvalue);
      }
	
      //// Apparently it would be expensive to put this inside the for statement.
      //LogicalArray::iterator iterend(collapseddata.end());
      
      // for(LogicalArray::iterator it = collapseddata.begin(); it != iterend;
      // 		    ++it)
      // 	  *it = data(it.itsCurPos).anyEQ(truthvalue);
    
      return collapseddata;
    }
    else{
      ostringstream os;
      
      os << "reformedMask(): Could not reconcile the input shape ("
	 << data.shape() << ")\n"
	 << "with the output shape (" << desiredform << ").";
      throw(ArrayConformanceError(os));
    }
  }
}

// template<typename L, typename R, typename BinaryOperator, typename RES>
// Array<RES> binOpExpandR(const Array<L>& left, const Array<R>& right,
// 			BinaryOperator op)
// {
//   const IPosition leftShape = left.shape();
//   Array<RES> res(leftShape);

//   if(left.conform(right)){
//     arrayContTransform(left, right, res, op);
//   }
//   else if(left.nelements() == right.nelements()){
//     arrayContTransform(left, right.reform(leftShape), res, op);
//   }
//   else{
//     const IPosition rightShape = right.shape();

//     if(rightExpandableToLeft(leftShape, rightShape)){
//       uInt n_right_dim = rightShape.nelements();
//       ArrayIterator<L>   left_cursor(left, n_right_dim);
//       ArrayIterator<RES> res_cursor(res, n_right_dim);
//       IPosition rightPos;
	
//       // Go through each position in the new array,
//       for(ArrayPositionIterator positer(rightShape, 0);
// 	  !positer.pastEnd(); positer.next()){
// 	rightPos = positer.pos();
// 	left_cursor.set(rightPos);
// 	res_cursor.set(rightPos);

// 	// "resChunk op= rightEntry"
// 	arrayContTransform(left_cursor.array(), right(rightPos),
// 			   res_cursor.array(), op);
//       }
//     }
//     else{
//       ostringstream os;  // String(rightShape) is not supported.

//       os << "binOpExpandR(): right's shape (" << rightShape << ")\n"
// 	 << " has more dimensions than left's (" << leftShape << ")!";
      
//       throw(ArrayConformanceError(os));
//     }
//   }
//   return res;
// }

template<typename L, typename R, typename BinaryOperator>
void binOpExpandInPlace(Array<L>& leftarr, const Array<R>& rightarr, BinaryOperator op)
{
  const IPosition leftShape  = leftarr.shape();
  const IPosition rightShape = rightarr.shape();

  // leftarr.conform(rightarr) fails if e.g. L == Double and R == Float.
  if(leftShape.nelements() == rightShape.nelements() && leftShape == rightShape){
    arrayTransformInPlace(leftarr, rightarr, op);  // Autochecks contiguity.
  }
  else if(leftarr.nelements() == rightarr.nelements()){
    arrayTransformInPlace(leftarr, rightarr.reform(leftShape), op);
  }
  else{
    uInt n_right_dim = rightShape.nelements();

    if(rightExpandableToLeft(leftShape, rightShape)){
      IPosition iteraxes(IPosition::otherAxes(leftShape.nelements(),
					      IPosition::makeAxisPath(n_right_dim)));
      ArrayIterator<L> left_cursor(leftarr, iteraxes);
      IPosition rightPos;
	
      // Go through each position in the new array,
      for(ArrayPositionIterator positer(rightShape, 0);
	  !positer.pastEnd(); positer.next()){
	rightPos = positer.pos();
	left_cursor.set(rightPos);

	// "leftChunk op= rightEntry"
	arrayTransformInPlace(left_cursor.array(), rightarr(rightPos), op);
      }
    }
    else{
      ostringstream os;  // String(rightShape) is not supported.
      
      os << "binOpExpandInPlace(): rightarr's shape (" << rightShape << ")\n"
	 << " has more dimensions than leftarr's (" << leftShape << ")!";
      
      throw(ArrayConformanceError(os));
    }
  }
}

} //#End casa namespace
