#include "../Array.h"
#include "../Cube.h"
#include "../Slice.h"
#include "../Slicer.h"
#include "../Vector.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(cpp11_features)

void checkEmpty(const IPosition& ip)
{
  BOOST_CHECK_EQUAL(ip.empty(), true);
  BOOST_CHECK_EQUAL(ip.size(), 0);
}

void checkSmall(const IPosition& ip)
{
  BOOST_CHECK_EQUAL(ip.empty(), false);
  BOOST_CHECK_EQUAL(ip.size(), 2);
  BOOST_CHECK_EQUAL(ip[0], 2);
  BOOST_CHECK_EQUAL(ip[1], 3);
}

void checkLarge(const IPosition& ip)
{
  BOOST_CHECK_EQUAL(ip.empty(), false);
  BOOST_CHECK_EQUAL(ip.size(), 6);
  for(size_t i=0; i!=6; ++i)
    BOOST_CHECK_EQUAL(ip[i], i+2);
}

BOOST_AUTO_TEST_CASE( ip_initializerlist )
{
  IPosition ip{};
  checkEmpty(ip);
  
  IPosition ip1{2, 3};
  checkSmall(ip1);
  
  IPosition ip2{2, 3, 4, 5, 6, 7};
  checkLarge(ip2);
}

BOOST_AUTO_TEST_CASE( ip_move_constructor )
{
  IPosition ip1{2, 3};
  IPosition ip2(std::move(ip1));
  checkSmall(ip2);
  checkEmpty(ip1);
  
  IPosition ip3{2, 3, 4, 5, 6, 7};
  IPosition ip4(std::move(ip3));
  checkLarge(ip4);
  checkEmpty(ip3);
}

BOOST_AUTO_TEST_CASE( ip_move_assignment )
{
  IPosition ip1{2, 3}, ip2;
  ip2 = std::move(ip1);
  checkSmall(ip2);
  checkEmpty(ip1);
  
  IPosition ip3{2, 3, 4, 5, 6, 7}, ip4;
  ip4 = std::move(ip3);
  checkLarge(ip4);
  checkEmpty(ip3);
  
  ip4 = std::move(ip2);
  checkSmall(ip4);
  checkEmpty(ip2);
}

BOOST_AUTO_TEST_CASE( ip_non_conforming_copy_assignment )
{
  // Since the 2020 changes, this should no longer throw.
  IPosition ip1{2, 3};
  IPosition ip2{2, 3, 4, 5, 6, 7};
  ip1 = ip2; // normal copy assignment
  checkLarge(ip1);
  checkLarge(ip2);
}

BOOST_AUTO_TEST_CASE( array_move_constructor )
{
  IPosition ip{2, 3};
  Array<int> a(ip, 42);
  Array<int> b(std::move(a));
  Array<int> ref(ip, 42);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());
  Array<int> c(std::move(a));
  Array<int> empty;
  BOOST_CHECK_EQUAL_COLLECTIONS(c.begin(), c.end(), empty.begin(), empty.end());
}

BOOST_AUTO_TEST_CASE( array_iterate_empty )
{
  Array<int> empty1, empty2;
  BOOST_CHECK_EQUAL_COLLECTIONS(empty1.begin(), empty1.end(), empty2.begin(), empty2.end());
}

BOOST_AUTO_TEST_CASE( array_move_assignment )
{
  IPosition ip{2, 3};
  Array<int> a(ip, 42);
  Array<int> b;
  b = std::move(a);
  Array<int> ref(ip, 42);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());

  Array<int> c(IPosition{1,1}, 7); // incorrect shape
  BOOST_CHECK_THROW(c = std::move(b), std::runtime_error); // TODO it is questionable whether we would want this to throw!
  BOOST_CHECK_EQUAL(c(IPosition(2, 0, 0)), 7);
  
  Array<int> d(IPosition(0));
  Vector<int> e(IPosition{3}, 1982);
  d = std::move(e); // Move assigning to empty should not validate shape
  BOOST_CHECK_EQUAL(d.shape(), IPosition(1, 3));
}

BOOST_AUTO_TEST_CASE( array_move_case1 )
{
  Array<int> a(IPosition{2, 3}, 0);
  Array<int> b = a[1];
  b = 5;
  std::vector<int> refa{0, 0, 5, 5, 0, 0};
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), refa.begin(), refa.end());
  
  Slicer slicer(IPosition(1,0));
  Array<int> sliced = b(slicer);
  sliced = 1982;
  std::vector<int> refsliced1{0, 0, 1982, 5, 0, 0};
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), refsliced1.begin(), refsliced1.end());
  
  Array<int> d(IPosition{3}, 42);
  sliced = d(IPosition{0}, IPosition{0}, IPosition{1});
  std::vector<int> refsliced2{0, 0, 42, 5, 0, 0};
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), refsliced2.begin(), refsliced2.end());
  
  Array<int> e(IPosition{1}, 16);
  e = d(IPosition{0}, IPosition{0}, IPosition{1});
  BOOST_CHECK_EQUAL(e(IPosition(1, 0)), 42);
}

BOOST_AUTO_TEST_CASE( array_move_case2 )
{
  Array<int> a(IPosition{2, 3}, 2);
  Array<int> b(a);
  Array<int> c(IPosition{2, 3}, 3);
  c = std::move(a);
  IPosition el{1, 2};
  BOOST_CHECK_EQUAL(c(el), 2);
  BOOST_CHECK_EQUAL(b(el), 2);
  // Since the semantics are that operator= does copy value assignment,
  // b should not change when we change c
  c(el) = 4;
  BOOST_CHECK_EQUAL(b(el), 2);
  BOOST_CHECK_EQUAL(c(el), 4);
}

BOOST_AUTO_TEST_CASE( array_move_case3 )
{
  int storage = 1988;
  Array<int> a(IPosition{1, 1}, &storage, SHARE);
  Array<int> b(IPosition{1, 1}, 3);
  b = std::move(a);
  IPosition el{0,0};
  BOOST_CHECK_EQUAL(storage, 1988);
  BOOST_CHECK_EQUAL(a(el), 1988);
  BOOST_CHECK_EQUAL(b(el), 1988);
  b(el) = 4;
  BOOST_CHECK_EQUAL(storage, 1988);
  BOOST_CHECK_EQUAL(a(el), 1988);
  BOOST_CHECK_EQUAL(b(el), 4);
  storage = 0;
  BOOST_CHECK_EQUAL(storage, 0);
  BOOST_CHECK_EQUAL(a(el), 0);
  BOOST_CHECK_EQUAL(b(el), 4);
}

BOOST_AUTO_TEST_CASE( vector_move_case2 )
{
  Vector<int> a(IPosition{6}, 2);
  Vector<int> b(a);
  Vector<int> c(IPosition{6}, 3);
  c = std::move(a);
  BOOST_CHECK_EQUAL(c[5], 2);
  BOOST_CHECK_EQUAL(b[5], 2);
  // Since the semantics are that operator= does copy value assignment,
  // b should not change when we change c
  c[5] = 4;
  BOOST_CHECK_EQUAL(b[5], 2);
  BOOST_CHECK_EQUAL(c[5], 4);
}

BOOST_AUTO_TEST_CASE( array_swap )
{
  IPosition sa{2,3}, sb{4,4,4};
  Array<int> a(sa, 8), b(sb, 1982), c, d(sa, 37);
  casacore::swap(a, b);
  casacore::swap(b, c);
  BOOST_CHECK_EQUAL(a.shape(), sb);
  BOOST_CHECK(allEQ(a, 1982));
  BOOST_CHECK(b.empty());
  BOOST_CHECK_EQUAL(c.shape(), sa);
  BOOST_CHECK(allEQ(c, 8));
  std::swap(c, d); // Will use moves & temporary, but should still work.
  BOOST_CHECK_EQUAL(c.shape(), sa);
  BOOST_CHECK(allEQ(c, 37));
}

BOOST_AUTO_TEST_CASE( array_can_hold_noncopyable_objects )
{
  // Because Array now moves objects when possible, it can also store
  // non-copyable objects such as std::unique_ptr
  Array<std::unique_ptr<int>> a, b, c;
  //a = Array<std::unique_ptr<int>>(IPosition{2,3}, nullptr); // TODO this could be made possible but isn't yet
  a = Array<std::unique_ptr<int>>(IPosition{2,3});
  IPosition pos{1, 2};
  a(pos) = std::unique_ptr<int>(new int(42));
  b = std::move(a);
  BOOST_CHECK_EQUAL(*b(pos), 42);
  c.reference(b);
  BOOST_CHECK_EQUAL(*c(pos), 42);
  BOOST_CHECK_THROW(a.assign(b), ArrayError);
}

BOOST_AUTO_TEST_CASE( vector_can_hold_noncopyable_objects )
{
  Vector<std::unique_ptr<int>> a, b, c;
  a = Vector<std::unique_ptr<int>>(2);
  a[1] = std::unique_ptr<int>(new int(42));
  b = std::move(a);
  BOOST_CHECK_EQUAL(*b[1], 42);
  c.reference(b);
  BOOST_CHECK_EQUAL(*c[1], 42);
  BOOST_CHECK_THROW(a.assign(b), ArrayError);
}

BOOST_AUTO_TEST_CASE( vector_range_constructor )
{
  std::vector<int> stdvec1{1, 2, 3, 4, 5};
  Vector<int> v1(stdvec1.begin(), stdvec1.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(v1.begin(), v1.end(), stdvec1.begin(), stdvec1.end());
  
  std::vector<std::string> stdvec2{"a", "b"};
  Vector<std::string> v2(stdvec2.begin(), stdvec2.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(v2.begin(), v2.end(), stdvec2.begin(), stdvec2.end());
  
  std::vector<size_t> stdvec3{37, 38, 39, 40, 41};
  v1 = Vector<int>(stdvec3.begin(), stdvec3.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(v1.begin(), v1.end(), stdvec3.begin(), stdvec3.end());

  Vector<std::string> v3(v2.begin(), v2.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(v2.begin(), v2.end(), v3.begin(), v3.end());

  Vector<int> v4;
  int *a=nullptr, *b=nullptr;
  v4 = Vector<int>(a, b);
  BOOST_CHECK_EQUAL_COLLECTIONS(v4.begin(), v4.end(), a, b);
  v1.resize();
}

BOOST_AUTO_TEST_CASE( array_to_string )
{
  Array<int> a(IPosition{2, 3}, 2);
  a(IPosition{0, 1}) = 1;
  BOOST_CHECK_EQUAL(to_string(a),
    "Axis Lengths: [2, 3]  (NB: Matrix in Row/Column order)\n"
    "[2, 1, 2\n"
    " 2, 2, 2]\n");
}

BOOST_AUTO_TEST_CASE( masked_array_move_constructor )
{
  Array<int> arr(IPosition{2, 3}, 37);
  LogicalArray mask(IPosition{2, 3}, false);
  mask(IPosition{0, 1}) = true;
  MaskedArray<int> ma1(arr, mask);
  MaskedArray<int> ma2(std::move(ma1));

  std::vector<int> arrReference{37, 37, 37, 37, 37, 37};
  BOOST_CHECK_EQUAL_COLLECTIONS(ma2.getArray().begin(), ma2.getArray().end(), arrReference.begin(), arrReference.end());
  
  std::vector<bool> maskReference{false, false, true, false, false, false};
  BOOST_CHECK_EQUAL_COLLECTIONS(ma2.getMask().begin(), ma2.getMask().end(), maskReference.begin(), maskReference.end());
}

BOOST_AUTO_TEST_CASE( masked_array_move_assignment )
{
  Array<int> arr(IPosition{2, 3}, 37);
  LogicalArray mask(IPosition{2, 3}, false);
  mask(IPosition{0, 1}) = true;
  MaskedArray<int> ma1(arr, mask), ma2, ma3;
  ma2 = ma1;
  ma3 = std::move(ma1);

  BOOST_CHECK_EQUAL_COLLECTIONS(ma2.getArray().begin(), ma2.getArray().end(), ma3.getArray().begin(), ma3.getArray().end());
  BOOST_CHECK_EQUAL_COLLECTIONS(ma2.getMask().begin(), ma2.getMask().end(), ma3.getMask().begin(), ma3.getMask().end());
}

BOOST_AUTO_TEST_CASE( masked_array_assign_rvalue_array )
{
  MaskedArray<int> ma;
  IPosition ip{2, 3};
  ma = Array<int>(ip, 37);
  BOOST_CHECK_EQUAL(ma.shape(), ip);
  for(size_t i=0; i!=6; ++i) {
    BOOST_CHECK_EQUAL(ma.getArray()(IPosition(2, i/3, i%3)), 37);
    BOOST_CHECK_EQUAL(ma.getMask()(IPosition(2, i/3, i%3)), true);
  }
}

BOOST_AUTO_TEST_CASE( pointer_vector )
{
  // Testing this because it gave compiling errors at some point
  // Vector<ptr>(3, 0) was allowed, but is no longer, because 0 is not implicitly convertable to a pointer
  // Hence now we need this:
  Vector<Array<int>*> ptrArray(3, nullptr);
  for(auto iter=ptrArray.begin(); iter!=ptrArray.end(); ++iter)
    BOOST_CHECK(*iter == nullptr);
}

BOOST_AUTO_TEST_CASE( bool_vector_constructor )
{
  // The bool Vector constructor that takes a std::vector is specialized because
  // std::vector<bool> is different than other std::vectors. This is tested here.
  std::vector<bool> stdv{true, false, true};
  Vector<bool> v(stdv);
  BOOST_CHECK_EQUAL_COLLECTIONS(stdv.begin(), stdv.end(), v.begin(), v.end());
}

BOOST_AUTO_TEST_CASE( cube_move_constructor )
{
  IPosition ip{1, 2, 3};
  Cube<int> a(ip, 42);
  Cube<int> b(std::move(a));
  Cube<int> ref(ip, 42);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());
  Cube<int> c(std::move(a));
  Cube<int> empty;
  BOOST_CHECK_EQUAL_COLLECTIONS(c.begin(), c.end(), empty.begin(), empty.end());
  
  BOOST_CHECK_THROW(Cube<int>(Array<int>(IPosition(5))), std::exception);
  
  Cube<int> d(Vector<int>(3, 17));
  BOOST_CHECK_EQUAL(d.shape(), IPosition(3, 3, 1, 1));
  std::vector<int> dref{17, 17, 17};
  BOOST_CHECK_EQUAL_COLLECTIONS(d.begin(), d.end(), dref.begin(), dref.end());
}

BOOST_AUTO_TEST_CASE( cube_move_assign )
{
  IPosition ip{1, 2, 3};
  Cube<int> a(ip, 42);
  Cube<int> b;
  b = std::move(a);
  Cube<int> ref(ip, 42);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());

  Cube<int> c(IPosition{3,2,1}, 7); // incorrect shape
  BOOST_CHECK_THROW(c = std::move(b), std::runtime_error);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL(b(0,0,0), 42);
  BOOST_CHECK_EQUAL(c.shape(), IPosition(3,3,2,1));
  BOOST_CHECK_EQUAL(c(0,0,0), 7);
 
  Cube<int> d;
  Vector<int> e(IPosition{3}, 1982);
  d = std::move(e);
  BOOST_CHECK_EQUAL(d.shape(), IPosition(3, 3, 1, 1));
  BOOST_CHECK_EQUAL(d(0,0,0), 1982);
}

BOOST_AUTO_TEST_CASE( matrix_move_constructor )
{
  IPosition ip{2, 3};
  Matrix<int> a(ip, 42);
  Matrix<int> b(std::move(a));
  Matrix<int> ref(ip, 42);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());
  Matrix<int> c(std::move(a));
  Matrix<int> empty;
  BOOST_CHECK_EQUAL_COLLECTIONS(c.begin(), c.end(), empty.begin(), empty.end());
  
  BOOST_CHECK_THROW(Matrix<int>(Array<int>(IPosition(3))), std::exception);
  
  Matrix<int> d(Vector<int>(3, 17));
  BOOST_CHECK_EQUAL(d.shape(), IPosition(2, 3, 1));
  std::vector<int> dref{17, 17, 17};
  BOOST_CHECK_EQUAL_COLLECTIONS(d.begin(), d.end(), dref.begin(), dref.end());
}

BOOST_AUTO_TEST_CASE( matrix_move_assign )
{
  IPosition ip{2, 3};
  Matrix<int> a(ip, 42);
  Matrix<int> b;
  b = std::move(a);
  Matrix<int> ref(ip, 42);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());

  Matrix<int> c(IPosition{3,2}, 7); // incorrect shape
  BOOST_CHECK_THROW(c = std::move(b), std::runtime_error);
  BOOST_CHECK_EQUAL(b.shape(), ip);
  BOOST_CHECK_EQUAL(b(0,0), 42);
  BOOST_CHECK_EQUAL(c.shape(), IPosition(2,3,2));
  BOOST_CHECK_EQUAL(c(0,0), 7);
 
  Matrix<int> d;
  Vector<int> e(IPosition{3}, 1982);
  d = std::move(e);
  BOOST_CHECK_EQUAL(d.shape(), IPosition(2, 3, 1));
  BOOST_CHECK_EQUAL(d(0,0), 1982);
}

BOOST_AUTO_TEST_SUITE_END()
