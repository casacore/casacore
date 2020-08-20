#include "../Array.h"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(array_exception_handling)

using namespace casacore;

class ExceptionThrower
{
public:
  explicit ExceptionThrower(bool someValue = false)
  {
    if(someValue || _count == _throwIndex)
      throw std::exception();
    ++_count;
    _maxCount=std::max(_count, _maxCount);
  }
  ExceptionThrower(const ExceptionThrower&)
  {
    if(_count == _throwIndex)
      throw std::exception();
    ++_count;
    _maxCount=std::max(_count, _maxCount);
  }
  ~ExceptionThrower() { --_count; }
  ExceptionThrower& operator=(const ExceptionThrower&) { return *this; }

  static void SetThrowIndex(size_t index) { _throwIndex = index; }
  static size_t Count() { return _count; }
  static size_t MaxCount() { return _maxCount; }
  static void Reset(size_t throwIndex) { _count = 0; _maxCount = 0; _throwIndex=throwIndex; }
  
private:
  static size_t _count, _maxCount, _throwIndex;
};

size_t ExceptionThrower::_count = 0;
size_t ExceptionThrower::_maxCount = 0;
size_t ExceptionThrower::_throwIndex = 0;

BOOST_AUTO_TEST_CASE( default_construct )
{
  ExceptionThrower::Reset(1);
  std::unique_ptr<Array<ExceptionThrower>> a;
  BOOST_CHECK_THROW(
    a.reset(new Array<ExceptionThrower>(IPosition(1, 3))),
    std::exception
  );
  BOOST_CHECK_EQUAL(ExceptionThrower::Count(), 0);
  BOOST_CHECK_EQUAL(ExceptionThrower::MaxCount(), 1);
}

BOOST_AUTO_TEST_CASE( value_construct )
{
  ExceptionThrower::Reset(2);
  std::unique_ptr<Array<ExceptionThrower>> a;
  ExceptionThrower value;
  BOOST_CHECK_THROW(
    a.reset(new Array<ExceptionThrower>(IPosition(1, 3), value)),
    std::exception
  );
  BOOST_CHECK_EQUAL(ExceptionThrower::Count(), 1);
  BOOST_CHECK_EQUAL(ExceptionThrower::MaxCount(), 2);
}

BOOST_AUTO_TEST_CASE( range_construct )
{
  ExceptionThrower::Reset(4);
  std::unique_ptr<Array<ExceptionThrower>> a;
  ExceptionThrower values[3];
  BOOST_CHECK_THROW(
    a.reset(new Array<ExceptionThrower>(IPosition(1, 3), values[0])),
    std::exception
  );
  BOOST_CHECK_EQUAL(ExceptionThrower::Count(), 3);
  BOOST_CHECK_EQUAL(ExceptionThrower::MaxCount(), 4);
}

BOOST_AUTO_TEST_SUITE_END()
