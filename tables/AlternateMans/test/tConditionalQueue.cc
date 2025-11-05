#include "../DataManagers/ConditionalQueue.h"

#include <boost/test/unit_test.hpp>

#include <cmath>
#include <complex>

namespace casacore::sisco {

BOOST_AUTO_TEST_SUITE(conditional_queue)

BOOST_AUTO_TEST_CASE(single_threaded) {
  ConditionalQueue<int> queue(10);
  BOOST_CHECK_EQUAL(queue.MaxSize(), 10);

  std::mutex mutex;
  std::unique_lock lock(mutex);
  BOOST_CHECK_EQUAL(queue.Size(lock), 0);
  for(size_t i=0; i!=10; ++i)
    queue.Push(i, lock);
  BOOST_CHECK_EQUAL(queue.Size(lock), 10);
  queue.Finish(lock);
  int result = 1000;

  BOOST_CHECK(queue.PopIf(result, lock, [](int){ return true; }));
  BOOST_CHECK_EQUAL(result, 0);
  BOOST_CHECK_EQUAL(queue.Size(lock), 9);

  queue.NotifyOneChange(); // shouldn't do anything

  for(size_t i=0; i!=5; ++i) {
    // Pop odd values
    BOOST_CHECK(queue.PopIf(result, lock, [](int value){ return value%2 == 1; }));
    BOOST_CHECK_EQUAL(result, i*2+1);
  }
  for(size_t i=0; i!=4; ++i) {
    const int pop_value = (4-i) * 2;
    BOOST_CHECK(queue.PopIf(result, lock, [pop_value](int value){ return value==pop_value; }));
    BOOST_CHECK_EQUAL(result, pop_value);
  }
  result = 1000;
  // Queue is now empty, but is already ended so should return immediately:
  BOOST_CHECK(!queue.PopIf(result, lock, [](int){ return true; }));
  BOOST_CHECK_EQUAL(result, 1000);
  // ...and without checking the condition...
  BOOST_CHECK(!queue.PopIf(result, lock, [](int){ return false; }));
  BOOST_CHECK_EQUAL(result, 1000);
}

BOOST_AUTO_TEST_CASE(multi_threaded) {
  ConditionalQueue<int> queue(3);
  size_t even_count = 0;
  size_t odd_count = 0;
  std::mutex mutex;
  std::thread a([&](){
    std::unique_lock lock(mutex);
    int value;
    while(queue.PopIf(value, lock, [](int value) { return value%2 == 0;})) {
      BOOST_CHECK(value%2 == 0);
      ++even_count;
    }
  });
  std::thread b([&](){
    std::unique_lock lock(mutex);
    int value;
    while(queue.PopIf(value, lock, [](int value) { return value%2 == 1;})) {
      BOOST_CHECK(value%2 == 1);
      ++odd_count;
    }
  });

  for(int i=0; i!=1000; ++i) {
    std::unique_lock lock(mutex);
    queue.Push(std::move(i), lock);
  }
  {
    std::unique_lock lock(mutex);
    queue.Finish(lock);
  }
  a.join();
  b.join();
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore
