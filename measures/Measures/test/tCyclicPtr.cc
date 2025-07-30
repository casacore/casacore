#include <casacore/measures/Measures/CyclicPtr.h>

#include <boost/test/unit_test.hpp>

using casacore::details::CyclicPtr;
using casacore::details::CyclicState;
using casacore::details::MakeCyclic;

class Counter {
public:
  Counter(int id) : id_(id) {
    ++constructions_;
  }
  ~Counter() {
    ++destructions_;
  }
  int Id() const { return id_; }

  static void Reset() {
    constructions_ = 0;
    destructions_ = 0;
  }

  static void Check(int n_constructions, int n_destructions) {
    BOOST_CHECK_EQUAL(constructions_, n_constructions);
    BOOST_CHECK_EQUAL(destructions_, n_destructions);
  }

  static void CheckNActive(int constructions_minus_destructions) {
    BOOST_CHECK_EQUAL(constructions_minus_destructions, constructions_ - destructions_);
  }

private:
  inline static int constructions_ = 0;
  inline static int destructions_ = 0;
  int id_;
};

struct SingleNode {
  Counter counter{42};
  CyclicPtr<SingleNode> link;
};

struct MultiNode {
  Counter counter{42};
  CyclicPtr<MultiNode> link_a;
  CyclicPtr<MultiNode> link_b;
};

BOOST_AUTO_TEST_SUITE(cyclic_ptr)

BOOST_AUTO_TEST_CASE(empty) {

  BOOST_CHECK(!CyclicPtr<int>());
  BOOST_CHECK(CyclicPtr<int>().Get() == nullptr);

  Counter::Reset();
  {
    CyclicPtr<Counter> empty;
    BOOST_CHECK(!empty);
    empty.Reset();
    BOOST_CHECK(!empty);
    Counter::Check(0, 0); // 'empty' is in scope.
  }
  Counter::Check(0, 0); // 'empty' is out of scope.
}

BOOST_AUTO_TEST_CASE(non_empty) {
  Counter::Reset();
  {
    CyclicPtr<Counter> ptr(new Counter(42));
    BOOST_CHECK(ptr);
    BOOST_CHECK_EQUAL(ptr->Id(), 42);
    Counter::Check(1, 0);
  }
  Counter::Check(1, 1);
}

BOOST_AUTO_TEST_CASE(copy_empty) {
  CyclicPtr<Counter> empty;
  CyclicPtr<Counter> empty_copy(empty);
  BOOST_CHECK(!empty_copy);
  Counter::Check(0, 0);
}

BOOST_AUTO_TEST_CASE(copy_non_empty) {
  Counter::Reset();
  Counter* counter = new Counter(42);
  CyclicPtr<Counter> a(counter);
  CyclicPtr<Counter> b(a);
  Counter::Check(1, 0);
  BOOST_CHECK(a);
  BOOST_CHECK(b);
  BOOST_CHECK(a.Get() == counter);
  BOOST_CHECK(b.Get() == counter);
  BOOST_CHECK_EQUAL(a->Id(), 42);
  BOOST_CHECK_EQUAL(b->Id(), 42);

  a.Reset();
  Counter::Check(1, 0);
  BOOST_CHECK(!a);
  BOOST_CHECK(b);
  BOOST_CHECK_EQUAL(b->Id(), 42);

  b.Reset();
  Counter::Check(1, 1);
  BOOST_CHECK(!b);

  a = b;
  BOOST_CHECK(!a);
  BOOST_CHECK(!b);
  Counter::Check(1, 1);

  b = MakeCyclic<Counter>(1982);
  Counter::CheckNActive(1);
  BOOST_CHECK(b);
  BOOST_CHECK(!a);
  BOOST_CHECK_EQUAL(b->Id(), 1982);

  a = MakeCyclic<Counter>(1981);
  Counter::CheckNActive(2);
  a = b;
  Counter::CheckNActive(1);
  BOOST_CHECK(b);
  BOOST_CHECK(a);
  BOOST_CHECK_EQUAL(a->Id(), 1982);
  BOOST_CHECK_EQUAL(b->Id(), 1982);

  a.Reset();
  Counter::CheckNActive(1);
  BOOST_CHECK(!a);
  BOOST_CHECK(b);
  BOOST_CHECK_EQUAL(b->Id(), 1982);

  b.Reset();
  Counter::CheckNActive(0);
}

BOOST_AUTO_TEST_CASE(move_empty) {
  Counter::Reset();
  CyclicPtr<Counter> empty;
  CyclicPtr<Counter> empty_copy(std::move(empty));
  BOOST_CHECK(!empty_copy);
  Counter::Check(0, 0);
}

BOOST_AUTO_TEST_CASE(move_non_empty) {
  Counter* counter = new Counter(42);
  CyclicPtr<Counter> a(counter);
  CyclicPtr<Counter> b(std::move(a));
  Counter::Check(1, 0);
  BOOST_CHECK(!a);
  BOOST_CHECK(b);
  BOOST_CHECK(a.Get() == nullptr);
  BOOST_CHECK(b.Get() == counter);
  BOOST_CHECK_EQUAL(b->Id(), 42);

  b.Reset();
  Counter::Check(1, 1);
  BOOST_CHECK(!b);

  a = std::move(b);
  BOOST_CHECK(!a);
  BOOST_CHECK(!b);
  Counter::Check(1, 1);

  a = MakeCyclic<Counter>(38);
  b = MakeCyclic<Counter>(37);

  a = std::move(b);
  Counter::CheckNActive(1);
  BOOST_CHECK(!b);
  BOOST_CHECK(a);
  BOOST_CHECK_EQUAL(a->Id(), 37);

  a.Reset();
  Counter::CheckNActive(0);
  BOOST_CHECK(!a);
  BOOST_CHECK(!b);
}

BOOST_AUTO_TEST_CASE(manual_break_cycle) {
  Counter::Reset();
  CyclicPtr<SingleNode> node = MakeCyclic<SingleNode>();
  node->link = node;
  Counter::CheckNActive(1);
  // Just node.Reset(); or letting it go out of scope will now not work, because node->link
  // will continue to hold a CyclicPtr, causing the Node not to be deleted. This behaviour
  // is the same as std::shared_ptr. Manually break the cycle:
  node->link.Reset();
  Counter::CheckNActive(1);
  node.Reset();
  Counter::CheckNActive(0);
}

BOOST_AUTO_TEST_CASE(freeze_empty) {
  Counter::Reset();
  CyclicPtr<Counter> p;
  const CyclicState state = p.Freeze();
  p.Unfreeze(state);
  Counter::Check(0, 0);
}

BOOST_AUTO_TEST_CASE(freeze_without_copies) {
  Counter::Reset();
  CyclicPtr<Counter> p = MakeCyclic<Counter>(37);
  const CyclicState state = p.Freeze();
  p.Unfreeze(state);
  BOOST_CHECK(p);
  BOOST_CHECK_EQUAL(p->Id(), 37);
  Counter::Check(1, 0);
}

BOOST_AUTO_TEST_CASE(freeze_with_cycle) {
  Counter::Reset();
  {
    CyclicPtr<SingleNode> node = MakeCyclic<SingleNode>();
    const CyclicState state = node.Freeze();
    node->link = node;
    node.Unfreeze(state);
    Counter::Check(1, 0);
  }
  Counter::Check(1, 1);
}

BOOST_AUTO_TEST_CASE(multiple_cycles) {
  Counter::Reset();
  {
    CyclicPtr<MultiNode> node1 = MakeCyclic<MultiNode>();
    {
      CyclicPtr<MultiNode> node2 = MakeCyclic<MultiNode>();
      const CyclicState state_1 = node1.Freeze();
      const CyclicState state_2 = node2.Freeze();
      node1->link_a = node2;
      node1->link_b = node1;
      node2->link_a = node1;
      node2->link_b = node2;
      node2.Unfreeze(state_2);
      node1.Unfreeze(state_1);
      Counter::Check(2, 0);
    }
    Counter::Check(2, 2);
    BOOST_CHECK(!node1);
  }
  Counter::Check(2, 2);
}

BOOST_AUTO_TEST_SUITE_END()
