#ifndef DYSCO_THREADGROUP_H
#define DYSCO_THREADGROUP_H

#include <stdexcept>
#include <thread>
#include <vector>

namespace dyscostman {

/**
 * Group of threads.
 */
class threadgroup {
 public:
  /** Constructor */
  threadgroup() {}
  /** Destructor. Will join all threads that have not been joined yet. */
  ~threadgroup() { join_all(); }

  /**
   * Create a new thread that will execute the given functor. The new thread
   * will be added to the group.
   * @param threadFunc The functor to be called.
   */
  template <typename T>
  void create_thread(T threadFunc) {
    _threads.emplace_back(threadFunc);
  }

  /**
   * Join all threads in the group that have not yet been joined.
   */
  void join_all() {
    for (std::thread &t : _threads) {
      t.join();
    }
    _threads.clear();
  }

  /**
   * Get state of thread group.
   * @returns true when there are unjoined threads in the group. Not
   * synchronized -- caller has to make sure that thread is safe.
   */
  bool empty() const { return _threads.empty(); }

 private:
  std::vector<std::thread> _threads;
};

}  // namespace dyscostman

#endif
