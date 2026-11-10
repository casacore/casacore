#ifndef SISCO_CONDITIONAL_QUEUE_H_
#define SISCO_CONDITIONAL_QUEUE_H_

#include <cassert>
#include <condition_variable>
#include <list>

namespace casacore::sisco {

/**
 * A queue with a limited size and the ability to query
 * only specific values. The queue is synchronized, but
 * the caller needs to maintain a mutex.
 *
 * The reason for this is that if the condition requires
 * locking, the same lock can be used for the queue operations
 * and the condition.
 *
 * @param SameCondition If all threads call PopIf() with the same
 * condition, this value can be set to @c true to increase the
 * performance of the queue. When true, any waiting thread can take
 * a new value, so only one thread needs to be woken up per value.
 */
template <typename T, bool SameCondition = false>
class ConditionalQueue {
 public:
  ConditionalQueue(size_t max_size) : max_size_(max_size) {}

  ~ConditionalQueue() = default;

  /**
   * Place a new object at the end of the queue. The lock
   * must be owned on entry and will be locked on exit, but
   * may be temporary unlocked during the function.
   *
   * If the queue is full, the method will block until space
   * is available.
   */
  void Push(T&& value, std::unique_lock<std::mutex>& lock) {
    assert(lock.owns_lock());
    while (values_.size() == max_size_) {
      pop_condition_.wait(lock);
    }
    values_.emplace_back(std::move(value));
    NotifyPush();
  }

  /**
   * Same as @ref Push(), but constructs in place.
   */
  template <typename... Args>
  void Emplace(std::unique_lock<std::mutex>& lock, Args&&... args) {
    assert(lock.owns_lock());
    while (values_.size() == max_size_) {
      pop_condition_.wait(lock);
    }
    values_.emplace_back(std::forward<Args>(args)...);
    NotifyPush();
  }

  /**
   * Pop an object from the queue for which a specified condition holds.
   * The lock must be owned on entry and will be locked on exit,
   * but may be temporary unlocked during the function.
   *
   * The condition must be a callable function that takes a value as
   * function parameter and returns a bool. The lock will always be
   * owned when the condition function is called.
   */
  template <typename Condition>
  bool PopIf(T& result, std::unique_lock<std::mutex>& lock, Condition condition) {
    assert(lock.owns_lock());

    typename std::list<T>::iterator iterator = GetNext(condition);
    while (iterator == values_.end()) {
      if (values_.empty() && is_finished_) return false;
      push_condition_.wait(lock);
      iterator = GetNext(condition);
    }

    result = std::move(*iterator);
    values_.erase(iterator);
    pop_condition_.notify_one();

    // If this thread emptied the finished queue while another thread is waiting
    // for a value, it needs to be notified to finish.
    if (values_.empty() && is_finished_) {
      push_condition_.notify_all();
    }

    return true;
  }

  /**
   * Notify the queue that the condition for values might have changed.
   *
   * When SameCondition=true, this will wake up only one waiting thread, and
   * so: i) the change must affect all threads equally; and ii) the change
   * may cause at most one queued item to become available.
   */
  void NotifyChange() { NotifyPush(); }

  /**
   * Notify the queue that the condition for multiple values might have changed.
   *
   * This will unconditionally wake up all threads. This implies that, unlike
   * @ref NotifyChange(), the change may affect different threads differently,
   * and the change may affect multiple queued items, even with SameCondition=true.
   *
   * Because of this, this method is slower compared to @ref NotifyChange().
   */
  void NotifyMultipleChanges() { push_condition_.notify_all(); }

  /**
   * Makes PopIf return false once the queue is empty.
   */
  void Finish([[maybe_unused]] std::unique_lock<std::mutex>& lock) {
    assert(lock.owns_lock());
    is_finished_ = true;
    push_condition_.notify_all();
  }

  size_t Size([[maybe_unused]] std::unique_lock<std::mutex>& lock) const {
    assert(lock.owns_lock());
    return values_.size();
  }

  size_t MaxSize() const { return max_size_; }

 private:
  template <typename Condition>
  std::list<T>::iterator GetNext(Condition&& condition) {
    for (typename std::list<T>::iterator i = values_.begin(); i != values_.end(); ++i) {
      if (condition(*i)) return i;
    }
    return values_.end();
  }

  void NotifyPush() {
    if constexpr (SameCondition) {
      push_condition_.notify_one();
    } else {
      // We need to notify all, because if only one thread is awoken that
      // is looking for a specific condition that is not met, the signal
      // is lost while another nother thread's condition may be met.
      push_condition_.notify_all();
    }
  }

  std::list<T> values_;
  bool is_finished_ = false;
  size_t max_size_;
  std::condition_variable pop_condition_;
  std::condition_variable push_condition_;
};

}  // namespace casacore::sisco

#endif
