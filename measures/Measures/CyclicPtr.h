#ifndef CYCLIC_PTR_H_
#define CYCLIC_PTR_H_

#include <atomic>
#include <memory>

namespace casacore::details {

template<typename T>
class CyclicPtr;

/**
 * Class to hold the state returned by CyclicPtr::Freeze().
 */
class CyclicState {
public:
  ~CyclicState() noexcept = default;

private:
  CyclicState() = delete;
  constexpr CyclicState(unsigned value) noexcept : value_(value)
  {}
  template<typename T>
  friend class CyclicPtr;

  unsigned value_;
};

/**
 * A smart pointer class that allows to work with complex cycles without
 * using weak_ptrs. Instead, the pointer needs to be manually frozen
 * during the creation of cycles, such that cycle-links are not counted.
 *
 * This class was specifically written for the MeasFrame class, which can
 * have cycles via various routes. Originally, the FrameRep class held a
 * counter and was destructed when the counter reached zero. While this
 * worked, it had issues:
 *
 * - It was hard to understand the implementation. It looked like a
 *   "standard" manual reference-counting implementation and it therefore
 *   seemed like it could be replaced by a shared_ptr. This class helps
 *   to annotate the intention.
 * - It had bugs: it would sometimes access the counter after destruction
 *   of the MeasFrame (see #1082). By holding the counter inside the pointer
 *   class, this is avoided.
 * - The counter was not implemented in a safe manner, hence copying the
 *   same frame to different threads would cause a race condition even
 *   when the frame was only read from (/copied).
 *
 * By having a separate class that also holds the counter, these problems
 * are avoided. Introducing weak_ptrs to break the cycles in the measures
 * code has been tried, but probably requires major changes in the
 * structure.
 *
 * This is an example of how to use the class:
 *
 *     struct Node { CyclicPtr<Node> link; };
 *
 *     CyclicPtr<Node> node = MakeCyclic<Node>();
 *     const CyclicState state = node.Freeze();
 *     node->link = node;
 *     node.Unfreeze(state);
 *
 * By freezing the state when creating the link, the link is not counted as
 * a reference. As soon as either node->link or node is reset or goes out of
 * scope, the Node object is destroyed and the other CyclicPtr will become
 * nullptr.
 *
 * This class implements the counter in a thread-safe way, such that the
 * pointer can be copied around in different threads as if it is a normal
 * pointer. @ref Freeze() and @ref Unfreeze() are not thread safe. In the
 * MeasFrame class, these functions are only used during initialization or
 * write-to actions of the MeasFrame, which remains valid.
 *
 * It should be clear that this is a dangerous class to use correctly.
 */
template<typename T>
class CyclicPtr {
public:

  CyclicPtr() = default;

  CyclicPtr(std::nullptr_t) {}

  explicit CyclicPtr(T* object) : data_(std::make_shared<Data>(object))
  {}

  CyclicPtr(const CyclicPtr& source) : data_(source.data_)
  {
    Increase();
  }

  CyclicPtr(CyclicPtr&& source) : data_(std::move(source.data_))
  {}

  ~CyclicPtr() {
    Decrease();
  }

  T* Get() const {
    return (data_ && data_->object_) ? data_->object_ : nullptr;
  }

  void Reset() {
    Decrease();
    data_.reset();
  }

  CyclicPtr& operator=(const CyclicPtr& other) {
    Decrease();
    data_ = other.data_;
    Increase();
    return *this;
  }

  CyclicPtr& operator=(CyclicPtr&& other) {
    Decrease();
    data_ = std::move(other.data_);
    return *this;
  }

  explicit operator bool() const { return data_ && data_->object_; }

  T& operator*() const { return *data_->object_; }
  T* operator->() const { return data_->object_; }

  friend bool operator==(const CyclicPtr<T>& lhs, const CyclicPtr<T>& rhs) {
    return lhs.data_ == rhs.data_;
  }

  friend bool operator!=(const CyclicPtr<T>& lhs, const CyclicPtr<T>& rhs) {
    return lhs.data_ != rhs.data_;
  }

  /**
   * Freeze the reference counter until Unfreeze() is called. The object is
   * guaranteed not to be destructed during a Freeze().
   *
   * The caller is responsible for calling Unfreeze(); letting CyclicState go
   * out of scope without unfreezing will not cause an error, so will lead to
   * an undetected state error. While it would be possible to detect this, it
   * would increase the size of CyclicState.
   */
  [[nodiscard]] CyclicState Freeze() const {
    return CyclicState(data_ ? data_->counter_.fetch_add(1) : 0);
  }

  /**
   * Continue 'normal' reference counting.
   */
  void Unfreeze(const CyclicState& frozen_state) {
    if(data_)
      data_->counter_.store(frozen_state.value_);
  }

private:
  struct Data {
    Data(T* object) : object_(object)
    {}

    T* object_ = nullptr;
    std::atomic<unsigned> counter_ = 1;
  };

  void Increase() {
    if(data_ && data_->object_) {
      ++(data_->counter_);
    }
  }

  void Decrease() {
    if(data_ && data_->object_) {
      const unsigned previous_value = data_->counter_.fetch_sub(1);
      if(previous_value == 1) {
        delete data_->object_;
        data_->object_ = nullptr;
      }
    }
  }

  std::shared_ptr<Data> data_;
};

template<typename T, typename... Args>
CyclicPtr<T> MakeCyclic(Args&&... args) {
  return CyclicPtr<T>(new T(std::forward<Args>(args)...));
}

} // namespace casacore::details

#endif
