#ifndef CASACORE_AOCOMMON_ONE_BY_ONE_LANE_11_H_
#define CASACORE_AOCOMMON_ONE_BY_ONE_LANE_11_H_

#include <condition_variable>
#include <cstddef>
#include <deque>
#include <mutex>
#include <utility>

/**
 * @file
 * Internal header file for the OneByOneLane.
 * @headername{Lane.h}
 *
 * Original location of this file:
 * https://gitlab.com/aroffringa/aocommon/-/blob/master/include/aocommon/lane.h
 */

namespace casacore::aocommon {

/**
 * @brief The Lane is an efficient cyclic buffer that is synchronized.
 * @details
 * A Lane can typically be used in a multi-threaded producer-consumer
 * situation. The Lane also holds a state which allows for
 * an ellegant way of communicating from producer(s) to
 * consumer(s) that all data has been produced.
 *
 * A simple example:
 * @code
 * void producer(Lane<Task>* taskLane)
 * {
 *   while(moreTasks)
 *     taskLane->write(nextTask());
 *   taskLane->write_end();
 * }
 *
 * void consumer(Lane<Task>* taskLane)
 * {
 *   Task task;
 *   while(taskLane->read(task))
 *     processTask(task);
 * }
 *
 * void run()
 * {
 *   Lane<Task> taskLane;
 *   std::thread consumerThread(&consumer(), &taskLane);
 *   producer(&taskLane);
 *   consumerThread.join();
 * }
 * @endcode
 *
 * The various read and write methods, as well as the empty(),
 * capacity() and size() methods are always thread safe. The other
 * methods are not: assignment, swap(), clear() and resize() can not
 * be called from a different thread while another thread is also
 * accessing the Lane. The same holds obviously for the constructors
 * and destructor. This is chosen because these methods should almost never
 * be called in parallel with other methods, and hence it is not worth
 * to increase every call with extra locks to make this possible.
 *
 * This class is a simplification of the Lane class, which allows
 * multi-item reads and writes. By allowing only one item at a time,
 * it is more efficiency (notify_one instead of all). This one-by-one
 * behaviour is quite common, hence a special class for this.
 *
 * @author Andre Offringa
 * @tparam Tp Type of elements to be stored in the Lane.
 */
template <typename Tp>
class OneByOneLane {
 public:
  /** @brief Integer type used to store size types. */
  typedef std::size_t size_type;

  /** @brief Type of elements stored in the Lane. */
  typedef Tp value_type;

  /** @brief Construct a Lane with zero elements.
   * @details A Lane with zero elements can not be written to or read to
   * (both operations will wait forever).
   *
   * This constructor makes it easy to construct e.g. a container
   * of Lanes. After the container is created, the Lanes can be
   * resized with @ref resize().
   */
  OneByOneLane() noexcept
      : _buffer(nullptr),
        _capacity(0),
        _write_position(0),
        _free_write_space(0),
        _status(status_normal) {}

  /** @brief Construct a Lane with the given capacity.
   * @details After construction, the Lane is ready for writing to and reading
   * from.
   * @param capacity Number of elements that the Lane can hold at once.
   */
  explicit OneByOneLane(size_t capacity)
      : _buffer(new Tp[capacity]),
        _capacity(capacity),
        _write_position(0),
        _free_write_space(_capacity),
        _status(status_normal) {}

  OneByOneLane(const OneByOneLane<Tp>& source) = delete;

  /** @brief Move construct a Lane.
   * @details This operation is not thread safe: the behaviour is undefined when
   * other threads access the source Lane.
   * @param source Original Lane to be moved from.
   */
  OneByOneLane(OneByOneLane<Tp>&& source) noexcept
      : _buffer(nullptr),
        _capacity(0),
        _write_position(0),
        _free_write_space(0),
        _status(status_normal) {
    swap(source);
  }

  /** @brief Destructor.
   * @details The destructor is not synchronized.
   */
  ~OneByOneLane() { delete[] _buffer; }

  OneByOneLane<Tp>& operator=(const OneByOneLane<Tp>& source) = delete;

  /** @brief Move assignment.
   * @details This operation is not thread safe: the behaviour is undefined when
   * other threads access the source Lane.
   * @param source Original Lane to be moved from.
   * @returns This Lane.
   */
  OneByOneLane<Tp>& operator=(OneByOneLane<Tp>&& source) noexcept {
    swap(source);
    return *this;
  }

  /** @brief Swap the contents of this Lane with another.
   * @details This operation is not thread safe: the behaviour is undefined when
   * other threads access either Lane.
   */
  void swap(OneByOneLane<Tp>& other) noexcept {
    std::swap(_buffer, other._buffer);
    std::swap(_capacity, other._capacity);
    std::swap(_write_position, other._write_position);
    std::swap(_free_write_space, other._free_write_space);
    std::swap(_status, other._status);
  }

  /** @brief Clear the contents and reset the state of the Lane.
   * @details After calling clear(), the Lane is in the same state as after
   * construction. This also means that after clearing the Lane, it
   * is as if write_end() has not been called yet.
   *
   * This method is not thread safe.
   */
  void clear() noexcept {
    _write_position = 0;
    _free_write_space = _capacity;
    _status = status_normal;
  }

  /** @brief Write a single element.
   * @details This method is thread safe, and can be called together with
   * other write and read methods from different threads.
   *
   * If this call comes after a call to write_end(), the call
   * will be ignored. If a write_end() call comes during write(), the
   * write() call is aborted and returns immediately.
   * @param element Object to be copied into the cyclic buffer.
   */
  void write(const value_type& element) {
    std::unique_lock<std::mutex> lock(_mutex);

    while (_free_write_space == 0 && _status == status_normal) {
      _writing_possible_condition.wait(lock);
    }
    if (_status == status_normal) {
      _buffer[_write_position] = element;
      _write_position = (_write_position + 1) % _capacity;
      --_free_write_space;
      // Now that there is less free write space, there is more free read
      // space and thus a reader may continue.
      _reading_possible_condition.notify_one();
    }
  }

  /** @brief Write a single element by constructing it.
   * @details This method is thread safe, and can be called together with
   * other write and read methods from different threads.
   *
   * If this call comes after a call to write_end(), the call
   * will be ignored. The implementation does not construct the value
   * in place, but rather constructs the value and then move assigns it.
   * This is because the value that it is moved into has already been
   * constructed (in the current implementation).
   * @param element Object to be moved into the cyclic buffer.
   */
  template <typename... Args>
  void emplace(Args&&... args) {
    std::unique_lock<std::mutex> lock(_mutex);

    while (_free_write_space == 0 && _status == status_normal) {
      _writing_possible_condition.wait(lock);
    }

    if (_status == status_normal) {
      _buffer[_write_position] = value_type(std::forward<Args>(args)...);
      _write_position = (_write_position + 1) % _capacity;
      --_free_write_space;
      // Now that there is less free write space, there is more free read
      // space and thus a reader can possibly continue.
      _reading_possible_condition.notify_one();
    }
  }

  /** @brief Write a single element by moving it in.
   * @details This method is thread safe, and can be called together with
   * other write and read methods from different threads.
   *
   * If this call comes after a call to write_end(), the call
   * will be ignored.
   * @param element Object to be moved into the cyclic buffer.
   */
  void write(value_type&& element) {
    std::unique_lock<std::mutex> lock(_mutex);

    while (_free_write_space == 0 && _status == status_normal) {
      _writing_possible_condition.wait(lock);
    }
    if (_status == status_normal) {
      _buffer[_write_position] = std::move(element);
      _write_position = (_write_position + 1) % _capacity;
      --_free_write_space;
      // Now that there is less free write space, there is more free read
      // space and thus a reader can possibly continue.
      _reading_possible_condition.notify_one();
    }
  }

  bool read(value_type& destination) {
    std::unique_lock<std::mutex> lock(_mutex);
    while (free_read_space() == 0 && _status == status_normal) {
      _reading_possible_condition.wait(lock);
    }
    if (free_read_space() == 0)
      return false;
    else {
      destination = std::move(_buffer[read_position()]);
      ++_free_write_space;
      // Now that there is more free write space, writers can possibly continue.
      _writing_possible_condition.notify_one();
      return true;
    }
  }

  void write_end() {
    std::lock_guard<std::mutex> lock(_mutex);
    _status = status_end;
    _writing_possible_condition.notify_all();
    _reading_possible_condition.notify_all();
  }

  size_t capacity() const noexcept { return _capacity; }

  size_t size() const {
    std::lock_guard<std::mutex> lock(_mutex);
    return _capacity - _free_write_space;
  }

  bool empty() const {
    std::lock_guard<std::mutex> lock(_mutex);
    return _capacity == _free_write_space;
  }

  /**
   * true when write_end() was called. Even when end, the lane may still
   * contain items.
   */
  bool is_end() const {
    std::lock_guard<std::mutex> lock(_mutex);
    return _status == status_end;
  }

  /**
   * true when write_end() and the lane does not contain items.
   */
  bool is_end_and_empty() const {
    std::lock_guard<std::mutex> lock(_mutex);
    return _status == status_end && _capacity == _free_write_space;
  }

  /**
   * Change the capacity of the Lane. This will erase all data in the Lane,
   * and clear the state.
   */
  void resize(size_t new_capacity) {
    Tp* new_buffer = new Tp[new_capacity];
    delete[] _buffer;
    _buffer = new_buffer;
    _capacity = new_capacity;
    _write_position = 0;
    _free_write_space = new_capacity;
    _status = status_normal;
  }

 private:
  Tp* _buffer;

  size_t _capacity;

  size_t _write_position;

  size_t _free_write_space;

  enum { status_normal, status_end } _status;

  mutable std::mutex _mutex;

  std::condition_variable _writing_possible_condition, _reading_possible_condition;

  size_t read_position() const noexcept {
    return (_write_position + _free_write_space) % _capacity;
  }

  size_t free_read_space() const noexcept { return _capacity - _free_write_space; }
};

template <typename Tp>
void swap(aocommon::OneByOneLane<Tp>& first, aocommon::OneByOneLane<Tp>& second) noexcept {
  first.swap(second);
}

}  // namespace casacore::aocommon

#endif  // AO_LANE11_H
