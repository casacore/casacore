#ifndef AOCOMMON_LANE_11_H_
#define AOCOMMON_LANE_11_H_

#include <condition_variable>
#include <cstring>
#include <deque>
#include <mutex>

/**
 * @file
 * Internal header file for the Lane.
 * @headername{Lane.h}
 */

//#define LANE_DEBUG_MODE

#ifdef LANE_DEBUG_MODE
#include <cmath>
#include <iostream>
#include <sstream>
#include <string>
#endif

namespace aocommon {

#ifdef LANE_DEBUG_MODE
#define set_lane_debug_name(lane, str) (lane).setDebugName(str)
#define LANE_REGISTER_DEBUG_INFO registerDebugInfo()
#define LANE_REGISTER_DEBUG_WRITE_WAIT registerDebugWriteWait()
#define LANE_REGISTER_DEBUG_READ_WAIT registerDebugReadWait()
#define LANE_REPORT_DEBUG_INFO reportDebugInfo()

#else

#define set_lane_debug_name(lane, str)
#define LANE_REGISTER_DEBUG_INFO
#define LANE_REGISTER_DEBUG_WRITE_WAIT
#define LANE_REGISTER_DEBUG_READ_WAIT
#define LANE_REPORT_DEBUG_INFO

#endif

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
 * With one reader and one writer, the order is guaranteed to be consistent.
 * With multiple readers or writers in combination with multi-element
 * write or read functions, a sequence of symbols might be interrupted. For
 * example, if a multi-element write() won't fit completely in the buffer,
 * the thread will wait for free space. Another thread might get now write
 * access first, causing the single call to the multi-element write to be
 * "split up".
 *
 * @author Andre Offringa
 * @tparam Tp Type of elements to be stored in the Lane.
 */
template <typename Tp>
class Lane {
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
  Lane() noexcept
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
  explicit Lane(size_t capacity)
      : _buffer(new Tp[capacity]),
        _capacity(capacity),
        _write_position(0),
        _free_write_space(_capacity),
        _status(status_normal) {}

  Lane(const Lane<Tp>& source) = delete;

  /** @brief Move construct a Lane.
   * @details This operation is not thread safe: the behaviour is undefined when
   * other threads access the source Lane.
   * @param source Original Lane to be moved from.
   */
  Lane(Lane<Tp>&& source) noexcept
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
  ~Lane() {
    LANE_REPORT_DEBUG_INFO;
    delete[] _buffer;
  }

  Lane<Tp>& operator=(const Lane<Tp>& source) = delete;

  /** @brief Move assignment.
   * @details This operation is not thread safe: the behaviour is undefined when
   * other threads access the source Lane.
   * @param source Original Lane to be moved from.
   * @returns This Lane.
   */
  Lane<Tp>& operator=(Lane<Tp>&& source) noexcept {
    swap(source);
    return *this;
  }

  /** @brief Swap the contents of this Lane with another.
   * @details This operation is not thread safe: the behaviour is undefined when
   * other threads access either Lane.
   */
  void swap(Lane<Tp>& other) noexcept {
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
    LANE_REGISTER_DEBUG_INFO;

    while (_free_write_space == 0 && _status == status_normal) {
      LANE_REGISTER_DEBUG_WRITE_WAIT;
      _writing_possible_condition.wait(lock);
    }
    if (_status == status_normal) {
      _buffer[_write_position] = element;
      _write_position = (_write_position + 1) % _capacity;
      --_free_write_space;
      // Now that there is less free write space, there is more free read
      // space and thus readers may continue.
      _reading_possible_condition.notify_all();
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
    LANE_REGISTER_DEBUG_INFO;

    while (_free_write_space == 0 && _status == status_normal) {
      LANE_REGISTER_DEBUG_WRITE_WAIT;
      _writing_possible_condition.wait(lock);
    }

    if (_status == status_normal) {
      _buffer[_write_position] = value_type(std::forward<Args>(args)...);
      _write_position = (_write_position + 1) % _capacity;
      --_free_write_space;
      // Now that there is less free write space, there is more free read
      // space and thus readers can possibly continue.
      _reading_possible_condition.notify_all();
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
    LANE_REGISTER_DEBUG_INFO;

    while (_free_write_space == 0 && _status == status_normal) {
      LANE_REGISTER_DEBUG_WRITE_WAIT;
      _writing_possible_condition.wait(lock);
    }
    if (_status == status_normal) {
      _buffer[_write_position] = std::move(element);
      _write_position = (_write_position + 1) % _capacity;
      --_free_write_space;
      // Now that there is less free write space, there is more free read
      // space and thus readers can possibly continue.
      _reading_possible_condition.notify_all();
    }
  }

  void write(const value_type* elements, size_t n) {
    write_generic(elements, n);
  }

  void move_write(value_type* elements, size_t n) {
    write_generic(elements, n);
  }

  bool read(value_type& destination) {
    std::unique_lock<std::mutex> lock(_mutex);
    LANE_REGISTER_DEBUG_INFO;
    while (free_read_space() == 0 && _status == status_normal) {
      LANE_REGISTER_DEBUG_READ_WAIT;
      _reading_possible_condition.wait(lock);
    }
    if (free_read_space() == 0)
      return false;
    else {
      destination = std::move(_buffer[read_position()]);
      ++_free_write_space;
      // Now that there is more free write space, writers can possibly continue.
      _writing_possible_condition.notify_all();
      return true;
    }
  }

  size_t read(value_type* destinations, size_t n) {
    size_t n_left = n;

    std::unique_lock<std::mutex> lock(_mutex);
    LANE_REGISTER_DEBUG_INFO;

    size_t free_space = free_read_space();
    size_t read_size = free_space > n ? n : free_space;
    immediate_read(destinations, read_size);
    n_left -= read_size;

    while (n_left != 0 && _status == status_normal) {
      destinations += read_size;

      do {
        LANE_REGISTER_DEBUG_READ_WAIT;
        _reading_possible_condition.wait(lock);
      } while (free_read_space() == 0 && _status == status_normal);

      free_space = free_read_space();
      read_size = free_space > n_left ? n_left : free_space;
      immediate_read(destinations, read_size);
      n_left -= read_size;
    }
    return n - n_left;
  }

  /**
   * This method does the same thing as read(buffer, n) but discards the data.
   * This eliminates the requirement to specify a buffer if the data is not
   * necessary anyway, and avoids a copy of the data.
   */
  size_t discard(size_t n) {
    size_t n_left = n;

    std::unique_lock<std::mutex> lock(_mutex);
    LANE_REGISTER_DEBUG_INFO;

    size_t free_space = free_read_space();
    size_t read_size = free_space > n ? n : free_space;
    immediate_discard(read_size);
    n_left -= read_size;

    while (n_left != 0 && _status == status_normal) {
      do {
        LANE_REGISTER_DEBUG_READ_WAIT;
        _reading_possible_condition.wait(lock);
      } while (free_read_space() == 0 && _status == status_normal);

      free_space = free_read_space();
      read_size = free_space > n_left ? n_left : free_space;
      immediate_discard(read_size);
      n_left -= read_size;
    }
    return n - n_left;
  }

  void write_end() {
    std::lock_guard<std::mutex> lock(_mutex);
    LANE_REGISTER_DEBUG_INFO;
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
   * True when write_end() was called. Even when end, the lane may still
   * contain items.
   */
  bool is_end() const {
    std::lock_guard<std::mutex> lock(_mutex);
    return _status == status_end;
  }

  /**
   * True when write_end() and the lane does not contain items.
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

  /**
   * Wait until this Lane is empty.
   */
  void wait_for_empty() {
    std::unique_lock<std::mutex> lock(_mutex);
    while (_capacity != _free_write_space) {
      _writing_possible_condition.wait(lock);
    }
  }

#ifdef LANE_DEBUG_MODE
  /**
   * Change the name of this Lane to make it appear in the output along
   * with statistics. Do not use this function directly; use the
   * set_lane_debug_name() macro instead.
   * @param nameStr New debug description of this Lane.
   */
  void setDebugName(const std::string& nameStr) { _debugName = nameStr; }
#endif
 private:
  Tp* _buffer;

  size_t _capacity;

  size_t _write_position;

  size_t _free_write_space;

  enum { status_normal, status_end } _status;

  mutable std::mutex _mutex;

  std::condition_variable _writing_possible_condition,
      _reading_possible_condition;

  size_t read_position() const noexcept {
    return (_write_position + _free_write_space) % _capacity;
  }

  size_t free_read_space() const noexcept {
    return _capacity - _free_write_space;
  }

  // This is a template to allow const and non-const (to be able to move)
  template <typename T>
  void write_generic(T* elements, size_t n) {
    std::unique_lock<std::mutex> lock(_mutex);
    LANE_REGISTER_DEBUG_INFO;

    if (_status == status_normal) {
      size_t write_size = _free_write_space > n ? n : _free_write_space;
      immediate_write(elements, write_size);
      n -= write_size;

      while (n != 0 && _status == status_normal) {
        elements += write_size;

        do {
          LANE_REGISTER_DEBUG_WRITE_WAIT;
          _writing_possible_condition.wait(lock);
        } while (_free_write_space == 0 && _status == status_normal);

        write_size = _free_write_space > n ? n : _free_write_space;
        immediate_write(elements, write_size);
        n -= write_size;
      }
    }
  }

  // This is a template to allow const and non-const (to be able to move)
  template <typename T>
  void immediate_write(T* elements, size_t n) noexcept {
    // Split the writing in two ranges if needed. The first range fits in
    // [_write_position, _capacity), the second range in [0, end). By doing
    // so, we only have to calculate the modulo in the write position once.
    if (n > 0) {
      size_t nPart;
      if (_write_position + n > _capacity) {
        nPart = _capacity - _write_position;
      } else {
        nPart = n;
      }
      for (size_t i = 0; i < nPart; ++i, ++_write_position) {
        _buffer[_write_position] = std::move(elements[i]);
      }

      _write_position = _write_position % _capacity;

      for (size_t i = nPart; i < n; ++i, ++_write_position) {
        _buffer[_write_position] = std::move(elements[i]);
      }

      _free_write_space -= n;

      // Now that there is less free write space, there is more free read
      // space and thus readers may continue.
      _reading_possible_condition.notify_all();
    }
  }

  void immediate_read(value_type* elements, size_t n) noexcept {
    // As with write, split in two ranges if needed. The first range fits in
    // [read_position(), _capacity), the second range in [0, end).
    if (n > 0) {
      size_t nPart;
      size_t position = read_position();
      if (position + n > _capacity) {
        nPart = _capacity - position;
      } else {
        nPart = n;
      }
      for (size_t i = 0; i < nPart; ++i, ++position) {
        elements[i] = std::move(_buffer[position]);
      }

      position = position % _capacity;

      for (size_t i = nPart; i < n; ++i, ++position) {
        elements[i] = std::move(_buffer[position]);
      }

      _free_write_space += n;

      // Now that there is more free write space, writers can possibly continue.
      _writing_possible_condition.notify_all();
    }
  }

  void immediate_discard(size_t n) noexcept {
    if (n > 0) {
      _free_write_space += n;

      // Now that there is more free write space, writers can possibly continue.
      _writing_possible_condition.notify_all();
    }
  }

#ifdef LANE_DEBUG_MODE
  void registerDebugInfo() noexcept {
    _debugSummedSize += _capacity - _free_write_space;
    _debugMeasureCount++;
  }
  void registerDebugReadWait() noexcept { ++_debugReadWaitCount; }
  void registerDebugWriteWait() noexcept { ++_debugWriteWaitCount; }
  void reportDebugInfo() {
    if (!_debugName.empty()) {
      std::stringstream str;
      str << "*** Debug report for the following Lane: ***\n"
          << "\"" << _debugName << "\"\n"
          << "Capacity: " << _capacity << '\n'
          << "Total read/write ops: " << _debugMeasureCount << '\n'
          << "Average size of buffer, measured per read/write op.: "
          << round(double(_debugSummedSize) * 100.0 / _debugMeasureCount) /
                 100.0
          << '\n'
          << "Number of wait events during reading: " << _debugReadWaitCount
          << '\n'
          << "Number of wait events during writing: " << _debugWriteWaitCount
          << '\n';
      std::cout << str.str();
    }
  }
  std::string _debugName;
  size_t _debugSummedSize = 0, _debugMeasureCount = 0, _debugReadWaitCount = 0,
         _debugWriteWaitCount = 0;
#endif
};

template <typename Tp>
void swap(aocommon::Lane<Tp>& first, aocommon::Lane<Tp>& second) noexcept {
  first.swap(second);
}

}  // namespace aocommon

#endif  // AO_LANE11_H
