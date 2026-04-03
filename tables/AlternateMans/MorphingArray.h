#ifndef CASACORE_MORPHING_ARRAY_H_
#define CASACORE_MORPHING_ARRAY_H_

#include <cstdlib>
#include <memory>
#include <new>
#include <type_traits>

/**
 * Array class for which the type is determined only at runtime. It handles
 * run-time allocation and alignment of the type. It simplifies buffer allocation
 * in the StokesIStManColumn class.
 *
 * Once the array is initialized using Resize(), the user must make sure its type
 * no longer changes.
 */
class MorphingArray {
 public:
  MorphingArray() noexcept = default;
  
  template<typename T>
  MorphingArray() {
    
  }

  ~MorphingArray() noexcept { std::free(data_); }

  MorphingArray(const MorphingArray&) = delete;
  MorphingArray& operator=(const MorphingArray&) = delete;

  MorphingArray(MorphingArray&& other) noexcept : data_(other.data_), size_(other.size_) {
    other.data_ = nullptr;
    other.size_ = 0;
  }
  MorphingArray& operator=(MorphingArray&& other) noexcept {
    std::free(data_);
    data_ = other.data_;
    size_ = other.size_;
    other.data_ = nullptr;
    other.size_ = 0;
    return *this;
  }

  /**
   * Make sure that the array can hold as least the specified number of
   * elements of type @p T.
   */
  template <typename T>
  void Resize(size_t new_size) {
    static_assert(std::is_trivially_destructible_v<T>);
    if (new_size > size_) {
      free(data_);
      const size_t n_bytes = ((new_size * sizeof(T) + alignof(T) - 1) / alignof(T)) * alignof(T);
      data_ = std::aligned_alloc(alignof(T), n_bytes);
      if (!data_) throw std::bad_alloc();
      std::uninitialized_default_construct_n(reinterpret_cast<T*>(data_), new_size);
      size_ = new_size;
    }
  }

  template <typename T>
  T* Data() {
    static_assert(std::is_trivially_destructible_v<T>);
    return reinterpret_cast<T*>(data_);
  }

  template <typename T>
  const T* Data() const {
    static_assert(std::is_trivially_destructible_v<T>);
    return reinterpret_cast<T*>(data_);
  }

  size_t Size() const { return size_; }

 private:
  void* data_ = nullptr;
  size_t size_ = 0;
};

#endif
